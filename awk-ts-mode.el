;;; awk-ts-mode.el --- Tree-sitter mode for GNU and POSIX awk  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/awk-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for GNU and POSIX awk.

;;; Code:

(require 'elec-pair)
(require 'editorconfig)
(require 'treesit)

(defgroup awk-ts nil
  "Tree-sitter mode for GNU and POSIX awk."
  :group 'languages)

;;;; Grammar

(defconst awk-ts-mode--grammar-sources
  '((gawk "https://github.com/konomanoasa/tree-sitter-gawk"
          :revision "v0.2.1")
    (posix-awk "https://github.com/konomanoasa/tree-sitter-posix-awk"
               :revision "v0.20.1"))
  "Tree-sitter grammar sources for GNU and POSIX awk.")

(defun awk-ts-mode--language ()
  "Return the language selected by the EditorConfig posix property."
  (let* ((properties (when buffer-file-name
                       (editorconfig-call-get-properties-function buffer-file-name)))
         (value (and properties (gethash 'posix properties))))
    (pcase value
      ((or 'nil "false") 'gawk)
      ("true" 'posix-awk)
      (_ (user-error "Invalid EditorConfig posix: %S (expected true or false)" value)))))

(defun awk-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language awk-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

;;;; Incremental Updates

(defconst awk-ts-mode-update--containers
  '("item_list")
  "Container nodes traversed to find an edited source unit.")

(defvar-local awk-ts-mode-update--state nil
  "Bounds and recovery state captured before a buffer edit.")

(defvar-local awk-ts-mode-update--pending-range nil
  "Edited owner range awaiting syntax propertization.")

(defun awk-ts-mode-update--bounds (beg end)
  "Return enclosing source-unit bounds around BEG and END."
  (let ((start beg)
        (finish end)
        (root (treesit-parser-root-node treesit-primary-parser)))
    (dolist (position (list (max (point-min) (1- beg))
                            (min (point-max) end)))
      (let ((node (treesit-node-at position treesit-primary-parser))
            owner)
        (while (and node (not (treesit-node-eq node root)))
          (unless (member (treesit-node-type node)
                          awk-ts-mode-update--containers)
            (setq owner node))
          (setq node (treesit-node-parent node)))
        (when owner
          (setq start (min start (treesit-node-start owner))
                finish (max finish (treesit-node-end owner))))))
    (cons start finish)))

(defun awk-ts-mode-update--before-change (beg end)
  "Save owner bounds and old-tree recovery state before changing BEG to END."
  (when treesit-primary-parser
    (save-restriction
      (widen)
      (setq awk-ts-mode-update--state
            (cons (awk-ts-mode-update--bounds beg end)
                  (treesit-node-check
                   (treesit-parser-root-node treesit-primary-parser)
                   'has-error))))))

(defun awk-ts-mode-update--reset-parser ()
  "Replace the primary parser without reusing its old tree."
  (let* ((parser treesit-primary-parser)
         (language (treesit-parser-language parser))
         (tag (treesit-parser-tag parser))
         (ranges (treesit-parser-included-ranges parser))
         (notifiers (treesit-parser-notifiers parser))
         (level (treesit-parser-embed-level parser)))
    (treesit-parser-delete parser)
    (setq treesit-primary-parser (treesit-parser-create language nil nil tag))
    (treesit-parser-set-included-ranges treesit-primary-parser ranges)
    (treesit-parser-set-embed-level treesit-primary-parser level)
    (dolist (notifier notifiers)
      (treesit-parser-add-notifier treesit-primary-parser notifier))))

(defun awk-ts-mode-update--after-change (beg end old-length)
  "Refresh edited owners and avoid stale trees after BEG, END and OLD-LENGTH."
  (when treesit-primary-parser
    (let ((narrowed (buffer-narrowed-p)))
      (save-restriction
        (widen)
        (let* ((state awk-ts-mode-update--state)
               (reset (or (cdr state)
                          (and (= beg 1) (> old-length 0)
                               (eq (char-after 1) #xfeff)))))
          (setq awk-ts-mode-update--state nil)
          (when reset (awk-ts-mode-update--reset-parser))
          (let* ((bounds (if reset (cons (point-min) (point-max))
                           (awk-ts-mode-update--bounds beg end)))
                 (start (min (car bounds) (or (caar state) beg)))
                 (finish (min (point-max)
                              (max (cdr bounds)
                                   (+ (or (cdar state) (+ beg old-length))
                                      (- end beg old-length)))))
                 (pending awk-ts-mode-update--pending-range)
                 (pending-end (cdr pending)))
            (when (and pending-end (>= pending-end beg))
              (setq pending-end (max end (+ pending-end (- end beg old-length)))))
            (setq awk-ts-mode-update--pending-range
                  (cons (min start (or (car pending) start))
                        (min (point-max) (max finish (or pending-end finish)))))
            (syntax-ppss-flush-cache start)
            (font-lock-flush start finish)
            (when narrowed (syntax-propertize finish))))))))

(defun awk-ts-mode-update--extend-region (start end)
  "Include edited owners when propertizing START to END."
  (when awk-ts-mode-update--pending-range
    (let ((bounds awk-ts-mode-update--pending-range))
      (setq awk-ts-mode-update--pending-range nil)
      (cons (max (point-min) (min start (car bounds)))
            (min (point-max) (max end (cdr bounds)))))))

(defun awk-ts-mode-update--setup ()
  "Configure incremental updates for the current buffer."
  (add-hook 'before-change-functions #'awk-ts-mode-update--before-change nil t)
  (add-hook 'after-change-functions #'awk-ts-mode-update--after-change nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'awk-ts-mode-update--extend-region nil t))

;;;; Context

(defconst awk-ts-mode--expression-types
  '("non_unary_expr" "unary_expr" "non_unary_print_expr" "unary_print_expr")
  "Node types that can own AWK expression structure.")

(defun awk-ts-mode--delimiter-query (capture)
  "Return a query for structural delimiters with CAPTURE."
  `((action ["{" "}"] ,capture)
    (item ["(" ")"] ,capture)
    (terminated_statement ["(" ")"] ,capture)
    (unterminated_statement ["(" ")"] ,capture)
    (terminatable_statement ["(" ")"] ,capture)
    (simple_print_statement ["(" ")"] ,capture)
    (non_unary_expr ["(" ")"] ,capture)
    (non_unary_print_expr ["(" ")"] ,capture)
    (lvalue ["[" "]"] ,capture)
    (simple_statement ["[" "]"] ,capture)))

(defun awk-ts-mode--gnu-delimiter-query (capture)
  "Return GNU structural delimiter patterns with CAPTURE."
  `((block ["{" "}"] ,capture)
    (switch_statement ["(" ")" "{" "}"] ,capture)
    (if_statement ["(" ")"] ,capture)
    (while_statement ["(" ")"] ,capture)
    (do_statement ["(" ")"] ,capture)
    (for_statement ["(" ")"] ,capture)
    (for_in_statement ["(" ")"] ,capture)
    (parenthesized_expression ["(" ")"] ,capture)
    (parameter_list ["(" ")"] ,capture)
    (argument_list ["(" ")"] ,capture)
    (output_arguments ["(" ")"] ,capture)
    (index_list ["(" ")"] ,capture)
    (subscript_list ["[" "]"] ,capture)))

;;;; Syntax

(defvar awk-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?' ?` ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar awk-ts-mode-syntax-table
  (let ((table (copy-syntax-table awk-ts-mode-syntax--text-table)))
    (dolist (entry '((?\( . "()") (?\) . ")(")
                     (?\[ . "(]") (?\] . ")[")
                     (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `awk-ts-mode'.")

;;;;; Syntax Queries

(defvar-local awk-ts-mode-syntax--query nil
  "Compiled syntax query for the selected language.")

;;;;; Propertization

(defun awk-ts-mode-syntax--delimiter-syntax (position)
  "Return the syntax descriptor for the delimiter at POSITION."
  (pcase (char-after position)
    (?\( (string-to-syntax "()"))
    (?\) (string-to-syntax ")("))
    (?\[ (string-to-syntax "(]"))
    (?\] (string-to-syntax ")["))
    (?{ (string-to-syntax "(}"))
    (?} (string-to-syntax "){"))))

(defvar-local awk-ts-mode-syntax--changed-start nil
  "Earliest pending syntax change reported by the parser.")

(defun awk-ts-mode-syntax--changed (ranges _parser)
  "Invalidate syntax properties for the parser's changed RANGES."
  (dolist (range ranges)
    (setq awk-ts-mode-syntax--changed-start
          (min (or awk-ts-mode-syntax--changed-start (car range))
               (car range))))
  (when awk-ts-mode-syntax--changed-start
    (syntax-ppss-flush-cache awk-ts-mode-syntax--changed-start)))

(defun awk-ts-mode-syntax--extend-region (start end)
  "Extend START and END to include pending structural changes."
  (treesit-parser-root-node treesit-primary-parser)
  (let ((begin awk-ts-mode-syntax--changed-start))
    (setq awk-ts-mode-syntax--changed-start nil)
    (when (and begin (< begin start))
      (cons (max (point-min) begin) end))))

(defun awk-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table awk-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        awk-ts-mode-syntax--query start end))
        (let* ((name (car capture))
               (node (cdr capture))
               (position (if (eq name 'comment)
                             (treesit-node-start node)
                           (1- (treesit-node-end node)))))
          (if (eq name 'string)
              (let ((opening (treesit-node-child-by-field-name node "opening"))
                    (closing (treesit-node-child-by-field-name node "closing")))
                (when (and opening closing
                           (not (treesit-node-check node 'has-error)))
                  (dolist (position (list (treesit-node-start opening)
                                          (1- (treesit-node-end closing))))
                    (put-text-property position (1+ position) 'syntax-table
                                       (string-to-syntax "|")))))
            (put-text-property
             position (1+ position) 'syntax-table
             (if (eq name 'comment)
                 (string-to-syntax "<")
               (awk-ts-mode-syntax--delimiter-syntax position)))))))))

;;;;; Setup

(defun awk-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (let ((language (treesit-parser-language treesit-primary-parser)))
    (setq-local awk-ts-mode-syntax--query
                (treesit-query-compile
                 language
                 (if (eq language 'gawk)
                     (append '((comment) @comment [(string) (regexp) (directive_string)] @string)
                             (awk-ts-mode--gnu-delimiter-query '@delimiter))
                   (append '((comment) @comment [(string) (ere)] @string)
                           (awk-ts-mode--delimiter-query '@delimiter))))))
  (treesit-parser-add-notifier treesit-primary-parser #'awk-ts-mode-syntax--changed)
  (setq-local syntax-propertize-function
              #'awk-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'awk-ts-mode-syntax--extend-region t t))

;;;; Comment Commands

(defun awk-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

;;;; Electric Pair

(defun awk-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil for a multiline CST delimiter pair around the newline."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening))
           (type (treesit-node-type owner)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (treesit-node-eq owner (treesit-node-parent closing))
           (if (eq (treesit-parser-language treesit-primary-parser) 'gawk)
               (and (equal (treesit-node-type opening) "{")
                    (member type '("block" "switch_statement")))
             (or (equal type "action")
                 (member type awk-ts-mode--expression-types)
                 (and (equal (treesit-node-type opening) "[")
                      (member type '("lvalue" "simple_statement")))))))))

(defun awk-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})
                 (?\" . ?\")))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (awk-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst awk-ts-mode-font-lock--feature-list
  '((comment)
    (function parameter builtin keyword directive namespace string variable)
    (number escape)
    (operator delimiter punctuation bracket regexp))
  "Font-lock features by decoration level.")

(defun awk-ts-mode-font-lock--gnu-regexp-p (node)
  "Return non-nil if NODE belongs to a static regexp."
  (treesit-parent-until node (lambda (parent) (equal (treesit-node-type parent) "regexp"))))

(defun awk-ts-mode-font-lock--gnu-outside-regexp-p (node)
  "Return non-nil if NODE is outside a static regexp."
  (not (awk-ts-mode-font-lock--gnu-regexp-p node)))

;;;;; Settings

(defun awk-ts-mode-font-lock--settings ()
  "Return font-lock settings for the current buffer."
  (treesit-font-lock-rules
   :default-language 'posix-awk

   :feature 'comment
   '((comment) @font-lock-comment-face)

   :feature 'function
   '((item
      name: [(name) (func_name)] @font-lock-function-name-face)
     (func_name) @font-lock-function-call-face)

   :feature 'parameter
   '((param_list
      (name) @font-lock-variable-name-face))

   :feature 'builtin
   '((builtin_func_name) @font-lock-builtin-face)

   :feature 'keyword
   '([(begin_keyword) (break_keyword) (continue_keyword) (delete_keyword)
      (do_keyword) (else_keyword) (end_keyword) (exit_keyword) (for_keyword)
      (function_keyword) (getline_keyword) (if_keyword) (in_keyword)
      (next_keyword) (nextfile_keyword) (print_keyword) (printf_keyword)
      (return_keyword) (while_keyword)]
     @font-lock-keyword-face)

   :feature 'escape
   '((string
      (escape_sequence) @font-lock-escape-face))

   :feature 'string
   '((string "\"" @font-lock-string-face)
     (string_content) @font-lock-string-face
     (string
      (escape_sequence) @font-lock-string-face))

   :feature 'variable
   '((name) @font-lock-variable-use-face)

   :feature 'number
   '((number) @font-lock-number-face)

   :feature 'delimiter
   '((ere "/" @font-lock-delimiter-face))

   :feature 'regexp
   '([(quoted_character) (escape_sequence) (escaped_delimiter)]
     @font-lock-escape-face
     (ordinary_character !escape) @font-lock-regexp-face
     [(collating_element_single) (collating_element_multi)
      (collating_element_content)] @font-lock-constant-face
     (one_char_or_coll_elem_ere "." @font-lock-constant-face)
     (class_name) @font-lock-constant-face
     (meta_character) @font-lock-constant-face
     (duplication_count) @font-lock-number-face
     (start_range "-" @font-lock-operator-face)
     (range_expression "-" @font-lock-constant-face)
     (bracket_list "-" @font-lock-constant-face)
     (bracket_expression
      ["[" "]"] @font-lock-bracket-face)
     (nonmatching_list "^" @font-lock-negation-char-face)
     (collating_symbol
      ["[" "]"] @font-lock-bracket-face)
     (collating_symbol
      "." @font-lock-punctuation-face)
     (equivalence_class
      ["[" "]"] @font-lock-bracket-face)
     (equivalence_class
      "=" @font-lock-punctuation-face)
     (character_class
      ["[" "]"] @font-lock-bracket-face)
     (character_class
      ":" @font-lock-punctuation-face)
     (ere_expression
      ["(" ")"] @font-lock-bracket-face)
     (extended_reg_exp
      "|" @font-lock-operator-face)
     (ere_expression ["^" "$"] @font-lock-operator-face)
     (ere_dupl_symbol
      ["*" "+" "?"] @font-lock-operator-face)
     (ere_dupl_symbol
      ["{" "}"] @font-lock-bracket-face)
     (ere_dupl_symbol
      "," @font-lock-punctuation-face)
     (repetition_modifier) @font-lock-operator-face)

   :feature 'operator
   '([(add_assign) (and) (append) (decr) (div_assign)
      (eq) (ge) (incr) (le) (mod_assign) (mul_assign)
      (ne) (no_match) (or) (pow_assign) (sub_assign)
      "!" "$" "%" "*" "+" "/" ":" "<" "=" ">"
      "?" "^" "|" "~" "-"]
     @font-lock-operator-face)

   :feature 'punctuation
   '(["," ";" "\\"] @font-lock-punctuation-face)

   :feature 'bracket
   (awk-ts-mode--delimiter-query '@font-lock-bracket-face)))

(defun awk-ts-mode-font-lock--gnu-settings ()
  "Return GNU Awk font-lock settings."
  (treesit-font-lock-rules
   :default-language 'gawk
   :feature 'comment
   '((comment) @font-lock-comment-face)
   :feature 'function
   '((function_definition name: (identifier) @font-lock-function-name-face)
     (function_definition name: (qualified_name name: (identifier) @font-lock-function-name-face))
     (function_call name: (identifier) @font-lock-function-call-face)
     (function_call name: (qualified_name name: (identifier) @font-lock-function-call-face)))
   :feature 'parameter
   '((parameter_list (identifier) @font-lock-variable-name-face))
   :feature 'builtin
   '((builtin_function) @font-lock-builtin-face)
   :feature 'keyword
   '(["BEGIN" "END" "BEGINFILE" "ENDFILE" "function" "func" "if" "else"
      "while" "do" "for" "switch" "case" "default" "break" "continue"
      "next" "nextfile" "return" "exit" "delete" "print" "printf" "getline"] @font-lock-keyword-face
      (for_in_statement "in" @font-lock-keyword-face)
      (typed_regexp "@" @font-lock-keyword-face))
   :feature 'directive
   '((namespace_directive ["@" "namespace"] @font-lock-preprocessor-face)
     (include_directive ["@" "include"] @font-lock-preprocessor-face)
     (nsinclude_directive ["@" "nsinclude"] @font-lock-preprocessor-face)
     (load_directive ["@" "load"] @font-lock-preprocessor-face)
     (localized_string "_" @font-lock-preprocessor-face)
     (directive_string "_" @font-lock-preprocessor-face))
   :feature 'namespace
   '((qualified_name namespace: (identifier) @font-lock-constant-face))
   :feature 'escape
   '(((escape_sequence !fragment) @font-lock-escape-face
      (:pred awk-ts-mode-font-lock--gnu-outside-regexp-p @font-lock-escape-face))
     ((escape_fragment) @font-lock-escape-face
      (:pred awk-ts-mode-font-lock--gnu-outside-regexp-p @font-lock-escape-face)))
   :feature 'string
   '((string "\"" @font-lock-string-face)
     (directive_string "\"" @font-lock-string-face)
     [(string_content) (directive_text)] @font-lock-string-face
     ((escape_sequence !fragment) @font-lock-string-face
      (:pred awk-ts-mode-font-lock--gnu-outside-regexp-p @font-lock-string-face))
     ((escape_fragment) @font-lock-string-face
      (:pred awk-ts-mode-font-lock--gnu-outside-regexp-p @font-lock-string-face)))
   :feature 'variable
   '((identifier) @font-lock-variable-use-face)
   :feature 'number
   '((number) @font-lock-number-face)
   :feature 'delimiter
   '((regexp "/" @font-lock-delimiter-face))
   :feature 'regexp
   '((regex_character (regex_text) @font-lock-regexp-face)
     (regex_operator ["|" "*" "+" "?" "-" (regex_text)] @font-lock-operator-face)
     (regex_anchor ["^" "$" (regex_text)] @font-lock-operator-face)
     (regex_wildcard ["." (regex_text)] @font-lock-constant-face)
     (regex_character_class (regex_text) @font-lock-constant-face)
     (regex_bracket_expression "^" @font-lock-negation-char-face)
     (regex_delimiter ["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)
     (regex_delimiter [":" "." "="] @font-lock-punctuation-face)
     (regex_interval "," @font-lock-punctuation-face)
     (regex_digits) @font-lock-number-face
     (regex_repetition_modifier "?" @font-lock-operator-face)
     (regex_class_name (regex_text) @font-lock-constant-face)
     (regex_collating_element (regex_text) @font-lock-constant-face)
     ((escape_sequence !fragment) @font-lock-escape-face
      (:pred awk-ts-mode-font-lock--gnu-regexp-p @font-lock-escape-face))
     ((escape_fragment) @font-lock-escape-face
      (:pred awk-ts-mode-font-lock--gnu-regexp-p @font-lock-escape-face)))
   :feature 'operator
   '((assignment_expression operator: _ @font-lock-operator-face)
     (binary_expression operator: _ @font-lock-operator-face)
     (unary_expression operator: _ @font-lock-operator-face)
     (update_expression operator: _ @font-lock-operator-face)
     (field_reference operator: _ @font-lock-operator-face)
     (membership_expression "in" @font-lock-operator-face)
     (conditional_expression ["?" ":"] @font-lock-operator-face)
     (indirect_function_call "@" @font-lock-operator-face)
     (getline_expression operator: _ @font-lock-operator-face)
     (output_redirection operator: _ @font-lock-operator-face))
   :feature 'punctuation
   '(["," ";" "::"] @font-lock-punctuation-face
     (case_clause ":" @font-lock-punctuation-face)
     (default_clause ":" @font-lock-punctuation-face)
     (line_continuation) @font-lock-punctuation-face)
   :feature 'bracket
   (awk-ts-mode--gnu-delimiter-query '@font-lock-bracket-face)))

;;;;; Setup

(defun awk-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              awk-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (if (eq (treesit-parser-language treesit-primary-parser) 'gawk)
                  (awk-ts-mode-font-lock--gnu-settings)
                (awk-ts-mode-font-lock--settings))))

;;;; Navigation

(defconst awk-ts-mode-navigation--item-regexp
  "^item$"
  "Regexp matching POSIX awk items.")

(defun awk-ts-mode-navigation--defun-p (node)
  "Return non-nil when NODE is a POSIX awk function item."
  (and (member (treesit-node-type node) '("item" "function_definition"))
       (treesit-node-child-by-field-name node "name")
       (treesit-node-child-by-field-name node "body")))

(defun awk-ts-mode-navigation--sexp-p (node)
  "Return non-nil when NODE represents an AWK editing unit."
  (let ((type (treesit-node-type node)))
    (and (< (treesit-node-start node) (treesit-node-end node))
         (cond
          ((member type '("item" "action" "name" "func_name"
                          "builtin_func_name" "number" "string" "ere"
                          "non_unary_input_function" "unary_input_function")))
          ((equal type "simple_statement")
           (member (treesit-node-field-name node) '("initializer" "update")))
          ((member type '("terminated_statement" "unterminated_statement"))
           (let ((children (treesit-node-children node t)))
             (while (and children
                         (member (treesit-node-type (car children))
                                 '("comment" "newline" "newline_opt")))
               (setq children (cdr children)))
             children))
          ((member type awk-ts-mode--expression-types)
           (or (treesit-node-child-by-field-name node "operator")
               (treesit-node-child-by-field-name node "condition")
               (and (treesit-node-child-by-field-name node "left")
                    (treesit-node-child-by-field-name node "right"))
               (member "(" (mapcar #'treesit-node-type
                                   (treesit-node-children node)))))
          ((equal type "lvalue")
           (or (treesit-node-child-by-field-name node "operand")
               (member "expr_list" (mapcar #'treesit-node-type
                                           (treesit-node-children node t)))))))))

(defun awk-ts-mode-navigation--gnu-sexp-p (node)
  "Return non-nil if NODE is a GNU Awk editing unit."
  (let ((type (treesit-node-type node))
        (parent (treesit-node-type (treesit-node-parent node))))
    (and (< (treesit-node-start node) (treesit-node-end node))
         (not (member parent '("qualified_name" "localized_string" "typed_regexp")))
         (not (equal type "empty_statement"))
         (or (member type '("rule" "function_definition" "block" "identifier"
                            "qualified_name" "number" "string" "localized_string"
                            "regexp" "typed_regexp" "function_call" "indirect_function_call"
                            "field_reference"))
             (string-match-p "_statement$" type)
             (string-match-p "_expression$" type)
             (string-match-p "_directive$" type)))))

(defconst awk-ts-mode-navigation--settings
  `((posix-awk
     (sexp awk-ts-mode-navigation--sexp-p)
     (defun (,awk-ts-mode-navigation--item-regexp
             . awk-ts-mode-navigation--defun-p))))
  "Tree-sitter thing definitions for POSIX awk.")

(defun awk-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              (if (eq (treesit-parser-language treesit-primary-parser) 'gawk)
                  '((gawk (sexp awk-ts-mode-navigation--gnu-sexp-p)
                          (defun "function_definition")))
                awk-ts-mode-navigation--settings)))

;;;; Imenu

(defun awk-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (awk-ts-mode-navigation--defun-p node)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (member (treesit-node-type name) '("name" "func_name" "identifier" "qualified_name"))
        (treesit-node-text name t)))))

(defconst awk-ts-mode-imenu--settings
  `(("Function" ,awk-ts-mode-navigation--item-regexp
     awk-ts-mode-navigation--defun-p nil))
  "Tree-sitter Imenu settings for POSIX awk.")

(defun awk-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'awk-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              (if (eq (treesit-parser-language treesit-primary-parser) 'gawk)
                  '(("Function" "function_definition" nil nil))
                awk-ts-mode-imenu--settings)))

;;;; Indentation

(defcustom awk-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'awk-ts)

(defun awk-ts-mode-indent--continuation (_node parent _bol)
  "Return the indentation for expression continuation in PARENT."
  (when (equal (treesit-node-type parent) "newline_opt")
    (setq parent (treesit-node-parent parent)))
  (when (member (treesit-node-type parent)
                (append awk-ts-mode--expression-types
                        '("print_expr_list" "multiple_expr_list"
                          "param_list" "normal_pattern")))
    (cons (save-excursion
            (goto-char (treesit-node-start parent))
            (back-to-indentation)
            (point))
          awk-ts-mode-indent-offset)))

(defun awk-ts-mode-indent--statement-newline (node parent _bol)
  "Return the indentation for statement layout at NODE in PARENT."
  (let* ((layout (if (equal (treesit-node-type node) "newline_opt") node parent))
         (statement (treesit-node-parent layout)))
    (when (and (equal (treesit-node-type layout) "newline_opt")
               (member (treesit-node-type statement)
                       '("terminated_statement" "terminatable_statement")))
      (let ((next (treesit-node-next-sibling layout t))
            (offset 0))
        (while (equal (treesit-node-type next) "comment")
          (setq next (treesit-node-next-sibling next t)))
        (if (member (treesit-node-field-name next) '("body" "consequence" "alternative"))
            (setq offset awk-ts-mode-indent-offset)
          (while (member (treesit-node-field-name statement)
                         '("body" "consequence" "alternative"))
            (setq statement (treesit-node-parent statement))))
        (cons (save-excursion
                (goto-char (treesit-node-start statement))
                (back-to-indentation)
                (point))
              offset)))))

(defconst awk-ts-mode-indent--rules
  '((posix-awk
     ((and (node-is "}")
           (lambda (node _parent _bol)
             (not (treesit-node-check node 'missing))))
      parent-bol 0)
     ((node-is "else_keyword") parent-bol 0)
     ((node-is "while_keyword") parent-bol 0)
     ((field-is "body") parent-bol awk-ts-mode-indent-offset)
     ((field-is "consequence") parent-bol awk-ts-mode-indent-offset)
     ((field-is "alternative") parent-bol awk-ts-mode-indent-offset)
     ((parent-is "action") parent-bol awk-ts-mode-indent-offset)
     awk-ts-mode-indent--continuation
     ((n-p-gp nil "newline_opt" "action")
      parent-bol awk-ts-mode-indent-offset)
     awk-ts-mode-indent--statement-newline
     ((node-is "newline_opt") parent-bol 0)
     ((parent-is "terminated_statement_list") first-sibling 0)
     ((parent-is "unterminated_statement_list") first-sibling 0)
     ((parent-is "program") column-0 0)
     ((parent-is "item_list") column-0 0)))
  "Tree-sitter indentation rules for POSIX awk.")

(defun awk-ts-mode-indent--gnu-literal-content-p (node parent bol)
  "Return non-nil for literal content at BOL in NODE or PARENT."
  (let ((literal (treesit-parent-until (or node parent)
                                       (lambda (candidate)
                                         (member (treesit-node-type candidate) '("string" "regexp" "directive_string"))) t)))
    (and literal (> bol (treesit-node-start literal)))))

(defun awk-ts-mode-indent--gnu-body (_node parent _bol)
  "Return the indentation for a statement list in PARENT."
  (when (equal (treesit-node-type parent) "statement_list")
    (let ((owner (treesit-node-parent parent)))
      (cons (save-excursion (goto-char (treesit-node-start owner)) (back-to-indentation) (point))
            awk-ts-mode-indent-offset))))

(defun awk-ts-mode-indent--gnu-control-newline (node parent _bol)
  "Return indentation for layout NODE inside a control PARENT."
  (when (and (equal (treesit-node-type node) "\n")
             (member (treesit-node-type parent)
                     '("if_statement" "while_statement" "do_statement"
                       "for_statement" "for_in_statement")))
    (let ((next (treesit-node-next-sibling node)))
      (while (member (treesit-node-type next) '("\n" "comment"))
        (setq next (treesit-node-next-sibling next)))
      (cons (save-excursion (goto-char (treesit-node-start parent)) (back-to-indentation) (point))
            (if (member (treesit-node-field-name next) '("body" "consequence" "alternative"))
                awk-ts-mode-indent-offset
              0)))))

(defconst awk-ts-mode-indent--gnu-rules
  '((gawk
     (awk-ts-mode-indent--gnu-literal-content-p no-indent 0)
     ((node-is "}") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((and (node-is "while") (parent-is "do_statement")) parent-bol 0)
     ((or (node-is "case_clause") (node-is "default_clause"))
      parent-bol awk-ts-mode-indent-offset)
     ((field-is "body") parent-bol awk-ts-mode-indent-offset)
     ((field-is "consequence") parent-bol awk-ts-mode-indent-offset)
     ((field-is "alternative") parent-bol awk-ts-mode-indent-offset)
     awk-ts-mode-indent--gnu-control-newline
     awk-ts-mode-indent--gnu-body
     ((or (parent-is "block") (parent-is "switch_statement")
          (parent-is "case_clause") (parent-is "default_clause"))
      parent-bol awk-ts-mode-indent-offset)
     ((or (parent-is ".*_expression") (parent-is "argument_list")
          (parent-is "output_arguments") (parent-is "parameter_list")
          (parent-is "index_list") (parent-is "subscript_list")
          (parent-is "range_pattern"))
      parent-bol awk-ts-mode-indent-offset)
     ((parent-is "program") column-0 0)))
  "GNU Awk indentation rules.")

(defun awk-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              (if (eq (treesit-parser-language treesit-primary-parser) 'gawk)
                  awk-ts-mode-indent--gnu-rules
                awk-ts-mode-indent--rules)))

;;;; Mode

(defun awk-ts-mode--setup ()
  "Configure `awk-ts-mode' in the current buffer."
  (let ((language (awk-ts-mode--language)))
    (awk-ts-mode--ensure-grammar language)
    (setq-local treesit-primary-parser (treesit-parser-create language)))
  (awk-ts-mode-syntax--setup)
  (awk-ts-mode-comment--setup)
  (awk-ts-mode-electric-pair--setup)
  (awk-ts-mode-font-lock--setup)
  (awk-ts-mode-navigation--setup)
  (awk-ts-mode-imenu--setup)
  (awk-ts-mode-indent--setup)
  (treesit-major-mode-setup)
  (awk-ts-mode-update--setup))

;;;###autoload
(define-derived-mode awk-ts-mode prog-mode "Awk-TS"
  "Major mode for editing GNU and POSIX awk."
  :syntax-table awk-ts-mode-syntax-table
  :group 'awk-ts
  (awk-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("awk" . awk-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("gawk" . awk-ts-mode))

(provide 'awk-ts-mode)

;;; awk-ts-mode.el ends here
