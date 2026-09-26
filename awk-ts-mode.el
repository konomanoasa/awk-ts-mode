;;; awk-ts-mode.el --- Tree-sitter mode for POSIX awk  -*- lexical-binding: t; -*-
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
;; Tree-sitter major mode for POSIX awk.

;;; Code:

(require 'treesit)
(require 'elec-pair)

(defgroup awk-ts nil
  "Tree-sitter mode for POSIX awk."
  :group 'languages)

;;;; Grammar

(defconst awk-ts-mode--grammar-sources
  '((posix-awk "https://github.com/konomanoasa/tree-sitter-posix-awk"
               :revision "v0.18.0"))
  "Tree-sitter grammar sources for POSIX awk.")

(defun awk-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language awk-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

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

(defconst awk-ts-mode-syntax--query
  (treesit-query-compile
   'posix-awk
   (append '((comment) @comment
             [(string) (ere)] @string)
           (awk-ts-mode--delimiter-query '@delimiter)))
  "Compiled syntax query for POSIX awk.")

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
  (setq-local syntax-propertize-function
              #'awk-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
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
           (or (equal type "action")
               (member type awk-ts-mode--expression-types)
               (and (equal (treesit-node-type opening) "[")
                    (member type '("lvalue" "simple_statement"))))))))

(defun awk-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})))
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
    (function parameter builtin keyword string variable)
    (number escape)
    (operator delimiter punctuation bracket regexp))
  "Font-lock features by decoration level.")

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
   '([(escape_sequence) (escaped_delimiter)] @font-lock-escape-face
     (ordinary_character_content) @font-lock-regexp-face
     (ordinary_character [")" "}"] @font-lock-regexp-face)
     (collating_element_content) @font-lock-constant-face
     (collating_element "-" @font-lock-constant-face)
     (wildcard "." @font-lock-constant-face)
     (class_name) @font-lock-constant-face
     (meta_character) @font-lock-constant-face
     (dup_count) @font-lock-number-face
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
      operator: "|" @font-lock-operator-face)
     [(left_anchor) (right_anchor)] @font-lock-operator-face
     (ere_dupl_symbol
      ["*" "+" "?"] @font-lock-operator-face)
     (ere_dupl_symbol
      ["{" "}"] @font-lock-bracket-face)
     (ere_dupl_symbol
      "," @font-lock-punctuation-face)
     (repetition_modifier "?" @font-lock-operator-face))

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

;;;;; Setup

(defun awk-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              awk-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (awk-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst awk-ts-mode-navigation--item-regexp
  "^item$"
  "Regexp matching POSIX awk items.")

(defun awk-ts-mode-navigation--defun-p (node)
  "Return non-nil when NODE is a POSIX awk function item."
  (and (treesit-node-match-p node awk-ts-mode-navigation--item-regexp)
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

(defconst awk-ts-mode-navigation--settings
  `((posix-awk
     (sexp awk-ts-mode-navigation--sexp-p)
     (defun (,awk-ts-mode-navigation--item-regexp
             . awk-ts-mode-navigation--defun-p))))
  "Tree-sitter thing definitions for POSIX awk.")

(defun awk-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              awk-ts-mode-navigation--settings))

;;;; Imenu

(defun awk-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (awk-ts-mode-navigation--defun-p node)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (member (treesit-node-type name) '("name" "func_name"))
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
              awk-ts-mode-imenu--settings))

;;;; Indentation

(defcustom awk-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'awk-ts)

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
     ((n-p-gp nil "newline_opt" "action")
      parent-bol awk-ts-mode-indent-offset)
     ((and (n-p-gp nil "newline_opt" "terminated_statement")
           (lambda (_node parent _bol)
             (member "action"
                     (mapcar #'treesit-node-type
                             (treesit-node-children
                              (treesit-node-parent parent) t)))))
      parent-bol 0)
     ((node-is "newline_opt") parent-bol 0)
     ((parent-is "terminated_statement_list") first-sibling 0)
     ((parent-is "unterminated_statement_list") first-sibling 0)
     ((parent-is "program") column-0 0)
     ((parent-is "item_list") column-0 0)))
  "Tree-sitter indentation rules for POSIX awk.")

(defun awk-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              awk-ts-mode-indent--rules))

;;;; Mode

(defun awk-ts-mode--setup ()
  "Configure `awk-ts-mode' in the current buffer."
  (awk-ts-mode--ensure-grammar 'posix-awk)
  (setq-local treesit-primary-parser (treesit-parser-create 'posix-awk))
  (awk-ts-mode-syntax--setup)
  (awk-ts-mode-electric-pair--setup)
  (awk-ts-mode-font-lock--setup)
  (awk-ts-mode-navigation--setup)
  (awk-ts-mode-imenu--setup)
  (awk-ts-mode-indent--setup)
  (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode awk-ts-mode prog-mode "Awk-TS"
  "Major mode for editing POSIX awk."
  :syntax-table awk-ts-mode-syntax-table
  :group 'awk-ts
  (awk-ts-mode--setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.awk\\'" . awk-ts-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("awk" . awk-ts-mode))

(provide 'awk-ts-mode)

;;; awk-ts-mode.el ends here
