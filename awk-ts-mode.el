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

(defgroup awk-ts nil
  "Tree-sitter mode for POSIX awk."
  :group 'languages)

(defconst awk-ts-mode--grammar-sources
  '((posix-awk "https://github.com/konomanoasa/tree-sitter-posix-awk"
               :revision "v0.14.0"))
  "Tree-sitter grammar sources for POSIX awk.")

;;;; Context

(defun awk-ts-mode--ancestor-p (node type)
  "Return non-nil when NODE has an ancestor of TYPE."
  (let (found)
    (while (and node (not found))
      (when (equal (treesit-node-type node) type)
        (setq found t))
      (setq node (treesit-node-parent node)))
    found))

(defun awk-ts-mode--delimiter-p (node)
  "Return non-nil when NODE is a structural delimiter."
  (let* ((type (treesit-node-type node))
         (parent (treesit-node-parent node))
         (parent-type (and parent (treesit-node-type parent))))
    (and parent
         (not (equal parent-type "ERROR"))
         (not (awk-ts-mode--ancestor-p parent "ere"))
         (or (not (member type '("{" "}")))
             (equal parent-type "action")))))

;;;; Syntax

(defvar awk-ts-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `awk-ts-mode'.")

(defvar awk-ts-mode-syntax--query-cache nil
  "Cached syntax query.")

;;;;; Syntax Queries

(defun awk-ts-mode-syntax--query ()
  "Return the cached syntax query."
  (or awk-ts-mode-syntax--query-cache
      (setq awk-ts-mode-syntax--query-cache
            (treesit-query-compile
             'posix-awk
             '((comment) @comment
               ((_ ["(" ")" "[" "]" "{" "}"] @delimiter)
                (:pred awk-ts-mode--delimiter-p @delimiter)))
             t))))

;;;;; Propertization

(defun awk-ts-mode-syntax--delimiter-syntax (position)
  "Return syntax-table syntax for the delimiter at POSITION."
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
        (remove-text-properties (point-min) start '(syntax-table nil))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        (awk-ts-mode-syntax--query) start end))
        (let* ((name (car capture))
               (node (cdr capture))
               (position (if (eq name 'comment)
                             (treesit-node-start node)
                           (1- (treesit-node-end node)))))
          (put-text-property
           position (1+ position) 'syntax-table
           (if (eq name 'comment)
               (string-to-syntax "<")
             (awk-ts-mode-syntax--delimiter-syntax position))))))))

;;;;; Setup

(defun awk-ts-mode-syntax-setup ()
  "Configure syntax handling for the current buffer."
  (setq-local syntax-propertize-function
              #'awk-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t))

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
  "Return the font-lock settings."
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
   '(((["(" ")" "[" "]" "{" "}"]
       @font-lock-bracket-face)
      (:pred awk-ts-mode--delimiter-p @font-lock-bracket-face)))))

;;;;; Setup

(defun awk-ts-mode-font-lock-setup ()
  "Configure font locking for the current buffer."
  (setq-local treesit-font-lock-feature-list
              awk-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (awk-ts-mode-font-lock--settings)))

;;;; Navigation

(defconst awk-ts-mode--item-regexp
  "^item$"
  "Regexp matching POSIX awk items.")

(defun awk-ts-mode--function-item-p (node)
  "Return non-nil when NODE is a POSIX awk function item."
  (and (treesit-node-match-p node awk-ts-mode--item-regexp)
       (treesit-node-child-by-field-name node "name")
       (treesit-node-child-by-field-name node "body")))

(defconst awk-ts-mode-thing-settings
  `((posix-awk
     (sexp ,awk-ts-mode--item-regexp)
     (defun (,awk-ts-mode--item-regexp
             . awk-ts-mode--function-item-p))))
  "Tree-sitter thing definitions for POSIX awk.")

(defun awk-ts-mode-navigation-setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings awk-ts-mode-thing-settings))

;;;; Imenu

(defconst awk-ts-mode-imenu-settings
  `(("Function" ,awk-ts-mode--item-regexp
     awk-ts-mode--function-item-p nil))
  "Tree-sitter Imenu settings for POSIX awk.")

(defun awk-ts-mode--defun-name (node)
  "Return the name of the function item NODE."
  (when (awk-ts-mode--function-item-p node)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (member (treesit-node-type name) '("name" "func_name"))
        (treesit-node-text name t)))))

(defun awk-ts-mode-imenu-setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function #'awk-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings awk-ts-mode-imenu-settings))

;;;; Indentation

(defcustom awk-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'awk-ts)

(defconst awk-ts-mode-indent-rules
  '((posix-awk
     ((node-is "}") parent-bol 0)
     ((node-is "else_keyword") parent-bol 0)
     ((node-is "while_keyword") parent-bol 0)
     ((field-is "body") parent-bol awk-ts-mode-indent-offset)
     ((field-is "consequence") parent-bol awk-ts-mode-indent-offset)
     ((field-is "alternative") parent-bol awk-ts-mode-indent-offset)
     ((parent-is "terminated_statement_list") first-sibling 0)
     ((parent-is "unterminated_statement_list") first-sibling 0)
     ((parent-is "program") column-0 0)
     ((parent-is "item_list") column-0 0)))
  "Tree-sitter indentation rules for POSIX awk.")

(defun awk-ts-mode-indent-setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              awk-ts-mode-indent-rules))

;;;; Mode

(defun awk-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language awk-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun awk-ts-mode--setup ()
  "Configure `awk-ts-mode' in the current buffer."
  (awk-ts-mode--ensure-grammar 'posix-awk)
  (setq-local treesit-primary-parser (treesit-parser-create 'posix-awk))
  (awk-ts-mode-syntax-setup)
  (awk-ts-mode-font-lock-setup)
  (awk-ts-mode-navigation-setup)
  (awk-ts-mode-imenu-setup)
  (awk-ts-mode-indent-setup)
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
