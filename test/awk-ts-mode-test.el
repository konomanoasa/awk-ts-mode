;;; awk-ts-mode-test.el --- Tests for awk-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'awk-ts-mode)

(dolist (language '(posix-awk gawk))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defmacro awk-ts-mode-test--with-file (file properties source &rest body)
  "Evaluate BODY with SOURCE in FILE using EditorConfig PROPERTIES."
  (declare (indent 3) (debug (form form form body)))
  `(let ((default-directory
          (file-name-as-directory (make-temp-file "awk-ts-mode-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".editorconfig")
             (insert "root = true\n[*]\n" ,properties))
           (with-temp-buffer
             (setq buffer-file-name (expand-file-name ,file))
             (insert ,source)
             (awk-ts-mode)
             ,@body))
       (delete-directory default-directory t))))

(defconst awk-ts-mode-test--posix-file
  (expand-file-name "posix.awk" (file-name-directory load-file-name))
  "File name matched by the POSIX test configuration.")

(defun awk-ts-mode-test--posix ()
  "Activate the mode with the POSIX test configuration."
  (let ((buffer-file-name awk-ts-mode-test--posix-file))
    (awk-ts-mode)))

(defun awk-ts-mode-test--position (fragment &optional line)
  "Return the position of FRAGMENT, optionally within LINE."
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun awk-ts-mode-test--face (fragment &optional offset line)
  "Return the face at FRAGMENT plus OFFSET, optionally within LINE."
  (get-text-property (+ (awk-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun awk-ts-mode-test--comment-p (fragment &optional offset line)
  "Return comment state at FRAGMENT plus OFFSET, optionally within LINE."
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss (+ (awk-ts-mode-test--position fragment line)
                         (or offset 0)))))

(defun awk-ts-mode-test--fontify (level lines)
  "Insert LINES and fontify them at LEVEL using the POSIX grammar."
  (let ((treesit-font-lock-level level))
    (insert (mapconcat #'identity lines "\n"))
    (awk-ts-mode-test--posix)
    (font-lock-ensure)))

(defun awk-ts-mode-test--indent (source &optional offset)
  "Indent SOURCE with OFFSET and verify that repeating it changes nothing."
  (with-temp-buffer
    (insert source)
    (awk-ts-mode-test--posix)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local awk-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun awk-ts-mode-test--buffer-state ()
  "Return fontification and syntax properties for the current buffer."
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun awk-ts-mode-test--should-match-fresh-buffer (&optional level compare-tree)
  "Compare classifications at LEVEL and optionally COMPARE-TREE with fresh parsing."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (awk-ts-mode-test--buffer-state))
        (tree (when compare-tree
                (treesit-node-string (treesit-parser-root-node treesit-primary-parser))))
        (file buffer-file-name)
        (level (or level treesit-font-lock-level))
        (language (treesit-parser-language treesit-primary-parser)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (if (eq language 'gawk) (awk-ts-mode) (awk-ts-mode-test--posix)))
      (should (equal state (awk-ts-mode-test--buffer-state)))
      (when compare-tree
        (should (equal tree (treesit-node-string
                             (treesit-parser-root-node treesit-primary-parser))))))))

;;;; Mode Selection

(ert-deftest awk-ts-mode-gnu-selects-only-from-editorconfig ()
  (let ((directory (make-temp-file "awk-ts-mode-language-" t)))
    (unwind-protect
        (dolist (option '(nil "true" "false" "unset" "invalid"))
          (with-temp-file (expand-file-name ".editorconfig" directory)
            (insert "root = true\n[*]\n")
            (when option (insert "posix = " option "\n")))
          (dolist (interpreter '("awk" "gawk"))
            (with-temp-buffer
              (setq buffer-file-name (expand-file-name "example.awk" directory))
              (insert "#!/usr/bin/env " interpreter " -f\nBEGIN {}")
              (if (equal option "invalid")
                  (should-error (awk-ts-mode) :type 'user-error)
                (awk-ts-mode)
                (should (eq (treesit-parser-language treesit-primary-parser)
                            (if (equal option "true") 'posix-awk 'gawk)))
                (awk-ts-mode)
                (should (equal (treesit-parser-list) (list treesit-primary-parser)))))))
      (delete-directory directory t)))
  (with-temp-buffer
    (awk-ts-mode)
    (should (eq (treesit-parser-language treesit-primary-parser) 'gawk))))

(ert-deftest awk-ts-mode-gnu-reselects-only-on-restart-and-checks-selected-grammar ()
  (let ((directory (make-temp-file "awk-ts-mode-language-" t))
        (ensure (symbol-function 'treesit-ensure-installed))
        requested)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language) (push language requested) t))
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "example.awk" directory))
            (dolist (option '("false" "true" "unset"))
              (with-temp-file (expand-file-name ".editorconfig" directory)
                (insert "root = true\n[*.awk]\nposix = " option "\n"))
              (when treesit-primary-parser
                (should (eq (treesit-parser-language treesit-primary-parser)
                            (car requested))))
              (awk-ts-mode)
              (should (eq (treesit-parser-language treesit-primary-parser)
                          (if (equal option "true") 'posix-awk 'gawk)))
              (should (equal (treesit-parser-list) (list treesit-primary-parser)))))
          (should (equal (reverse requested) '(gawk posix-awk gawk))))
      (fset 'treesit-ensure-installed ensure)
      (delete-directory directory t))))

;;;; Syntax

(ert-deftest awk-ts-mode-fences-complete-strings-and-regexps ()
  (dolist (literal '("\"a\\\"#b\"" "/a\\/#b/" "/[[:alpha:]]/"))
    (with-temp-buffer
      (insert "BEGIN { print " literal " } # comment\n")
      (awk-ts-mode-test--posix)
      (let ((start 15)
            (end (+ 15 (length literal))))
        (syntax-propertize (point-max))
        (should (= (syntax-class (syntax-after start)) 15))
        (should (= (syntax-class (syntax-after (1- end))) 15))
        (should (nth 3 (syntax-ppss (1+ start))))
        (should-not (nth 4 (syntax-ppss (1- end))))
        (should-not (nth 3 (syntax-ppss end)))
        (should (awk-ts-mode-test--comment-p "comment"))))))

(ert-deftest awk-ts-mode-refreshes-string-fences-after-edits-and-narrowing ()
  (with-temp-buffer
    (insert "BEGIN { print \"abc\" }\n")
    (let ((treesit-font-lock-level 4)) (awk-ts-mode-test--posix))
    (syntax-propertize (point-max))
    (goto-char 19)
    (delete-char 1)
    (syntax-propertize (point-max))
    (should (= (syntax-class (syntax-after 15)) 1))
    (insert "\"")
    (awk-ts-mode-test--should-match-fresh-buffer 4)
    (narrow-to-region 16 19)
    (syntax-propertize (point-max))
    (widen)
    (should (nth 3 (syntax-ppss 17)))
    (should-not (nth 3 (syntax-ppss 20)))))

;;;; Electric Pair

(ert-deftest awk-ts-mode-restricts-pair-newlines-to-cst-contexts ()
  (let ((electric-pair-open-newline-between-pairs t))
    (pcase-dolist (`(,before ,after ,expected)
                   '(("BEGIN {" "}" "BEGIN {\n\n}")
                     ("function f(" ") {}" "function f(\n) {}")
                     ("# {" "}" "# {\n}")
                     ("BEGIN { print \"{" "}\" }" "BEGIN { print \"{\n}\" }")
                     ("BEGIN { print /(" ")/ }" "BEGIN { print /(\n)/ }")))
      (ert-info ((format "%S / %S" before after))
        (with-temp-buffer
          (insert before after)
          (awk-ts-mode-test--posix)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length before)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         expected)))))))

(ert-deftest awk-ts-mode-gnu-pair-newlines-respect-the-language ()
  (dolist (source '("BEGIN {\n}" "BEGIN {f(\n)}" "BEGIN {a[\n]}"
                    "function f(\n) {}" "BEGIN {switch(\n) {}}"))
    (with-temp-buffer
      (insert source)
      (awk-ts-mode)
      (goto-char (point-min))
      (search-forward "\n")
      (should (eq (not (null (awk-ts-mode-electric-pair--newline-context-p)))
                  (equal source "BEGIN {\n}"))))))

;;;; Navigation

(ert-deftest awk-ts-mode-navigates-structures ()
  (with-temp-buffer
    (let ((begin "BEGIN { print 1 }")
          (rule "value { print value }")
          (first "function first(value) { return value }")
          (second "function second (value) { return value }")
          (end "END { print 2 }"))
      (insert begin "\n" rule "\n" first "\n" second "\n" end "\n")
      (awk-ts-mode-test--posix)
      (goto-char (point-min))
      (dolist (line (list begin rule first second end))
        (forward-sexp)
        (should (= (point)
                   (+ (awk-ts-mode-test--position line line)
                      (length line)))))
      (goto-char (point-max))
      (beginning-of-defun)
      (should (looking-at-p "function second"))
      (beginning-of-defun)
      (should (looking-at-p "function first"))
      (end-of-defun)
      (should (eq (char-before) ?\n))
      (should (eq (char-before (1- (point))) ?})))))

(ert-deftest awk-ts-mode-navigates-nested-boundaries-and-edited-expressions ()
  (with-temp-buffer
    (insert "BEGIN { print a + b * c, tail; }\n")
    (awk-ts-mode-test--posix)
    (goto-char (awk-ts-mode-test--position "b * c"))
    (forward-sexp)
    (should (= (point) (+ (awk-ts-mode-test--position "a + b * c") 9)))
    (backward-sexp)
    (should (= (point) (awk-ts-mode-test--position "a + b * c")))
    (delete-region (point) (+ (point) 9))
    (insert "array[index]")
    (backward-sexp)
    (should (= (point) (awk-ts-mode-test--position "array[index]")))
    (forward-sexp)
    (should (= (point) (+ (awk-ts-mode-test--position "array[index]") 12)))))

(ert-deftest awk-ts-mode-gnu-syntax-comments-and-navigation-use-cst ()
  (with-temp-buffer
    (insert "@include _\"#file\"\nfunction lib::f(x) {return /a[#]/}\nBEGIN {print \"#text\"} # comment\n")
    (awk-ts-mode)
    (syntax-propertize (point-max))
    (dolist (text '("#file" "#text" "[#]"))
      (goto-char (point-min)) (search-forward text)
      (should-not (nth 4 (syntax-ppss (1- (point)))))))
  (with-temp-buffer
    (insert "function lib::f(x) {return x}\nfunction g() {}\n")
    (awk-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Function" (treesit-simple-imenu)))) '("lib::f" "g")))
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at "function g"))
    (beginning-of-defun)
    (should (looking-at "function lib"))))

;;;; Imenu

(ert-deftest awk-ts-mode-indexes-definitions ()
  (with-temp-buffer
    (insert "BEGIN { print 1 }\n"
            "function first(value) { return value }\n"
            "value { print value }\n"
            "function second (value) { return value }\n"
            "END { print 2 }\n")
    (awk-ts-mode-test--posix)
    (let ((index (funcall imenu-create-index-function)))
      (should (equal (mapcar #'car index) '("Function")))
      (setq index (cdr (assoc "Function" index)))
      (should (equal (mapcar #'car index) '("first" "second")))
      (should
       (equal
        (mapcar (lambda (entry) (marker-position (cdr entry))) index)
        (list
         (awk-ts-mode-test--position "function" "function first(value) { return value }")
         (awk-ts-mode-test--position "function" "function second (value) { return value }")))))
    (let ((function (treesit-thing-next (point-min) 'defun))
          (root (treesit-buffer-root-node 'posix-awk)))
      (should (equal (treesit-defun-name function) "first"))
      (should-not (treesit-defun-name root)))))

;;;; Indentation

(ert-deftest awk-ts-mode-indents-consecutive-returns ()
  (pcase-dolist (`(,prefix ,suffix ,column)
                 '(("BEGIN {\n  print 1" "\n}" 2)
                   ("BEGIN {\n  if (ready) {\n    print 1" "\n  }\n}" 4)
                   ("BEGIN {\n  if (ready) {\n    print 1\n  }" "\n}" 2)
                   ("BEGIN {" "\n}" 2)
                   ("BEGIN {\n  if (a)\n    print a" "\n  print b\n}" 2)
                   ("BEGIN {\n  if (a)" "\n    print a\n}" 4)
                   ("BEGIN {\n  if (a)\n    print a\n  else" "\n    print b\n}" 4)
                   ("BEGIN {\n  if (a)\n    if (b)\n      print a" "\n  print b\n}" 2)
                   ("BEGIN {\n  while (a)\n    print a" "\n  print b\n}" 2)
                   ("BEGIN {\n  print 1," "\n    2\n}" 4)
                   ("BEGIN {\n  x = f(1," "\n    2)\n}" 4)
                   ("BEGIN {\n  x = a &&" "\n    b\n}" 4)
                   ("BEGIN {}" "" 0)))
    (ert-info ((format "%S / %S" prefix suffix))
      (with-temp-buffer
        (insert prefix suffix)
        (awk-ts-mode-test--posix)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (goto-char (1+ (length prefix)))
        (dotimes (_ 3)
          (call-interactively (key-binding (kbd "RET")))
          (should (= (current-column) column)))))))

(ert-deftest awk-ts-mode-indents-structures ()
  (pcase-dolist (`(,source ,offset ,expected)
                 '(("BEGIN {\nprint 1\nif (ready)\nprint 2\nelse\nprint 3\n}\n" 2 "BEGIN {\n  print 1\n  if (ready)\n    print 2\n  else\n    print 3\n}\n")
                   ("BEGIN {\nprint 1\n}\n" 4 "BEGIN {\n    print 1\n}\n")
                   ("BEGIN {\ndo\nprint\n        while (0)\n}\n" 2 "BEGIN {\n  do\n    print\n  while (0)\n}\n")))
    (ert-info ((format "Offset %s: %S" offset source))
      (should (equal (awk-ts-mode-test--indent source offset) expected)))))

(ert-deftest awk-ts-mode-gnu-indents-control-structures-and-preserves-literal-lines ()
  (pcase-dolist (`(,source ,expected)
                 '(("BEGIN {\nswitch(x) {\ncase 1:\nprint x;\ndefault:\nprint 0;\n}\n}\n"
                    "BEGIN {\n  switch(x) {\n    case 1:\n      print x;\n    default:\n      print 0;\n  }\n}\n")
                   ("function f(x,\ny) {\nif(x)\nprint x;\nelse\nprint y;\n}\n"
                    "function f(x,\n  y) {\n    if(x)\n      print x;\n    else\n      print y;\n  }\n")
                   ("BEGIN {\nprint \"a\\\n   b\";\nx = @/a\\\n c/\n}\n"
                    "BEGIN {\n  print \"a\\\n   b\";\n  x = @/a\\\n c/\n}\n")))
    (with-temp-buffer
      (insert source)
      (awk-ts-mode)
      (setq-local indent-tabs-mode nil)
      (dotimes (_ 2)
        (indent-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max)) expected))))))

(ert-deftest awk-ts-mode-gnu-indents-enter-in-bodies-headers-and-continuations ()
  (pcase-dolist (`(,prefix ,suffix ,column)
                 '(("BEGIN {" "\n}" 2)
                   ("BEGIN {\n  if(a)" "\n    print a\n}" 4)
                   ("BEGIN {\n  if(a)\n    print a" "\n  print b\n}" 2)
                   ("BEGIN {\n  if(a)\n    print a\n  else" "\n    print b\n}" 4)
                   ("BEGIN {\n  x = f(1," "\n    2)\n}" 4)
                   ("BEGIN {\n  switch(x) {\n    case 1:" "\n      print x;\n  }\n}" 6)))
    (with-temp-buffer
      (insert prefix suffix)
      (awk-ts-mode)
      (setq-local indent-tabs-mode nil)
      (electric-indent-local-mode 1)
      (goto-char (1+ (length prefix)))
      (dotimes (_ 3)
        (call-interactively (key-binding (kbd "RET")))
        (ert-info ((format "%S" prefix))
          (should (= (current-column) column)))))))

;;;; Updates

(ert-deftest awk-ts-mode-reparses-old-recovery-even-for-distant-edits ()
  (pcase-dolist (`(,language ,file ,properties)
                 '((gawk "test.awk" "")
                   (posix-awk "test.awk" "posix = true\n")))
    (let ((treesit-font-lock-level 4))
      (awk-ts-mode-test--with-file file properties "function f(x) {return (x}\nfunction g() {}\n"
        (awk-ts-mode-test--buffer-state)
        (let* ((parser treesit-primary-parser)
               (notifiers (treesit-parser-notifiers parser)))
          (should (treesit-node-check (treesit-parser-root-node parser) 'has-error))
          (treesit-parser-set-embed-level parser 3)
          (goto-char (point-min))
          (search-forward "function g")
          (save-restriction
            (narrow-to-region (line-beginning-position) (line-end-position))
            (insert "x"))
          (should-not (eq parser treesit-primary-parser))
          (should (eq (treesit-parser-language treesit-primary-parser) language))
          (should (= (treesit-parser-embed-level treesit-primary-parser) 3))
          (should (equal (treesit-parser-notifiers treesit-primary-parser) notifiers))
          (should (equal (treesit-parser-list) (list treesit-primary-parser))))
        (goto-char (point-min))
        (search-forward "x}")
        (backward-char 1)
        (insert ")")
        (should-not (treesit-node-check
                     (treesit-parser-root-node treesit-primary-parser) 'has-error))
        (awk-ts-mode-test--should-match-fresh-buffer 4 t)))))

(ert-deftest awk-ts-mode-refreshes-edited-owners-without-parser-notifications ()
  (pcase-dolist (`(,language ,file ,properties ,source ,fragment ,owner-start)
                 '((gawk "test.awk" "" "function f(x) {return x}\n" "x" 1)
                   (posix-awk "test.awk" "posix = true\n" "function f(x) {return x}\n" "x" 1)))
    (dolist (narrow '(nil t))
      (ert-info ((format "%s, narrowed: %s" language narrow))
        (let ((treesit-font-lock-level 4))
          (awk-ts-mode-test--with-file file properties source
            (rename-buffer "awk-ts-mode-test" t)
            (let ((noninteractive nil)) (font-lock-mode 1))
            (awk-ts-mode-test--buffer-state)
            (should (get-text-property owner-start 'fontified))
            (let ((parser treesit-primary-parser))
              (should-not (treesit-node-check (treesit-parser-root-node parser) 'has-error))
              (dolist (notifier (treesit-parser-notifiers parser))
                (treesit-parser-remove-notifier parser notifier))
              (goto-char (awk-ts-mode-test--position fragment))
              (when narrow
                (narrow-to-region (line-beginning-position) (line-end-position)))
              (insert "x")
              (widen)
              (should (eq parser treesit-primary-parser))
              (should-not (get-text-property owner-start 'fontified))
              (awk-ts-mode-test--should-match-fresh-buffer 4 t))))))))

(ert-deftest awk-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (pcase-dolist (`(,language ,file ,properties ,prefix ,body ,fragment ,face)
                 '((gawk "test.awk" "" "BEGIN {}\n" "# note\n" "note" font-lock-comment-face)
                   (posix-awk "test.awk" "posix = true\n" "BEGIN {}\n" "# note\n" "note" font-lock-comment-face)))
    (dolist (narrow '(nil t))
      (ert-info ((format "%s, narrowed: %s" language narrow))
        (let ((treesit-font-lock-level 4))
          (awk-ts-mode-test--with-file file properties (concat prefix "\ufeff" body)
            (awk-ts-mode-test--buffer-state)
            (when narrow (narrow-to-region 1 (+ 2 (length prefix))))
            (delete-region 1 (1+ (length prefix)))
            (widen)
            (should (eq (treesit-parser-language treesit-primary-parser) language))
            (should (equal (treesit-parser-list) (list treesit-primary-parser)))
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           (concat "\ufeff" body)))
            (awk-ts-mode-test--buffer-state)
            (should-not (get-text-property 1 'face))
            (goto-char (point-min))
            (search-forward fragment)
            (should (eq (get-text-property (1- (point)) 'face) face))
            (awk-ts-mode-test--should-match-fresh-buffer 4 t)))))))

(ert-deftest awk-ts-mode-refreshes-syntax-before-multiline-errors ()
  (dolist (narrow '(nil t))
    (with-temp-buffer
      (insert "function f(x) {\n  print x\n}\n")
      (let ((treesit-font-lock-level 4)) (awk-ts-mode-test--posix))
      (syntax-propertize (point-max))
      (should (= (syntax-class (syntax-after 11)) 4))
      (goto-char (point-min))
      (search-forward "}")
      (backward-char 1)
      (let ((position (point)) (removed (char-after)))
        (save-restriction
          (when narrow (narrow-to-region (line-beginning-position) (point-max)))
          (delete-char 1)
          (syntax-propertize (point-max)))
        (awk-ts-mode-test--should-match-fresh-buffer 4)
        (goto-char position)
        (insert removed)
        (awk-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 11)) 4))))))

(ert-deftest awk-ts-mode-updates-like-fresh-buffer ()
  (pcase-dolist (`(,source ,old ,new ,fragment ,face)
                 '(("BEGIN { print 1.0 f }\n" "1.0 f" "1.0f" "f" font-lock-number-face)
                   ("function f(value { return value }\n" "value {" "value) {" "f(" font-lock-function-name-face)
                   ("BEGIN { print /[[:alpha }\n" "/[[:alpha" "/[[:alpha:]]/" "alpha" font-lock-constant-face)))
    (ert-info ((format "%S: %S -> %S" source old new))
      (with-temp-buffer

        (insert source)
        (let ((treesit-font-lock-level 4)) (awk-ts-mode-test--posix))
        (awk-ts-mode-test--buffer-state)
        (goto-char (awk-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (awk-ts-mode-test--face fragment) face))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-reclassifies-calls-after-continuation-edits ()
  (with-temp-buffer
    (awk-ts-mode-test--fontify 4 '("BEGIN { target (value) }"))
    (dolist (gap '("\\\n" "" " " "\\\n"))
      (goto-char (point-min))
      (search-forward "target")
      (let ((start (point)))
        (search-forward "(")
        (delete-region start (1- (point)))
        (goto-char start)
        (insert gap)
        (font-lock-flush)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                        'has-error))
        (should (eq (get-text-property (1- start) 'face)
                    (if (equal gap "")
                        'font-lock-function-call-face
                      'font-lock-variable-use-face)))
        (dotimes (offset (length gap))
          (should (eq (get-text-property (+ start offset) 'face)
                      (when (and (equal gap "\\\n") (= offset 0))
                        'font-lock-punctuation-face))))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-preserves-collating-content-after-escape-edits ()
  (dolist (marker '("." "="))
    (with-temp-buffer
      (awk-ts-mode-test--fontify
       4 (list (concat "/[[" marker "aé日" marker "]]/")))
      (dolist (escape '("\\n" "" "\\n"))
        (goto-char 6)
        (when (eq (char-after) ?\\) (delete-char 2))
        (insert escape)
        (font-lock-flush)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                        'has-error))
        (should (eq (get-text-property 5 'face) 'font-lock-constant-face))
        (unless (equal escape "")
          (should-not (text-property-not-all 6 8 'face 'font-lock-escape-face)))
        (should-not (text-property-not-all (+ 6 (length escape))
                                           (+ 8 (length escape))
                                           'face 'font-lock-constant-face))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-gnu-edits-reclassify-faces-and-syntax-like-fresh-buffers ()
  (pcase-dolist (`(,source ,before ,after)
                 '(("@namespace \"awk\"\nBEGIN {print systime (3)}" "awk" "lib")
                   ("function f(systime) {return systime (3)}" "systime" "other")
                   ("BEGIN {print (a)}" "(a)" "(a)+b")
                   ("BEGIN {print /a\\x2b/}" "2b" "28b\\x29")
                   ("@include \"#file\"\nBEGIN {}" "file\"" "file")
                   ("BEGIN {print \"unfinished}" "unfinished}" "finished\"}")))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (awk-ts-mode))
      (awk-ts-mode-test--buffer-state)
      (goto-char (point-min))
      (search-forward before)
      (replace-match after t t)
      (awk-ts-mode-test--should-match-fresh-buffer 4))))

(ert-deftest awk-ts-mode-gnu-refreshes-distant-context-without-change-notifications ()
  (with-temp-buffer
    (rename-buffer "awk-ts-mode-gnu-test" t)
    (insert "function f(systime) {\n  return systime (3)\n}\n")
    (awk-ts-mode)
    (let ((noninteractive nil)) (font-lock-mode 1))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "return ")
    (let ((body (copy-marker (point)))
          (parser treesit-primary-parser))
      (unwind-protect
          (progn
            (should (eq (get-text-property body 'face) 'font-lock-variable-use-face))
            (should (get-text-property body 'fontified))
            (dolist (notifier (treesit-parser-notifiers parser))
              (treesit-parser-remove-notifier parser notifier))
            (goto-char (point-min))
            (search-forward "f(")
            (insert "x")
            (should (eq parser treesit-primary-parser))
            (should-not (get-text-property body 'fontified))
            (font-lock-ensure)
            (should (eq (get-text-property body 'face) 'font-lock-builtin-face))
            (awk-ts-mode-test--should-match-fresh-buffer treesit-font-lock-level))
        (set-marker body nil)))))

(provide 'awk-ts-mode-test)

;;; awk-ts-mode-test.el ends here
