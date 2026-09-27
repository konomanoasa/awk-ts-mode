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
(require 'loaddefs-gen)
(require 'newcomment)
(require 'awk-ts-mode)

(dolist (language '(posix-awk))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defun awk-ts-mode-test--position (fragment &optional line)
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
  (get-text-property (+ (awk-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun awk-ts-mode-test--comment-p (fragment &optional offset line)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss (+ (awk-ts-mode-test--position fragment line)
                         (or offset 0)))))

(defun awk-ts-mode-test--syntax-class (fragment &optional offset line)
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (awk-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun awk-ts-mode-test--should-have-faces (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (ert-info ((format "%S: %S" line fragment))
      (should (eq (awk-ts-mode-test--face fragment nil line) face)))))

(defun awk-ts-mode-test--fontify (level lines)
  (let ((treesit-font-lock-level level))
    (insert (mapconcat #'identity lines "\n"))
    (awk-ts-mode)
    (font-lock-ensure)))

(defun awk-ts-mode-test--should-have-next-faces (cases)
  (pcase-dolist (`(,fragment ,face) cases)
    (ert-info ((format "%S" fragment))
      (search-forward fragment)
      (should (eq (get-text-property (1- (point)) 'face) face)))))

(defun awk-ts-mode-test--indent (source &optional offset)
  (with-temp-buffer
    (insert source)
    (awk-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local awk-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun awk-ts-mode-test--buffer-state ()
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun awk-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (awk-ts-mode-test--buffer-state))
        (file buffer-file-name))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (awk-ts-mode))
      (should (equal state (awk-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest awk-ts-mode-respects-grammar-sources ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)) received)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (language)
                  (setq received (assq language treesit-language-source-alist))
                  t))
          (dolist (source awk-ts-mode--grammar-sources)
            (let* ((language (car source))
                   (custom (list language "/local/grammar" :revision "custom")))
              (dolist (configured (list nil (list custom)))
                (let ((treesit-language-source-alist configured))
                  (should (awk-ts-mode--ensure-grammar language))
                  (should (equal received (if configured custom source)))
                  (should (eq treesit-language-source-alist configured)))))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest awk-ts-mode-reports-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_language) nil))
          (with-temp-buffer
            (let ((buffer-file-name nil))
              (should-error (awk-ts-mode) :type 'user-error)
              (should-not (treesit-parser-list)))))
      (fset 'treesit-ensure-installed ensure))))

(ert-deftest awk-ts-mode-starts-and-reuses-parser ()
  (with-temp-buffer
    (insert "BEGIN { print 1 }\n")
    (awk-ts-mode)
    (should (eq major-mode 'awk-ts-mode))
    (should (eq (treesit-parser-language treesit-primary-parser) 'posix-awk))
    (should (equal (treesit-node-type (treesit-parser-root-node treesit-primary-parser))
                   "program"))
    (awk-ts-mode)
    (should (equal (treesit-parser-list) (list treesit-primary-parser)))))

;;;; Mode Selection

(ert-deftest awk-ts-mode-selects-files ()
  (let (patterns)
    (dolist (entry auto-mode-alist)
      (when (eq (cdr entry) 'awk-ts-mode)
        (push (car entry) patterns)))
    (should (equal (nreverse patterns) '("\\.awk\\'"))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.awk")
    (set-auto-mode)
    (should (eq major-mode 'awk-ts-mode))))

(ert-deftest awk-ts-mode-selects-interpreters ()
  (should (equal (alist-get "awk" interpreter-mode-alist nil nil #'equal)
                 'awk-ts-mode))
  (dolist (shebang '("#!/usr/bin/awk -f\n"
                     "#!/usr/bin/env awk -f\n"
                     "#!/usr/bin/env -S awk -f\n"))
    (with-temp-buffer
      (setq buffer-file-name "/tmp/example")
      (insert shebang "{ print }\n")
      (set-auto-mode)
      (should (eq major-mode 'awk-ts-mode)))))

(ert-deftest awk-ts-mode-generates-autoloads ()
  (let ((output (make-temp-file "awk-ts-mode-loaddefs-"))
        (directory (file-name-directory (locate-library "awk-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'awk-ts-mode" "(add-to-list 'auto-mode-alist" "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

;;;; Syntax

(ert-deftest awk-ts-mode-classifies-delimiters ()
  (dolist (source '("function f(x) { return x }\n"
                    "BEGIN { if (x) print x\n}\n"
                    "BEGIN { if (x) print x }\n"
                    "BEGIN { while (x) x-- }\n"
                    "BEGIN { for (i = 0; i < 3; i++) print i }\n"
                    "BEGIN { for (i in a) print a[i] }\n"
                    "BEGIN { do x--; while (x) }\n"
                    "BEGIN { print (x, y); printf(x, y) }\n"
                    "BEGIN { x = f((y)); print length(y), (x) }\n"
                    "BEGIN { x = ((x, y) in a); delete a[x, y] }\n"))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (awk-ts-mode))
      (font-lock-ensure)
      (should-not (treesit-node-check
                   (treesit-parser-root-node treesit-primary-parser) 'has-error))
      (goto-char (point-min))
      (while (re-search-forward "[][(){}]" nil t)
        (let ((position (1- (point))))
          (ert-info ((format "%S at %d" source position))
            (should (= (syntax-class (syntax-after position))
                       (if (memq (char-after position) '(?\( ?\[ ?{)) 4 5)))
            (should (eq (get-text-property position 'face)
                        'font-lock-bracket-face)))))))
  (with-temp-buffer
    (let ((header "function f(a) {")
          (statement "  if ((a[1])) print f(a)")
          (regexp "  if (/([b]){2}/) print \"()[]{}\"")
          (closing "}"))
      (insert header "\n" statement "\n" regexp "\n" closing "\n")
      (awk-ts-mode)
      (dolist (pair '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})))
        (should (eq (matching-paren (car pair)) (cdr pair))))
      (dolist (expectation
               `((,header "f(" 1 4)
                 (,header "a)" 1 5)
                 (,header "{" 0 4)
                 (,statement "if (" 3 4)
                 (,statement "(a[" 0 4)
                 (,statement "a[" 1 4)
                 (,statement "1]" 1 5)
                 (,statement "a[1])" 4 5)
                 (,statement "]))" 2 5)
                 (,statement "f(" 1 4)
                 (,statement "a)" 1 5)
                 (,regexp "if (" 3 4)
                 (,regexp "/)" 1 5)
                 (,closing "}" 0 5)))
        (pcase-let ((`(,line ,fragment ,offset ,class) expectation))
          (should
           (= (awk-ts-mode-test--syntax-class fragment offset line)
              class))))
      (dolist (expectation
               `((,regexp "/(" 1)
                 (,regexp "([" 0)
                 (,regexp "[b" 0)
                 (,regexp "b]" 1)
                 (,regexp "])" 1)
                 (,regexp "{2" 0)
                 (,regexp "2}" 1)
                 (,regexp "()" 0)
                 (,regexp "()" 1)
                 (,regexp "[]" 0)
                 (,regexp "[]" 1)
                 (,regexp "{}" 0)
                 (,regexp "{}" 1)))
        (pcase-let ((`(,line ,fragment ,offset) expectation))
          (should
           (= (awk-ts-mode-test--syntax-class fragment offset line)
              1)))))))

(ert-deftest awk-ts-mode-classifies-comments ()
  (with-temp-buffer
    (let ((top "# top")
          (string "BEGIN { print \"# string\" }")
          (regexp "/#regexp/ { print } # trailing")
          (continued "# backslash \\")
          (next "BEGIN { print 2 }"))
      (insert (mapconcat #'identity
                         (list top string regexp continued next)
                         "\n")
              "\n")
      (awk-ts-mode)
      (should (awk-ts-mode-test--comment-p "top" nil top))
      (should-not (awk-ts-mode-test--comment-p "string" nil string))
      (should-not (awk-ts-mode-test--comment-p "regexp" nil regexp))
      (should (awk-ts-mode-test--comment-p "trailing" nil regexp))
      (should (awk-ts-mode-test--comment-p "backslash" nil continued))
      (should-not (awk-ts-mode-test--comment-p "print" nil next)))))

(ert-deftest awk-ts-mode-fences-complete-strings-and-regexps ()
  (dolist (literal '("\"a\\\"#b\"" "/a\\/#b/" "/[[:alpha:]]/"))
    (with-temp-buffer
      (insert "BEGIN { print " literal " } # comment\n")
      (awk-ts-mode)
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
    (let ((treesit-font-lock-level 4)) (awk-ts-mode))
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

(ert-deftest awk-ts-mode-keeps-comments-open-through-buffer-end ()
  (dolist (source '("# note" "#"))
    (with-temp-buffer
      (insert source)
      (awk-ts-mode)
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss (point-max)))))))

;;;; Comment Commands

(ert-deftest awk-ts-mode-comments-and-uncomments ()
  (with-temp-buffer
    (insert "BEGIN { print 1 }\n")
    (awk-ts-mode)
    (comment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "# BEGIN { print 1 }\n"))
    (uncomment-region (point-min) (point-max))
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "BEGIN { print 1 }\n"))))

;;;; Electric Pair

(ert-deftest awk-ts-mode-supplies-electric-pairs ()
  (let ((electric-pair-pairs '((?% . ?%)))
        (electric-pair-mode nil))
    (pcase-dolist (`(,prefix ,opening ,expected)
                   '(("BEGIN " ?{ "BEGIN {}")
                     ("BEGIN { print " ?\( "BEGIN { print ()")
                     ("BEGIN { print a" ?\[ "BEGIN { print a[]")))
      (ert-info ((format "%S / %c" prefix opening))
        (with-temp-buffer
          (insert prefix)
          (awk-ts-mode)
          (should-not electric-pair-mode)
          (should (local-variable-p 'electric-pair-pairs))
          (should (equal (assq ?% electric-pair-pairs) '(?% . ?%)))
          (electric-pair-local-mode 1)
          (let ((last-command-event opening)) (self-insert-command 1))
          (should (equal (buffer-string) expected))
          (should (= (point) (1- (point-max)))))))
    (should (equal electric-pair-pairs '((?% . ?%)))))
  (let ((electric-pair-pairs '((?{ . ?>))))
    (pcase-dolist (`(,prefix ,opening ,expected)
                   '(("BEGIN " ?{ "BEGIN {>")))
      (with-temp-buffer
        (insert prefix)
        (awk-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event opening)) (self-insert-command 1))
        (should (equal (buffer-string) expected))
        (should (= (point) (1- (point-max))))))
    (should (equal electric-pair-pairs '((?{ . ?>))))))

(ert-deftest awk-ts-mode-respects-pair-newline-preferences ()
  (let ((calls 0))
    (pcase-dolist (`(,setting ,expected)
                   (list (list nil "BEGIN {\n}")
                         (list t "BEGIN {\n\n}")
                         (list (lambda () (setq calls (1+ calls)) nil) "BEGIN {\n}")
                         (list (lambda () (setq calls (1+ calls)) t) "BEGIN {\n\n}")))
      (setq calls 0)
      (let ((electric-pair-open-newline-between-pairs setting))
        (with-temp-buffer
          (insert "BEGIN {}")
          (awk-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char 8)
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected)))
        (should (eq (> calls 0) (functionp setting)))
        (should (eq electric-pair-open-newline-between-pairs setting))))))

(ert-deftest awk-ts-mode-pairs-delimiters-and-indents-on-return ()
  (pcase-dolist (`(,prefix ,suffix ,offset ,expand ,expected ,column)
                 '(("function func() " "" 2 t "function func() {\n  \n}" 2)
                   ("BEGIN " "" 4 t "BEGIN {\n    \n}" 4)
                   ("BEGIN {\n  if (ready) " "\n}" 2 t
                    "BEGIN {\n  if (ready) {\n    \n  }\n}" 4)
                   ("function func() " "" 2 nil "function func() {\n}" 0)))
    (ert-info ((format "Expand %S, offset %s: %S" expand offset prefix))
      (let ((electric-pair-open-newline-between-pairs expand))
        (with-temp-buffer
          (insert prefix suffix)
          (awk-ts-mode)
          (setq-local indent-tabs-mode nil)
          (setq-local awk-ts-mode-indent-offset offset)
          (electric-indent-local-mode 1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length prefix)))
          (let ((last-command-event ?{))
            (self-insert-command 1))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-string) expected))
          (should (= (current-column) column))
          (when expand
            (should (eolp))
            (should (= (line-number-at-pos) (1+ (length (split-string prefix "\n")))))))))))

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
          (awk-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length before)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         expected)))))))

(ert-deftest awk-ts-mode-pairs-language-quotes ()
  (let ((electric-pair-pairs nil)
        (electric-pair-text-pairs nil))
    (pcase-dolist (`(,prefix ,quote ,opened ,closed)
                   '(("BEGIN { print " ?\" "BEGIN { print \"\"" "BEGIN { print \"x\"")))
      (with-temp-buffer
        (insert prefix)
        (awk-ts-mode)
        (electric-pair-local-mode 1)
        (let ((last-command-event quote)) (self-insert-command 1))
        (should (equal (buffer-string) opened))
        (should (= (point) (1- (point-max))))
        (let ((last-command-event ?x)) (self-insert-command 1))
        (let ((last-command-event quote)) (self-insert-command 1))
        (should (equal (buffer-string) closed))
        (should (eobp))))
    (should-not electric-pair-pairs)))

;;;; Font Lock

(ert-deftest awk-ts-mode-fontifies-by-level ()
  (dolist (level '(1 2 3 4))
    (with-temp-buffer
      (insert "# note\nBEGIN { print \"a\\tb\", 1, /c\\n/ }\n")
      (let ((treesit-font-lock-level level)) (awk-ts-mode))
      (font-lock-ensure)
      (pcase-dolist (`(,fragment ,minimum ,face)
                     '(("note" 1 font-lock-comment-face) ("BEGIN" 2 font-lock-keyword-face)
                       ("a\\tb" 2 font-lock-string-face) ("1" 3 font-lock-number-face)
                       ("c" 4 font-lock-regexp-face) ("\\n" 4 font-lock-escape-face)
                       ("{" 4 font-lock-bracket-face)))
        (ert-info ((format "Level %s: %S" level fragment))
          (should (eq (awk-ts-mode-test--face fragment) (and (>= level minimum) face)))))
      (should (eq (awk-ts-mode-test--face "\\t")
                  (cond ((>= level 3) 'font-lock-escape-face)
                        ((= level 2) 'font-lock-string-face)))))))

(ert-deftest awk-ts-mode-fontifies-language-syntax ()
  (with-temp-buffer
    (let ((comment "# total values")
          (definition "function total(value, scale) {")
          (spaced "function half (value) { return value / 2 }")
          (calculation "  result = sqrt(value) + scale")
          (condition "  if (result >= 10) {")
          (statements "  first = 1; second = 2")
          (output "    printf \"%g\\n\", result")
          (call "BEGIN { total(4, 2) }")
          (division "BEGIN { value = 8 / 2 }")
          (range "NR == 1, NR == 3 { print }")
          (field "BEGIN { print $3, $name, $(1 + 1) }")
          (continuation "BEGIN { value = 1 + \\"))
      (awk-ts-mode-test--fontify
       4
       (list comment definition calculation condition statements output
             "  }" "  return result" "}" spaced call division range field
             continuation "2 }"))
      (awk-ts-mode-test--should-have-faces
       `((,comment "total" font-lock-comment-face)
         (,definition "total" font-lock-function-name-face)
         (,definition "value" font-lock-variable-name-face)
         (,definition "scale" font-lock-variable-name-face)
         (,definition "(" font-lock-bracket-face)
         (,definition "," font-lock-punctuation-face)
         (,definition "{" font-lock-bracket-face)
         (,calculation "result" font-lock-variable-use-face)
         (,calculation "=" font-lock-operator-face)
         (,calculation "sqrt" font-lock-builtin-face)
         (,calculation "value" font-lock-variable-use-face)
         (,calculation "+" font-lock-operator-face)
         (,condition "10" font-lock-number-face)
         (,output "\"" font-lock-string-face)
         (,output "%g" font-lock-string-face)
         (,output "\\n" font-lock-escape-face)
         (,output "," font-lock-punctuation-face)
         (,statements ";" font-lock-punctuation-face)
         (,spaced "half" font-lock-function-name-face)
         (,call "total" font-lock-function-call-face)
         (,call "4" font-lock-number-face)
         (,division "/" font-lock-operator-face)
         (,range "," font-lock-punctuation-face)
         (,field "$3" font-lock-operator-face)
         (,field "3" font-lock-number-face)
         (,field "$name" font-lock-operator-face)
         (,field "name" font-lock-variable-use-face)
         (,field "$(1 + 1)" font-lock-operator-face)
         (,continuation "\\" font-lock-punctuation-face))))))

(ert-deftest awk-ts-mode-fontifies-line-continuations-between-tokens ()
  (with-temp-buffer
    (awk-ts-mode-test--fontify
     4
     '("\\" "function total\\" "(value,\\" " scale)\\" "{"
       "  if (value)\\" "    return value\\" "      + scale"
       "}" "BEGIN { total\\" "(1); print \\" " 3 }"))
    (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                    'has-error))
    (awk-ts-mode-test--should-have-faces
     '(("function total\\" "total" font-lock-function-name-face)
       ("(value,\\" "value" font-lock-variable-name-face)
       (" scale)\\" "scale" font-lock-variable-name-face)
       ("  if (value)\\" "if" font-lock-keyword-face)
       ("    return value\\" "value" font-lock-variable-use-face)
       ("      + scale" "+" font-lock-operator-face)
       ("BEGIN { total\\" "total" font-lock-variable-use-face)))
    (goto-char (point-min))
    (while (search-forward "\\\n" nil t)
      (should (eq (get-text-property (- (point) 2) 'face)
                  'font-lock-punctuation-face))
      (should-not (get-text-property (1- (point)) 'face)))))

(ert-deftest awk-ts-mode-fontifies-every-posix-awk-keyword ()
  (with-temp-buffer
    (let ((header "function walk(items, key) {")
          (loop "  for (key in items) delete items[key]")
          (branch "  if (key) next; else nextfile")
          (repeat "  do break; while (key)")
          (tail "  return")
          (start "BEGIN { getline; print 1; printf \"%d\", 1; exit 0 }")
          (finish "END { while (0) continue }"))
      (awk-ts-mode-test--fontify
       4 (list header loop branch repeat tail "}" start finish))
      (pcase-dolist (`(,line . ,keyword)
                     `((,header . "function") (,loop . "for")
                       (,loop . "in") (,loop . "delete")
                       (,branch . "if") (,branch . "next")
                       (,branch . "else") (,branch . "nextfile")
                       (,repeat . "do") (,repeat . "break")
                       (,repeat . "while") (,tail . "return")
                       (,start . "BEGIN") (,start . "getline")
                       (,start . "print") (,start . "printf")
                       (,start . "exit") (,finish . "END")
                       (,finish . "continue")))
        (should (eq (awk-ts-mode-test--face keyword nil line)
                    'font-lock-keyword-face))))))

(ert-deftest awk-ts-mode-fontifies-every-posix-awk-named-operator ()
  (with-temp-buffer
    (let ((assignments "  a += 1; a -= 1; a *= 2; a /= 2; a %= 2; a ^= 2")
          (steps "  a++; a--")
          (comparisons "  if (a == 1 && a != 2 || a <= 3 && a >= 4) a = 5")
          (matches "  if (a !~ /x/) print a >> \"log\""))
      (awk-ts-mode-test--fontify
       4 (list "BEGIN {" assignments steps comparisons matches "}"))
      (pcase-dolist (`(,line . ,operator)
                     `((,assignments . "+=") (,assignments . "-=")
                       (,assignments . "*=") (,assignments . "/=")
                       (,assignments . "%=") (,assignments . "^=")
                       (,steps . "++") (,steps . "--")
                       (,comparisons . "==") (,comparisons . "&&")
                       (,comparisons . "!=") (,comparisons . "||")
                       (,comparisons . "<=") (,comparisons . ">=")
                       (,matches . "!~") (,matches . ">>")))
        (should (eq (awk-ts-mode-test--face operator nil line)
                    'font-lock-operator-face)))))
  (with-temp-buffer
    (awk-ts-mode-test--fontify 4 '("BEGIN { if (left && right) print }"))
    (goto-char (point-min))
    (search-forward "&&")
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-operator-face))))

(ert-deftest awk-ts-mode-fontifies-posix-ere-components ()
  (with-temp-buffer
    (let ((line "/^a\\.([[:alpha:]x-z]{2,3}|b+).*$/ { print }")
          (literal-meta "/[a^]/ { print }")
          (literal-dot "/[.a.]/ { print }")
          (literal-equals "/[=b=]/ { print }")
          (negated "/[^a[.b.]]/ { print }")
          (collating-meta "/[[.^.]]/ { print }")
          (digit-range "/[0-9]/ { print }")
          (bare "/[a-z0]/ { print }")
          (equivalence "/[[=c=]]/ { print }")
          (modifier "/x{2,3}?/ { print }"))
      (awk-ts-mode-test--fontify
       4 (list line literal-meta literal-dot literal-equals negated
               collating-meta equivalence digit-range bare modifier))
      (awk-ts-mode-test--should-have-faces
       `((,line "/" font-lock-delimiter-face)
         (,line "^" font-lock-operator-face)
         (,line "a" font-lock-regexp-face)
         (,line "\\." font-lock-escape-face)
         (,line "(" font-lock-bracket-face)
         (,line "[" font-lock-bracket-face)
         (,line "[:" font-lock-bracket-face)
         (,line ":]" font-lock-punctuation-face)
         (,line "alpha" font-lock-constant-face)
         (,line "x-z" font-lock-constant-face)
         (,line "-z" font-lock-operator-face)
         (,line "z]" font-lock-constant-face)
         (,line "]{" font-lock-bracket-face)
         (,line "{" font-lock-bracket-face)
         (,line "2,3" font-lock-number-face)
         (,line ",3" font-lock-punctuation-face)
         (,line "|" font-lock-operator-face)
         (,line "b+" font-lock-regexp-face)
         (,line "+)" font-lock-operator-face)
         (,line ")" font-lock-bracket-face)
         (,line ".*" font-lock-constant-face)
         (,line "*$" font-lock-operator-face)
         (,line "$" font-lock-operator-face)
         (,digit-range "0" font-lock-constant-face)
         (,digit-range "9" font-lock-constant-face)
         (,bare "a-z" font-lock-constant-face)
         (,bare "-z" font-lock-operator-face)
         (,bare "z0" font-lock-constant-face)
         (,bare "0]" font-lock-constant-face)
         (,literal-meta "^]" font-lock-constant-face)
         (,literal-dot "a.]" font-lock-constant-face)
         (,literal-equals "b=]" font-lock-constant-face)
         (,negated "^" font-lock-negation-char-face)
         (,negated "a[." font-lock-constant-face)
         (,negated "[." font-lock-bracket-face)
         (,negated ".]" font-lock-punctuation-face)
         (,negated "b" font-lock-constant-face)
         (,collating-meta "^" font-lock-constant-face)
         (,equivalence "[=" font-lock-bracket-face)
         (,equivalence "=]" font-lock-punctuation-face)
         (,equivalence "c" font-lock-constant-face)
         (,modifier "?/" font-lock-operator-face))))))

(ert-deftest awk-ts-mode-fontifies-hyphens-by-context ()
  (pcase-dolist (`(,source ,faces)
                 '(("BEGIN { value = -left - right }"
                    (("-" font-lock-operator-face) ("-" font-lock-operator-face)))
                   ("/[+--]/ { print }"
                    (("-" font-lock-operator-face) ("-" font-lock-constant-face)))
                   ("/[a--]/ { print }"
                    (("-" font-lock-operator-face) ("-" font-lock-constant-face)))
                   ("/[]--]/ { print }"
                    (("-" font-lock-operator-face) ("-" font-lock-constant-face)))
                   ("/[-]/ { print }" (("-" font-lock-constant-face)))
                   ("/[a-]/ { print }" (("-" font-lock-constant-face)))
                   ("/[+-]/ { print }" (("-" font-lock-constant-face)))
                   ("/[%--@]/ { print }"
                    (("%" font-lock-constant-face) ("-" font-lock-operator-face)
                     ("-" font-lock-constant-face) ("@" font-lock-constant-face)))))
    (ert-info ((format "%S" source))
      (with-temp-buffer
        (awk-ts-mode-test--fontify 4 (list source))
        (goto-char (point-min))
        (awk-ts-mode-test--should-have-next-faces faces)))))

(ert-deftest awk-ts-mode-fontifies-ere-content-and-escapes-separately ()
  (pcase-dolist (`(,opening ,closing ,content-face)
                 '(("/" "/" font-lock-regexp-face)
                   ("/[" "]/" font-lock-constant-face)
                   ("/[[." ".]]/" font-lock-constant-face)
                   ("/[[=" "=]]/" font-lock-constant-face)))
    (with-temp-buffer
      (awk-ts-mode-test--fontify
       4 (list (concat opening "a\\né\\141 \\e\\/日\\.\\q\\\\" closing)))
      (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                      'has-error))
      (let ((position (1+ (length opening))))
        (pcase-dolist (`(,text ,face)
                       `(("a" ,content-face)
                         ("\\n" font-lock-escape-face)
                         ("é" ,content-face)
                         ("\\141" font-lock-escape-face)
                         (" " ,content-face)
                         ("\\e" font-lock-escape-face)
                         ("\\/" font-lock-escape-face)
                         ("日" ,content-face)
                         ("\\." font-lock-escape-face)
                         ("\\q" font-lock-escape-face)
                         ("\\\\" font-lock-escape-face)))
          (ert-info ((format "%s…%s, %S" opening closing text))
            (should-not
             (text-property-not-all position (+ position (length text))
                                    'face face)))
          (setq position (+ position (length text))))))))

(ert-deftest awk-ts-mode-captures-only-leaves ()
  (dolist (source '("function f(x) { return x + 1 }\nBEGIN { print f(2), \"x\\n\" }\n# note\n" "/[^[.a.][=b=][:alpha:]]{2,3}/ { print }\n"
                    "/a)b}c\\/é/\n"
                    "/[][-a\\n日]/\n"
                    "\\\nfunction f\\\n(x,\\\n y)\\\n{ return x\\\n + y }\nBEGIN { f\\\n(1) }\n"
                    "/[[.a\\né\\141 \\e\\/日\\..]]/\n"
                    "/[[=a\\né\\141 \\e\\/日\\.=]]/\n"))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (awk-ts-mode))
      (let ((root (treesit-parser-root-node treesit-primary-parser)))
        (should-not (treesit-node-check root 'has-error))
        (dolist (setting treesit-font-lock-settings)
          (dolist (capture (treesit-query-capture root (car setting)))
            (ert-info ((treesit-node-type (cdr capture)))
              (should (= (treesit-node-child-count (cdr capture)) 0)))))))))

;;;; Navigation

(ert-deftest awk-ts-mode-navigates-structures ()
  (with-temp-buffer
    (let ((begin "BEGIN { print 1 }")
          (rule "value { print value }")
          (first "function first(value) { return value }")
          (second "function second (value) { return value }")
          (end "END { print 2 }"))
      (insert begin "\n" rule "\n" first "\n" second "\n" end "\n")
      (awk-ts-mode)
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

(ert-deftest awk-ts-mode-navigates-cst-editing-units ()
  (pcase-dolist (`(,prefix ,unit ,suffix)
                 '(("BEGIN { print " "value" ", tail; }\n")
                   ("BEGIN { print " "12.5" ", tail; }\n")
                   ("BEGIN { print " "\"two words\"" ", tail; }\n")
                   ("BEGIN { print " "/ab+/" ", tail; }\n")
                   ("BEGIN { print " "a + b * c" ", tail; }\n")
                   ("BEGIN { print " "a b" ", tail; }\n")
                   ("BEGIN { print " "a ? b : c" ", tail; }\n")
                   ("BEGIN { print " "-a" ", tail; }\n")
                   ("BEGIN { print " "f(a, b)" ", tail; }\n")
                   ("BEGIN { print " "length(a)" ", tail; }\n")
                   ("BEGIN { print " "(a + b)" ", tail; }\n")
                   ("BEGIN { print " "a[i, j]" ", tail; }\n")
                   ("BEGIN { print " "$2" ", tail; }\n")
                   ("BEGIN { print (" "getline x" "), tail; }\n")
                   ("BEGIN { " "print x;" " print y; }\n")
                   ("BEGIN { " "if (x) print y;" " print z; }\n")
                   ("BEGIN { " "{ print x; }" " print y; }\n")
                   ("BEGIN { for (" "i = 0" "; i < 3; i++) print i; }\n")
                   ("BEGIN { for (i = 0; i < 3; " "i++" ") print i; }\n")))
    (ert-info ((format "%S" unit))
      (with-temp-buffer
        (insert prefix unit suffix)
        (awk-ts-mode)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk) 'has-error))
        (let ((start (1+ (length prefix)))
              (end (1+ (+ (length prefix) (length unit)))))
          (goto-char start)
          (forward-sexp)
          (should (= (point) end))
          (backward-sexp)
          (should (= (point) start)))))))

(ert-deftest awk-ts-mode-excludes-sexp-wrappers-and-empty-statements ()
  (with-temp-buffer
    (insert "BEGIN { ; print a[1], value, \"text\"; }\n")
    (awk-ts-mode)
    (dolist (node (treesit-query-capture
                   (treesit-buffer-root-node 'posix-awk)
                   '([(expr) (print_expr) (expr_list) (string_content)
                      (non_unary_expr) (non_unary_print_expr) (lvalue)
                      (terminated_statement)] @candidate) nil nil t))
      (when (member (treesit-node-text node t)
                    '(";" "value" "text" "1"))
        (should-not (awk-ts-mode-navigation--sexp-p node))))))

(ert-deftest awk-ts-mode-navigates-nested-boundaries-and-edited-expressions ()
  (with-temp-buffer
    (insert "BEGIN { print a + b * c, tail; }\n")
    (awk-ts-mode)
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

;;;; Imenu

(ert-deftest awk-ts-mode-indexes-definitions ()
  (with-temp-buffer
    (insert "BEGIN { print 1 }\n"
            "function first(value) { return value }\n"
            "value { print value }\n"
            "function second (value) { return value }\n"
            "END { print 2 }\n")
    (awk-ts-mode)
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

(ert-deftest awk-ts-mode-indents-after-return ()
  (pcase-dolist (`(,source ,line ,offset ,expected)
                 '(("BEGIN {\n\n}" 0 2 "BEGIN {\n  \n\n}")
                   ("BEGIN {\n  print 1\n}" 1 2 "BEGIN {\n  print 1\n  \n}")
                   ("BEGIN {\n  if (ready) {\n\n  }\n}" 1 2 "BEGIN {\n  if (ready) {\n    \n\n  }\n}")
                   ("BEGIN {\n  if (ready) {\n    print 1\n  }\n}" 3 2 "BEGIN {\n  if (ready) {\n    print 1\n  }\n  \n}")
                   ("BEGIN {\n  {\n    print 1\n  }\n}" 3 2 "BEGIN {\n  {\n    print 1\n  }\n  \n}")
                   ("BEGIN {\n  if (ready) {\n    print 1\n  } # done\n}" 3 2 "BEGIN {\n  if (ready) {\n    print 1\n  } # done\n  \n}")
                   ("BEGIN {\n  if (ready) {\n    while (more) {\n      print 1\n    }\n  }\n}" 4 2 "BEGIN {\n  if (ready) {\n    while (more) {\n      print 1\n    }\n    \n  }\n}")
                   ("BEGIN {\n    if (ready) {\n        print 1\n    }\n}" 3 4 "BEGIN {\n    if (ready) {\n        print 1\n    }\n    \n}")
                   ("BEGIN {\n\n}" 0 4 "BEGIN {\n    \n\n}")
                   ("BEGIN {}" 0 2 "BEGIN {}\n")))
    (ert-info ((format "Return on line %s: %S" line source))
      (with-temp-buffer
        (insert source)
        (awk-ts-mode)
        (setq-local indent-tabs-mode nil)
        (setq-local awk-ts-mode-indent-offset offset)
        (electric-indent-local-mode 1)
        (goto-char (point-min))
        (forward-line line)
        (end-of-line)
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-string) expected))))))

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
        (awk-ts-mode)
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

;;;; Updates

(ert-deftest awk-ts-mode-refreshes-syntax-before-multiline-errors ()
  (dolist (narrow '(nil t))
    (with-temp-buffer
      (insert "function f(x) {\n  print x\n}\n")
      (let ((treesit-font-lock-level 4)) (awk-ts-mode))
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
        (should (= (syntax-class (syntax-after 11)) 1))
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
        (let ((treesit-font-lock-level 4)) (awk-ts-mode))
        (awk-ts-mode-test--buffer-state)
        (goto-char (awk-ts-mode-test--position old))
        (delete-char (length old))
        (insert new)
        (font-lock-ensure)
        (should (eq (awk-ts-mode-test--face fragment) face))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-reclassifies-delimiters-after-edits ()
  (with-temp-buffer
    (insert "BEGIN {\nprint\n}\n")
    (awk-ts-mode)
    (should (= (awk-ts-mode-test--syntax-class "}" nil "}") 5))
    (goto-char (point-min))
    (search-forward "{")
    (delete-char -1)
    (should (= (awk-ts-mode-test--syntax-class "}" nil "}") 1))
    (goto-char (point-min))
    (search-forward "BEGIN ")
    (insert "{")
    (should (= (awk-ts-mode-test--syntax-class "}" nil "}") 5))))

(ert-deftest awk-ts-mode-recomputes-comment-syntax-after-edits ()
  (with-temp-buffer
    (let ((commented "BEGIN { print x # note"))
      (insert commented "\n}\n")
      (awk-ts-mode)
      (should (awk-ts-mode-test--comment-p "note" nil commented))
      (goto-char (awk-ts-mode-test--position "#" commented))
      (let ((quote-position (point))
            (string "BEGIN { print x \"# note"))
        (insert "\"")
        (should-not (awk-ts-mode-test--comment-p "note" nil string))
        (delete-region quote-position (1+ quote-position))
        (should (awk-ts-mode-test--comment-p "note" nil commented))))))

(ert-deftest awk-ts-mode-preserves-syntax-when-narrowed ()
  (with-temp-buffer
    (insert "# first\n# second\n")
    (awk-ts-mode)
    (goto-char (point-min))
    (forward-line 1)
    (narrow-to-region (point) (point-max))
    (should (nth 4 (syntax-ppss (+ (point-min) 2))))
    (widen)
    (should (nth 4 (syntax-ppss (+ (point-min) 2))))))

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

(ert-deftest awk-ts-mode-rebuilds-imenu-after-edits ()
  (with-temp-buffer
    (insert "function first() { return 1 }\n")
    (awk-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Function"
                                             (funcall imenu-create-index-function))))
                   '("first")))
    (goto-char (point-max))
    (insert "function second() { return 2 }\n")
    (should (equal (mapcar #'car (cdr (assoc "Function"
                                             (funcall imenu-create-index-function))))
                   '("first" "second")))))

(provide 'awk-ts-mode-test)

;;; awk-ts-mode-test.el ends here
