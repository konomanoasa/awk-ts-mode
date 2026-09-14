;;; awk-ts-mode-test.el --- Tests for awk-ts-mode  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'awk-ts-mode)

;;;; Helpers

(defun awk-ts-mode-test--require-grammar ()
  (unless (treesit-ready-p 'posix-awk t)
    (ert-skip "The posix-awk grammar is unavailable")))

(defun awk-ts-mode-test--grammar-source (configured)
  (let ((treesit-language-source-alist configured)
        (ensure (symbol-function 'treesit-ensure-installed))
        source)
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed
                (lambda (installed-language)
                  (setq source
                        (assq installed-language
                              treesit-language-source-alist))
                  t))
          (should (awk-ts-mode--ensure-grammar 'posix-awk))
          source)
      (fset 'treesit-ensure-installed ensure))))

(defun awk-ts-mode-test--fontify (level lines)
  (let ((treesit-font-lock-level level))
    (insert (mapconcat #'identity lines "\n"))
    (awk-ts-mode)
    (font-lock-ensure)))

(defun awk-ts-mode-test--position-in-line (line fragment)
  (let (line-start)
    (save-excursion
      (goto-char (point-min))
      (while (and (not line-start) (not (eobp)))
        (let ((start (line-beginning-position))
              (end (line-end-position)))
          (if (equal line (buffer-substring-no-properties start end))
              (setq line-start start)
            (forward-line 1)))))
    (unless line-start
      (ert-fail (format "Test line not found: %s" line)))
    (let ((offset (string-search fragment line)))
      (unless offset
        (ert-fail (format "Fragment %s not found in test line: %s"
                          fragment line)))
      (+ line-start offset))))

(defun awk-ts-mode-test--face-in-line (line fragment &optional offset)
  (get-text-property
   (+ (awk-ts-mode-test--position-in-line line fragment)
      (or offset 0))
   'face))

(defun awk-ts-mode-test--comment-in-line-p (line fragment &optional offset)
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss
          (+ (awk-ts-mode-test--position-in-line line fragment)
             (or offset 0)))))

(defun awk-ts-mode-test--syntax-class-in-line
    (line fragment &optional offset)
  (syntax-propertize (point-max))
  (syntax-class
   (syntax-after
    (+ (awk-ts-mode-test--position-in-line line fragment)
       (or offset 0)))))

(defun awk-ts-mode-test--should-fontify (cases)
  (pcase-dolist (`(,line ,fragment ,face) cases)
    (should (equal (list line fragment
                         (awk-ts-mode-test--face-in-line line fragment))
                   (list line fragment face)))))

(defun awk-ts-mode-test--face-at-next (text)
  (search-forward text)
  (get-text-property (1- (point)) 'face))

(defun awk-ts-mode-test--should-have-next-faces (cases)
  (dolist (case cases)
    (should (eq (awk-ts-mode-test--face-at-next (car case))
                (nth 1 case)))))

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
  (syntax-propertize (point-max))
  (let ((position (point-min)) state)
    (while (< position (point-max))
      (push (list (get-text-property position 'face)
                  (syntax-after position))
            state)
      (setq position (1+ position)))
    (nreverse state)))

(defun awk-ts-mode-test--should-match-fresh-buffer (level)
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (awk-ts-mode-test--buffer-state)))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level level)) (awk-ts-mode))
      (font-lock-ensure)
      (should (equal state (awk-ts-mode-test--buffer-state))))))

;;;; Grammar

(ert-deftest awk-ts-mode-starts-a-posix-awk-parser ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "BEGIN { print \"hello\" }\n")
    (awk-ts-mode)
    (should (eq major-mode 'awk-ts-mode))
    (should
     (equal (treesit-node-type (treesit-buffer-root-node 'posix-awk))
            "program"))))

(ert-deftest awk-ts-mode-provides-a-default-grammar-source ()
  (should
   (equal
    (awk-ts-mode-test--grammar-source nil)
    '(posix-awk "https://github.com/konomanoasa/tree-sitter-posix-awk"
                :revision "v0.15.0"))))

(ert-deftest awk-ts-mode-preserves-a-user-grammar-source ()
  (let ((custom '(posix-awk . ("custom-source"))))
    (should (equal (awk-ts-mode-test--grammar-source (list custom))
                   custom))))

(ert-deftest awk-ts-mode-reports-an-unavailable-grammar ()
  (let ((ensure (symbol-function 'treesit-ensure-installed)))
    (unwind-protect
        (progn
          (fset 'treesit-ensure-installed (lambda (_) nil))
          (with-temp-buffer
            (let ((error-data (should-error (awk-ts-mode) :type 'user-error)))
              (should (string-match-p
                       "posix-awk" (error-message-string error-data)))
              (should-not (treesit-parser-list nil nil t)))))
      (fset 'treesit-ensure-installed ensure))))

;;;; Syntax

(ert-deftest awk-ts-mode-classifies-only-cst-delimiters-as-parens ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((header "function f(a) {")
          (statement "  if ((a[1])) print f(a)")
          (regexp "  if (/([b]){2}/) print \"()[]{}\"")
          (closing "}"))
      (insert header "\n" statement "\n" regexp "\n" closing "\n")
      (awk-ts-mode)
      (dolist (character '(?\( ?\) ?\[ ?\] ?{ ?}))
        (should (eq (char-syntax character) ?.)))
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
           (= (awk-ts-mode-test--syntax-class-in-line
               line fragment offset)
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
           (= (awk-ts-mode-test--syntax-class-in-line
               line fragment offset)
              1)))))))

(ert-deftest awk-ts-mode-reclassifies-delimiters-after-edits ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "BEGIN {\nprint\n}\n")
    (awk-ts-mode)
    (should (= (awk-ts-mode-test--syntax-class-in-line "}" "}") 5))
    (goto-char (point-min))
    (search-forward "{")
    (delete-char -1)
    (should (= (awk-ts-mode-test--syntax-class-in-line "}" "}") 1))
    (goto-char (point-min))
    (search-forward "BEGIN ")
    (insert "{")
    (should (= (awk-ts-mode-test--syntax-class-in-line "}" "}") 5))))

(ert-deftest awk-ts-mode-configures-posix-awk-line-comments ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode)
    (should (equal comment-start "# "))
    (should (equal comment-end ""))
    (should (equal comment-start-skip "#[[:blank:]]*"))
    (should comment-use-syntax)
    (should (eq (char-syntax ?#) ?.))
    (should (eq (char-syntax ?\") ?.))
    (should (eq (char-syntax ?\\) ?.))
    (should (eq (char-syntax ?\n) ?>))))

(ert-deftest awk-ts-mode-keeps-comments-open-through-buffer-end ()
  (awk-ts-mode-test--require-grammar)
  (dolist (source '("# note" "#"))
    (with-temp-buffer
      (insert source)
      (awk-ts-mode)
      (syntax-propertize (point-max))
      (should (nth 4 (syntax-ppss (point-max)))))))

(ert-deftest awk-ts-mode-recognizes-only-tree-sitter-comments ()
  (awk-ts-mode-test--require-grammar)
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
      (should (awk-ts-mode-test--comment-in-line-p top "top"))
      (should-not (awk-ts-mode-test--comment-in-line-p string "string"))
      (should-not (awk-ts-mode-test--comment-in-line-p regexp "regexp"))
      (should (awk-ts-mode-test--comment-in-line-p regexp "trailing"))
      (should (awk-ts-mode-test--comment-in-line-p continued "backslash"))
      (should-not (awk-ts-mode-test--comment-in-line-p next "print")))))

(ert-deftest awk-ts-mode-recomputes-comment-syntax-after-edits ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (let ((commented "BEGIN { print x # note"))
      (insert commented "\n}\n")
      (awk-ts-mode)
      (should (awk-ts-mode-test--comment-in-line-p commented "note"))
      (goto-char (awk-ts-mode-test--position-in-line commented "#"))
      (let ((quote-position (point))
            (string "BEGIN { print x \"# note"))
        (insert "\"")
        (should-not (awk-ts-mode-test--comment-in-line-p string "note"))
        (delete-region quote-position (1+ quote-position))
        (should (awk-ts-mode-test--comment-in-line-p commented "note"))))))

(ert-deftest awk-ts-mode-propertizes-comments-hidden-before-a-narrowing ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (insert "# first\n# second\n")
    (awk-ts-mode)
    (goto-char (point-min))
    (forward-line 1)
    (narrow-to-region (point) (point-max))
    (should (nth 4 (syntax-ppss (+ (point-min) 2))))
    (widen)
    (should (nth 4 (syntax-ppss (+ (point-min) 2))))))

(ert-deftest awk-ts-mode-propertizes-comments-at-the-end-of-wide-programs ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (dotimes (number 10000)
      (insert (format "value%d = %d # comment%d\n"
                      number number number)))
    (awk-ts-mode)
    (goto-char (point-max))
    (search-backward "# comment9999")
    (let ((comment-start (point)))
      (funcall syntax-propertize-function comment-start (point-max))
      (should-not (nth 4 (syntax-ppss (1- comment-start))))
      (should (nth 4 (syntax-ppss (+ comment-start 2)))))))

;;;; Font Lock

(ert-deftest awk-ts-mode-fontifies-posix-awk-syntax ()
  (awk-ts-mode-test--require-grammar)
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
      (awk-ts-mode-test--should-fontify
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
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode-test--fontify
     4
     '("\\" "function total\\" "(value,\\" " scale)\\" "{"
       "  if (value)\\" "    return value\\" "      + scale"
       "}" "BEGIN { total\\" "(1); print \\" " 3 }"))
    (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                    'has-error))
    (awk-ts-mode-test--should-fontify
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

(ert-deftest awk-ts-mode-preserves-function-roles-after-parameter-newline-edits ()
  (awk-ts-mode-test--require-grammar)
  (dolist (header '("function total(" "function total ("))
    (with-temp-buffer
      (awk-ts-mode-test--fontify
       4 (list (concat header "first, second) { return first }")))
      (dolist (gap '("\n" " # note\n\n" " "))
        (goto-char (point-min))
        (search-forward ",")
        (let ((start (point)))
          (search-forward "second")
          (delete-region start (- (point) (length "second")))
          (goto-char start)
          (insert gap))
        (font-lock-flush)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                        'has-error))
        (goto-char (point-min))
        (awk-ts-mode-test--should-have-next-faces
         '(("total" font-lock-function-name-face)
           ("first" font-lock-variable-name-face)
           ("," font-lock-punctuation-face)
           ("second" font-lock-variable-name-face)
           ("first" font-lock-variable-use-face)))
        (should (equal (mapcar #'car
                               (cdr (assoc "Function"
                                           (funcall imenu-create-index-function))))
                       '("total")))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-reclassifies-calls-after-continuation-edits ()
  (awk-ts-mode-test--require-grammar)
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

(ert-deftest awk-ts-mode-restores-fontification-after-token-split-repairs ()
  (awk-ts-mode-test--require-grammar)
  (pcase-dolist (`(,before ,after ,fragment ,face)
                 '(("BE" "GIN { print 1 }\n" "BEGIN" font-lock-keyword-face)
                   ("function to" "tal(value) { return value }\n"
                    "total" font-lock-function-name-face)
                   ("BEGIN { print va" "lue }\n"
                    "value" font-lock-variable-use-face)
                   ("BEGIN { print len" "gth(value) }\n"
                    "length" font-lock-builtin-face)
                   ("BEGIN { value +" "= 1 }\n"
                    "+=" font-lock-operator-face)
                   ("BEGIN { print 1e+" "2 }\n"
                    "1e+2" font-lock-number-face)
                   ("BEGIN { print 1.0" "F }\n"
                    "1.0F" font-lock-number-face)
                   ("BEGIN { print \"a" "b\" }\n"
                    "ab" font-lock-string-face)
                   ("BEGIN { print /[a" "-z]/ }\n"
                    "-" font-lock-operator-face)))
    (ert-info ((concat before after))
      (with-temp-buffer
        (awk-ts-mode-test--fontify 4 (list (concat before after)))
        (goto-char (1+ (length before)))
        (insert "\\\n")
        (dolist (prefix '("" "# shifted source\n"))
          (goto-char (point-min))
          (insert prefix)
          (font-lock-flush)
          (font-lock-ensure))
        (goto-char (point-min))
        (search-forward "\\\n")
        (delete-region (- (point) 2) (point))
        (font-lock-flush)
        (font-lock-ensure)
        (let ((root (treesit-buffer-root-node 'posix-awk)))
          (should-not (treesit-node-check root 'has-error)))
        (goto-char (point-min))
        (search-forward fragment)
        (should-not (text-property-not-all (- (point) (length fragment))
                                           (point) 'face face))
        (should (awk-ts-mode-test--comment-in-line-p
                 "# shifted source" "shifted"))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-fontifies-floating-suffixes-at-number-boundaries ()
  (awk-ts-mode-test--require-grammar)
  (pcase-dolist (`(,expression ,number ,name)
                 '(("1.0f" "1.0f" nil)
                   (".5F" ".5F" nil)
                   ("1e2l" "1e2l" nil)
                   ("1.L" "1.L" nil)
                   ("1f" "1" "f")
                   ("1L" "1" "L")
                   ("1.0eF" "1.0" "eF")
                   ("1e+F" "1" "F")
                   ("1.0foo" "1.0f" "oo")))
    (with-temp-buffer
      (awk-ts-mode-test--fontify
       3 (list (concat "BEGIN { print " expression " }")))
      (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                      'has-error))
      (goto-char (point-min))
      (search-forward number)
      (dotimes (offset (length number))
        (should (eq (get-text-property (- (point) 1 offset) 'face)
                    'font-lock-number-face)))
      (when name
        (search-forward name)
        (dotimes (offset (length name))
          (should (eq (get-text-property (- (point) 1 offset) 'face)
                      'font-lock-variable-use-face)))))))

(ert-deftest awk-ts-mode-reclassifies-floating-suffixes-after-edits ()
  (awk-ts-mode-test--require-grammar)
  (pcase-dolist (`(,expression ,offset ,removed ,inserted ,suffix)
                 '(("1.0 f" 3 " " "" "f")
                   ("1.0eF" 4 "" "2" "F")
                   ("1l" 1 "" "." "l")
                   ("1. L" 2 " " "" "L")))
    (with-temp-buffer
      (awk-ts-mode-test--fontify
       3 (list (concat "BEGIN { print " expression " }")))
      (pcase-dolist (`(,old ,new ,face)
                     `(("" "" font-lock-variable-use-face)
                       (,removed ,inserted font-lock-number-face)
                       (,inserted ,removed font-lock-variable-use-face)))
        (goto-char (point-min))
        (search-forward "print ")
        (forward-char offset)
        (delete-char (length old))
        (insert new)
        (font-lock-flush)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                        'has-error))
        (goto-char (point-min))
        (should (eq (awk-ts-mode-test--face-at-next suffix) face))))))

(ert-deftest awk-ts-mode-preserves-getline-highlighting-after-postfix-edits ()
  (awk-ts-mode-test--require-grammar)
  (pcase-dolist (`(,expression ,update ,faces)
                 '(("getline x" "++"
                    (("getline" font-lock-keyword-face)
                     ("x" font-lock-variable-use-face)))
                   ("getline a[1]" "--"
                    (("getline" font-lock-keyword-face)
                     ("a" font-lock-variable-use-face)
                     ("[" font-lock-bracket-face)
                     ("1" font-lock-number-face)
                     ("]" font-lock-bracket-face)))
                   ("getline $i" "++"
                    (("getline" font-lock-keyword-face)
                     ("$" font-lock-operator-face)
                     ("i" font-lock-variable-use-face)))
                   ("command | getline x" "--"
                    (("command" font-lock-variable-use-face)
                     ("|" font-lock-operator-face)
                     ("getline" font-lock-keyword-face)
                     ("x" font-lock-variable-use-face)))))
    (pcase-dolist (`(,prefix ,closing)
                   '(("BEGIN { " " }") ("BEGIN { print (" ") }")))
      (with-temp-buffer
        (awk-ts-mode-test--fontify
         4 (list (concat prefix expression closing)))
        (dolist (suffix (list "" update ""))
          (goto-char (point-min))
          (search-forward expression)
          (let ((end (point)))
            (search-forward closing)
            (delete-region end (- (point) (length closing)))
            (goto-char end)
            (insert suffix))
          (font-lock-flush)
          (font-lock-ensure)
          (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                          'has-error))
          (goto-char (point-min))
          (search-forward prefix)
          (awk-ts-mode-test--should-have-next-faces faces)
          (unless (equal suffix "")
            (awk-ts-mode-test--should-have-next-faces
             (list (list suffix 'font-lock-operator-face)))))))))

(ert-deftest awk-ts-mode-fontifies-awk-delimiters ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode-test--fontify
     4 '("function f(a) { if ((a[1])) { print f(a) } }"))
    (goto-char (point-min))
    (awk-ts-mode-test--should-have-next-faces
     '(("f(" font-lock-bracket-face)
       ("{" font-lock-bracket-face)
       ("if (" font-lock-bracket-face)
       ("(" font-lock-bracket-face)
       ("[" font-lock-bracket-face)
       ("]" font-lock-bracket-face)
       ("{" font-lock-bracket-face)
       ("f(" font-lock-bracket-face)))))

(ert-deftest awk-ts-mode-keeps-static-ere-delimiter-faces ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode-test--fontify
     4 '("BEGIN { if (/((a)[b])/) print $(x) }"))
    (goto-char (point-min))
    (awk-ts-mode-test--should-have-next-faces
     '(("{" font-lock-bracket-face)
       ("if (" font-lock-bracket-face)
       ("/(" font-lock-bracket-face)
       ("[" font-lock-bracket-face)
       ("$" font-lock-operator-face)
       ("(" font-lock-bracket-face)))))

(ert-deftest awk-ts-mode-fontifies-every-posix-awk-keyword ()
  (awk-ts-mode-test--require-grammar)
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
        (should (eq (awk-ts-mode-test--face-in-line line keyword)
                    'font-lock-keyword-face))))))

(ert-deftest awk-ts-mode-fontifies-every-posix-awk-named-operator ()
  (awk-ts-mode-test--require-grammar)
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
        (should (eq (awk-ts-mode-test--face-in-line line operator)
                    'font-lock-operator-face)))))
  (with-temp-buffer
    (awk-ts-mode-test--fontify 4 '("BEGIN { if (left && right) print }"))
    (goto-char (point-min))
    (search-forward "&&")
    (should (eq (get-text-property (1- (point)) 'face)
                'font-lock-operator-face))))

(ert-deftest awk-ts-mode-fontifies-posix-ere-components ()
  (awk-ts-mode-test--require-grammar)
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
      (awk-ts-mode-test--should-fontify
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
  (awk-ts-mode-test--require-grammar)
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
  (awk-ts-mode-test--require-grammar)
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

(ert-deftest awk-ts-mode-preserves-collating-content-after-escape-edits ()
  (awk-ts-mode-test--require-grammar)
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

(ert-deftest awk-ts-mode-restores-ere-highlighting-after-repair ()
  (awk-ts-mode-test--require-grammar)
  (dolist (case '(("/broken" "/broken/" "broken" font-lock-regexp-face)
                  ("/[[:alpha" "/[[:alpha:]]/" "alpha" font-lock-constant-face)
                  ("/[[.x" "/[[.x.]]/" "." font-lock-punctuation-face)
                  ("/[[=x" "/[[=x=]]/" "=" font-lock-punctuation-face)
                  ("/a{2," "/a{2,3}/" "2" font-lock-number-face)
                  ("/[[:alpha#:]]/" "/[[:alpha:]]/" "alpha" font-lock-constant-face)
                  ("/a{2#}/" "/a{2}/" "2" font-lock-number-face)
                  ("/[[=-=]]/" "/[[=^=]]/" "^" font-lock-constant-face)
                  ("/[[=]=]]/" "/[[=]a=]]/" "]a" font-lock-constant-face)))
    (pcase-let ((`(,broken ,repaired ,fragment ,face) case))
      (with-temp-buffer
        (insert "BEGIN { print " broken "\n}\n"
                "function after() { return 1 }\n# note\n")
        (let ((treesit-font-lock-level 4)) (awk-ts-mode))
        (font-lock-ensure)
        (syntax-propertize (point-max))
        (should (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                    'has-error))
        (goto-char (point-min))
        (search-forward broken)
        (replace-match repaired t t)
        (font-lock-flush)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                        'has-error))
        (let ((line (concat "BEGIN { print " repaired)))
          (should (eq (awk-ts-mode-test--face-in-line line fragment) face))
          (should (eq (awk-ts-mode-test--face-in-line line "/")
                      'font-lock-delimiter-face)))
        (should (= (awk-ts-mode-test--syntax-class-in-line "}" "}") 5))
        (should (awk-ts-mode-test--comment-in-line-p "# note" "note"))
        (should (equal (mapcar #'car (cdr (assoc "Function"
                                                 (funcall imenu-create-index-function))))
                       '("after")))))))

(ert-deftest awk-ts-mode-fontifies-by-font-lock-level ()
  (awk-ts-mode-test--require-grammar)
  (pcase-dolist (`(,level ,comment-face ,keyword-face ,string-face
                          ,escape-face ,number-face ,regexp-face
                          ,regexp-escape-face ,regexp-content-face
                          ,operator-face ,continuation-face)
                 '((1 font-lock-comment-face nil nil nil nil nil nil nil nil nil)
                   (2 font-lock-comment-face font-lock-keyword-face
                      font-lock-string-face font-lock-string-face
                      nil nil nil nil nil nil)
                   (3 font-lock-comment-face font-lock-keyword-face
                      font-lock-string-face font-lock-escape-face
                      font-lock-number-face nil nil nil nil nil)
                   (4 font-lock-comment-face font-lock-keyword-face
                      font-lock-string-face font-lock-escape-face
                      font-lock-number-face font-lock-regexp-face
                      font-lock-escape-face font-lock-constant-face
                      font-lock-operator-face font-lock-punctuation-face)))
    (with-temp-buffer
      (let ((note "# note")
            (program "BEGIN { if (\"a\\tb\" ~ /c\\n[é\\141]/) print 1 + 2 }")
            (continued "BEGIN { value = 1 + \\")
            (tail "2 }"))
        (awk-ts-mode-test--fontify level (list note program continued tail))
        (awk-ts-mode-test--should-fontify
         `((,note "note" ,comment-face)
           (,program "BEGIN" ,keyword-face)
           (,program "\"a" ,string-face)
           (,program "\\t" ,escape-face)
           (,program "1 +" ,number-face)
           (,program "c" ,regexp-face)
           (,program "\\n" ,regexp-escape-face)
           (,program "é" ,regexp-content-face)
           (,program "\\141" ,regexp-escape-face)
           (,program "+" ,operator-face)
           (,continued "\\" ,continuation-face)))))))

(ert-deftest awk-ts-mode-keeps-font-lock-levels-independent-between-buffers ()
  (awk-ts-mode-test--require-grammar)
  (let ((source "BEGIN { print 1 }\n"))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (awk-ts-mode))
      (font-lock-ensure)
      (let ((state (awk-ts-mode-test--buffer-state)))
        (with-temp-buffer
          (insert source)
          (let ((treesit-font-lock-level 1)) (awk-ts-mode))
          (font-lock-ensure)
          (goto-char (point-min))
          (search-forward "{")
          (should-not (get-text-property (1- (point)) 'face)))
        (font-lock-flush)
        (font-lock-ensure)
        (should (equal state (awk-ts-mode-test--buffer-state)))
        (goto-char (point-min))
        (search-forward "{")
        (should (eq (get-text-property (1- (point)) 'face)
                    'font-lock-bracket-face))))))

(ert-deftest awk-ts-mode-fontifies-chunks-like-the-whole-buffer ()
  (awk-ts-mode-test--require-grammar)
  (let ((source "function f(x) {\n if (x ~ /[[:alpha:]]+/) print \"x\\n\", x\n}\n# note\nBEGIN { f(1) }\n"))
    (dolist (chunk '(7 97))
      (with-temp-buffer
        (insert source)
        (let ((treesit-font-lock-level 4)) (awk-ts-mode))
        (let ((position (point-min)))
          (while (< position (point-max))
            (let ((end (min (point-max) (+ position chunk))))
              (font-lock-fontify-region position end)
              (setq position end))))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest awk-ts-mode-captures-only-leaves ()
  (awk-ts-mode-test--require-grammar)
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

(ert-deftest awk-ts-mode-uses-standard-tree-sitter-navigation ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode)
    (should (equal (alist-get 'posix-awk treesit-thing-settings)
                   (alist-get 'posix-awk awk-ts-mode-thing-settings)))
    (should (treesit-thing-defined-p 'sexp 'posix-awk))
    (should-not (treesit-thing-defined-p 'sentence 'posix-awk))
    (should (eq forward-sexp-function #'treesit-forward-sexp))
    (should (eq beginning-of-defun-function
                #'treesit-beginning-of-defun))
    (should (eq end-of-defun-function #'treesit-end-of-defun))))

(ert-deftest awk-ts-mode-navigates-items-and-functions ()
  (awk-ts-mode-test--require-grammar)
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
                   (+ (awk-ts-mode-test--position-in-line line line)
                      (length line)))))
      (goto-char (point-max))
      (beginning-of-defun)
      (should (looking-at-p "function second"))
      (beginning-of-defun)
      (should (looking-at-p "function first"))
      (end-of-defun)
      (should (eq (char-before) ?\n))
      (should (eq (char-before (1- (point))) ?})))))

;;;; Imenu

(ert-deftest awk-ts-mode-uses-standard-tree-sitter-imenu ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode)
    (should (equal treesit-simple-imenu-settings
                   awk-ts-mode-imenu-settings))
    (should (eq treesit-defun-name-function
                #'awk-ts-mode--defun-name))
    (should (eq imenu-create-index-function #'treesit-simple-imenu))))

(ert-deftest awk-ts-mode-indexes-only-function-items ()
  (awk-ts-mode-test--require-grammar)
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
         (awk-ts-mode-test--position-in-line
          "function first(value) { return value }" "function")
         (awk-ts-mode-test--position-in-line
          "function second (value) { return value }" "function")))))
    (let ((function (treesit-thing-next (point-min) 'defun))
          (root (treesit-buffer-root-node 'posix-awk)))
      (should (equal (treesit-defun-name function) "first"))
      (should-not (treesit-defun-name root)))))

(ert-deftest awk-ts-mode-rebuilds-imenu-after-edits ()
  (awk-ts-mode-test--require-grammar)
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

(ert-deftest awk-ts-mode-restores-structure-and-imenu-after-repair ()
  (awk-ts-mode-test--require-grammar)
  (dolist (case '(("function repaired(value { return value }\n"
                   "value {" "value) {" ("repaired" "after"))
                  ("function repaired()\n"
                   ")" ") { return 1 }" ("repaired" "after"))
                  ("BEGIN {} END {}\n" " END" "\nEND" ("after"))
                  ("left, { print 1 }\n" ", " ", right " ("after"))
                  ("BEGIN { if (value) }\n" ") " ") print value " ("after"))
                  ("BEGIN { do print value; }\n" "; " "; while (value) " ("after"))
                  ("BEGIN { print 1 &&\n}\n" "&&\n" "&&\n2\n" ("after"))
                  ("BEGIN { print (value }\n" "value " "value) " ("after"))
                  ("BEGIN { print array[1 }\n" "1 " "1] " ("after"))))
    (pcase-let ((`(,source ,removed ,inserted ,names) case))
      (with-temp-buffer
        (insert source "function after() { return 1 }\n# note\n")
        (let ((treesit-font-lock-level 4)) (awk-ts-mode))
        (font-lock-ensure)
        (syntax-propertize (point-max))
        (funcall imenu-create-index-function)
        (should (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                    'has-error))
        (goto-char (point-min))
        (search-forward removed)
        (replace-match inserted t t)
        (font-lock-flush)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-buffer-root-node 'posix-awk)
                                        'has-error))
        (should (equal (mapcar #'car (cdr (assoc "Function"
                                                 (funcall imenu-create-index-function))))
                       names))
        (should (eq (awk-ts-mode-test--face-in-line
                     "function after() { return 1 }" "after")
                    'font-lock-function-name-face))
        (should (awk-ts-mode-test--comment-in-line-p "# note" "note"))
        (awk-ts-mode-test--should-match-fresh-buffer 4)))))

;;;; Indentation

(ert-deftest awk-ts-mode-uses-standard-tree-sitter-indentation ()
  (awk-ts-mode-test--require-grammar)
  (with-temp-buffer
    (awk-ts-mode)
    (should (eq indent-line-function #'treesit-indent))
    (should (eq indent-region-function #'treesit-indent-region))
    (should (equal (alist-get 'posix-awk treesit-simple-indent-rules)
                   (alist-get 'posix-awk awk-ts-mode-indent-rules)))))

(ert-deftest awk-ts-mode-indents-structural-rules ()
  (awk-ts-mode-test--require-grammar)
  (should
   (equal
    (awk-ts-mode-test--indent
     "BEGIN {\nprint 1\nif (ready)\nprint 2\nelse\nprint 3\n}\n")
    "BEGIN {\n  print 1\n  if (ready)\n    print 2\n  else\n    print 3\n}\n")))

(ert-deftest awk-ts-mode-indent-offset-controls-rules ()
  (awk-ts-mode-test--require-grammar)
  (should
   (equal
    (awk-ts-mode-test--indent
     "BEGIN {\nprint 1\n}\n" 4)
    "BEGIN {\n    print 1\n}\n")))

(ert-deftest awk-ts-mode-indents-do-while-closers ()
  (awk-ts-mode-test--require-grammar)
  (should
   (equal
    (awk-ts-mode-test--indent
     "BEGIN {\ndo\nprint\n        while (0)\n}\n")
    "BEGIN {\n  do\n    print\n  while (0)\n}\n")))

;;;; Mode Selection

(ert-deftest awk-ts-mode-selects-awk-files ()
  (awk-ts-mode-test--require-grammar)
  (let (patterns)
    (dolist (entry auto-mode-alist)
      (when (eq (cdr entry) 'awk-ts-mode)
        (push (car entry) patterns)))
    (should (equal (nreverse patterns) '("\\.awk\\'"))))
  (with-temp-buffer
    (setq buffer-file-name "/tmp/example.awk")
    (set-auto-mode)
    (should (eq major-mode 'awk-ts-mode))))

(ert-deftest awk-ts-mode-generates-mode-and-file-association-autoloads ()
  (require 'loaddefs-gen)
  (let ((output (make-temp-file "awk-ts-mode-loaddefs-"))
        (directory
         (file-name-directory (locate-library "awk-ts-mode"))))
    (unwind-protect
        (progn
          (loaddefs-generate directory output nil nil nil t)
          (with-temp-buffer
            (insert-file-contents output)
            (dolist (form '("(autoload 'awk-ts-mode"
                            "(add-to-list 'auto-mode-alist"
                            "(add-to-list 'interpreter-mode-alist"))
              (goto-char (point-min))
              (should (search-forward form nil t)))))
      (delete-file output))))

(ert-deftest awk-ts-mode-selects-awk-interpreters ()
  (awk-ts-mode-test--require-grammar)
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

(provide 'awk-ts-mode-test)

;;; awk-ts-mode-test.el ends here
