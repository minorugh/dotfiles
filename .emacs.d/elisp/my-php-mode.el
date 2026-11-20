;;; my-php-mode.el --- PHP highlighting only  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:

(defvar my-php-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    (modify-syntax-entry ?' "\"" st)
    (modify-syntax-entry ?\" "\"" st)
    st))

(defvar my-php-font-lock-keywords
  (list
   '("<\\?\\(?:php\\|=\\)?\\|\\?>" . font-lock-preprocessor-face)
   (cons (concat "\\_<"
                 (regexp-opt '("abstract" "and" "as" "break" "case" "catch" "class"
                               "clone" "const" "continue" "declare" "default" "do"
                               "echo" "else" "elseif" "empty" "enum" "extends"
                               "final" "finally" "fn" "for" "foreach" "function"
                               "global" "if" "implements" "include" "include_once"
                               "instanceof" "interface" "isset" "list" "match"
                               "namespace" "new" "or" "print" "private" "protected"
                               "public" "readonly" "require" "require_once" "return"
                               "static" "switch" "throw" "trait" "try" "unset"
                               "use" "var" "while" "xor" "yield" "die" "exit")
                             t)
                 "\\_>")
         'font-lock-keyword-face)
   (cons (concat "\\_<"
                 (regexp-opt '("true" "false" "null") t)
                 "\\_>")
         'font-lock-constant-face)
   '("__\\(?:FILE\\|LINE\\|DIR\\|FUNCTION\\|CLASS\\|METHOD\\|NAMESPACE\\|TRAIT\\)__"
     . font-lock-constant-face)
   (cons (concat "\\_<"
                 (regexp-opt '("int" "float" "string" "bool" "array" "object"
                               "mixed" "void" "iterable" "callable" "self" "parent")
                             t)
                 "\\_>")
         'font-lock-type-face)
   '("\\_<function\\_>[ \t&]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     1 font-lock-function-name-face)
   '("\\_<\\(?:class\\|interface\\|trait\\|enum\\)\\_>[ \t]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     1 font-lock-type-face)
   '("\\$[A-Za-z_][A-Za-z0-9_]*" . font-lock-variable-name-face)))

;;;###autoload
(define-derived-mode my-php-mode prog-mode "PHP"
  "Minimal major mode for PHP syntax highlighting."
  (setq-local font-lock-defaults '(my-php-font-lock-keywords nil t))
  (setq-local comment-start "// ")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.php\\'" . my-php-mode))

(provide 'my-php-mode)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-php-mode.el ends here
