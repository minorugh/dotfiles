(require 'solarized)
(deftheme solarized-iceberg-dark "The solarized-iceberg-dark colour theme of Solarized colour theme flavor.")
(solarized-with-color-variables 'dark 'solarized-iceberg-dark
  '((base03 . "#161821")
    (base02 . "#1a1c25")
    (base01 . "#4d4f59")
    (base00 . "#565761")
    (base0 . "#6b6d76")
    (base1 . "#767781")
    (base2 . "#babcc5")
    (base3 . "#c6c8d1")
    (yellow . "#e2a478")
    (orange . "#e27878")
    (red . "#e27878")
    (magenta . "#a093c7")
    (violet . "#b4be82")
    (blue . "#84a0c6")
    (cyan . "#89b8c2")
    (green . "#84a0c6")
    (yellow-d . "#3b3031")
    (yellow-l . "#cec1bf")
    (orange-d . "#3b2a31")
    (orange-l . "#cfb9bf")
    (red-d . "#3b2a31")
    (red-l . "#cfb9bf")
    (magenta-d . "#2e2e3e")
    (magenta-l . "#bfbdcf")
    (violet-d . "#323533")
    (violet-l . "#c3c6c1")
    (blue-d . "#2a303e")
    (blue-l . "#b9c0cf")
    (cyan-d . "#2b343d")
    (cyan-l . "#bac5ce")
    (green-d . "#2a303e")
    (green-l . "#b9c0cf")
    (yellow-1bg . "#322a2d")
    (orange-1bg . "#32252d")
    (red-1bg . "#32252d")
    (magenta-1bg . "#282837")
    (blue-1bg . "#252a36")
    (cyan-1bg . "#252d36")
    (green-1bg . "#252a36")
    (violet-1bg . "#2b2d2f")
    (yellow-1fg . "#dcaf92")
    (orange-1fg . "#de9192")
    (red-1fg . "#de9192")
    (magenta-1fg . "#aca3ca")
    (violet-1fg . "#bac19a")
    (blue-1fg . "#99acc9")
    (cyan-1fg . "#9cbdc6")
    (green-1fg . "#99acc9")
    (yellow-2bg . "#624b42")
    (orange-2bg . "#623c42")
    (red-2bg . "#623c42")
    (magenta-2bg . "#48455e")
    (violet-2bg . "#515446")
    (blue-2bg . "#3f4a5d")
    (cyan-2bg . "#40525c")
    (green-2bg . "#3f4a5d")
    (yellow-2fg . "#d9b4a0")
    (orange-2fg . "#da9d9f")
    (red-2fg . "#da9d9f")
    (magenta-2fg . "#b1abcc")
    (violet-2fg . "#bdc2a6")
    (blue-2fg . "#a3b2cb")
    (cyan-2fg . "#a5bfc9")
    (green-2fg . "#a3b2cb"))
  '((custom-theme-set-faces theme-name
			    `(default
			      ((,class
				(:foreground ,base3 :background ,base03))))
			    `(vertical-border
			      ((,class
				(:foreground ,base03))))
			    `(mode-line
			      ((,class
				(:foreground ,base2 :background ,base02))))
			    `(mode-line-inactive
			      ((,class
				(:foreground ,base0 :background ,base03))))
			    `(font-lock-comment-delimiter-face
			      ((,class
				(:foreground "#6b7089"))))
			    `(font-lock-comment-face
			      ((,class
				(:foreground "#6b7089"))))
			    `(font-lock-preprocessor-face
			      ((,class
				(:foreground ,green))))
			    `(font-lock-type-face
			      ((,class
				(:foreground ,cyan))))
			    `(font-lock-builtin-face
			      ((,class
				(:foreground ,green))))
			    `(diff-function
			      ((,class
				(:foreground ,violet-1fg))))
			    `(diff-header
			      ((,class
				(:foreground ,green))))
			    `(diff-hunk-header
			      ((,class
				(:foreground ,green))))
			    `(diff-file-header
			      ((,class
				(:background ,base03 :foreground ,green))))
			    `(diff-added
			      ((,class
				(:background ,violet-1bg :foreground ,violet-1fg))))
			    `(diff-indicator-added
			      ((t
				(:foreground ,violet))))
			    `(markdown-header-face
			      ((,class
				(:foreground ,yellow))))
			    `(markdown-header-rule-face
			      ((,class
				(:foreground ,green))))
			    `(markdown-markup-face
			      ((,class
				(:inherit default))))
			    `(markdown-url-face
			      ((,class
				(:foreground ,magenta))))
			    `(markdown-link-face
			      ((,class
				(:foreground ,green :underline t))))
			    `(markdown-inline-code-face
			      ((,class
				(:foreground ,cyan))))
			    `(markdown-pre-face
			      ((,class
				(:foreground ,cyan))))
			    `(sh-quoted-exec
			      ((,class
				(:foreground ,violet))))
			    `(haskell-type-face
			      ((,class
				(:inherit default))))
			    `(haskell-constructor-face
			      ((,class
				(:inherit default))))
			    `(haskell-operator-face
			      ((,class
				(:foreground ,green))))
			    `(haskell-definition-face
			      ((,class
				(:inherit default))))
			    `(web-mode-block-delimiter-face
			      ((,class
				(:inherit default))))
			    `(web-mode-html-attr-value-face
			      ((,class
				(:foreground ,cyan))))
			    `(web-mode-mode-type-face
			      ((,class
				(:inherit default))))
			    `(web-mode-function-call-face
			      ((,class
				(:inherit default))))
			    `(web-mode-keyword-face
			      ((,class
				(:foreground ,green))))
			    `(web-mode-constant-face
			      ((,class
				(:foreground ,cyan))))
			    `(web-mode-variable-name-face
			      ((,class
				(:foreground ,cyan))))
			    `(web-mode-html-tag-bracket-face
			      ((,class
				(:foreground ,green))))
			    `(org-verbatim
			      ((,class
				(:foreground ,cyan))))
			    `(php-php-tag
			      ((,class
				(:inherit default))))
			    `(php-constant
			      ((,class
				(:inherit default))))
			    `(php-paamayim-nekudotayim
			      ((,class
				(:foreground ,green))))
			    `(php-object-op
			      ((,class
				(:foreground ,cyan))))
			    `(php-variable-name
			      ((,class
				(:foreground ,cyan))))
			    `(php-variable-sigil
			      ((,class
				(:foreground ,cyan)))))))
(provide-theme 'solarized-iceberg-dark)
(provide 'solarized-iceberg-dark-theme)
