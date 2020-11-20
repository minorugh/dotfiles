;; iceberg-theme --- a dark blue theme
;;; Commentary:
;;
;; cool-headed perspective for your coding
;;
;; Colorscheme: cocopon <cocopon@me.com>
;; Port:        apnsngr <github.com/apnsngr>
;; License:     MIT
;; Modify:      minorugh <minorugh@gmail.com>
;;
;;; Code:

(deftheme iceberg)

(let
    ((class '((class color) (min-colors 89)))
     (background "#161822")
     (foreground "#C7C9D1")
     (current-line "#282D43")
     (silver "#9DA5B4")
     (comment "#6B7089")
     (selection "#454D73")
	 (gray "#393F60")
     (red "#E27878")
     (orange "#E2A478")
     (green "#B4BE82")
     (cyan "#89B8C2")
     (blue "#84A0C6")
     (purple "#A093C7"))

  (custom-theme-set-faces
   'iceberg

   ;; Built-in
   `(default ((t (:background ,background :foreground ,foreground))))
   `(fringe ((t (:background ,background))))
   `(minibuffer-prompt ((t (:foreground ,blue))))
   `(menu ((t (:foreground ,foreground :background ,background))))
   `(region ((t (:foreground ,foreground :background ,selection))))
   `(highlight ((t (:background ,current-line))))
   `(success ((t (:foreground ,blue :weight bold))))
   `(warning ((t (:foreground ,red :weight bold))))

   ;; mode-line
   `(mode-line ((t (:background ,gray :foreground ,foreground))))
   `(mode-line-emphasis ((t (:weight bold))))
   `(mode-line-inactive ((t (:background ,background :foreground ,comment))))
   `(mode-line-buffer-id ((t (:foreground ,cyan :weight bold))))

   ;; Font-lock
   `(font-lock-builtin-face ((t (:foreground ,foreground :weight bold))))
   `(font-lock-comment-face ((t (:foreground ,comment))))
   `(font-lock-comment-delimiter-face ((t :foreground ,comment)))
   `(font-lock-constant-face ((t (:foreground ,purple))))
   `(font-lock-doc-face ((t (:foreground ,comment))))
   `(font-lock-doc-string-face ((t (:foreground ,comment))))
   `(font-lock-function-name-face ((t (:foreground ,orange))))
   `(font-lock-keyword-face ((t (:foreground ,blue :weight bold))))
   `(font-lock-preprocessor-face ((t (:foreground ,green))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,purple))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,purple))))
   `(font-lock-string-face ((t (:foreground ,cyan))))
   `(font-lock-type-face ((t (:foreground ,cyan))))
   `(font-lock-variable-name-face ((t (:foreground ,purple))))
   `(font-lock-warning-face ((t (:foreground ,red))))

   ;; Everything else (ordered by alphabetically)

   ;; diff
   `(diff-added ((,class (:foreground ,green :background nil))
				 (t (:foreground ,green :background nil))))
   `(diff-changed ((t (:foreground ,orange))))
   `(diff-removed ((,class (:foreground ,red :background nil))
				   (t (:foreground ,red :background nil))))
   `(diff-refine-added ((t (:inherit diff-added :weight bold))))
   `(diff-refine-change ((t (:inherit diff-changed :weight bold))))
   `(diff-refine-removed ((t (:inherit diff-removed :weight bold))))
   `(diff-header ((,class (:background ,background))
				  (t (:background ,foreground :foreground ,background))))
   `(diff-file-header
	 ((,class (:background ,background :foreground ,foreground :bold t))
	  (t (:background ,foreground :foreground ,background :bold t))))

   ;; diff-hl
   `(diff-hl-change ((,class (:foreground ,orange :background ,background))))
   `(diff-hl-delete ((,class (:foreground ,red :background ,background))))
   `(diff-hl-insert ((,class (:foreground ,green :background ,background))))
   `(diff-hl-unknown ((,class (:foreground ,cyan :background ,background))))

   ;; flycheck
   `(flycheck-error
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,red) :inherit unspecified))
	  (t (:foreground ,red :weight bold :underline t))))
   `(flycheck-warning
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,orange) :inherit unspecified))
	  (t (:foreground ,orange :weight bold :underline t))))
   `(flycheck-info
	 ((((supports :underline (:style wave)))
	   (:underline (:style wave :color ,blue) :inherit unspecified))
	  (t (:foreground ,blue :weight bold :underline t))))
   `(flycheck-fringe-error ((t (:foreground ,red :weight bold))))
   `(flycheck-fringe-warning ((t (:foreground ,orange :weight bold))))
   `(flycheck-fringe-info ((t (:foreground ,blue :weight bold))))

   ;; hl-line-mode
   `(hl-line ((t (:background ,current-line))))

   ;; ido-mode
   `(do-first-match ((t (:foreground ,blue :weight bold))))
   `(ido-only-match ((t (:foreground ,green :weight bold))))
   `(ido-subdir ((t (:foreground ,cyan))))

   ;; linum
   `(linum ((t (:foreground ,selection :background ,background))))
   ;; hlinum
   `(linum-highlight-face ((t (:foreground ,selection :background ,background))))
   ;; native line numbers (emacs version >=26)
   `(line-number ((t (:foreground ,selection :background ,background))))
   `(line-number-current-line ((t (:foreground ,foreground :background ,current-line))))

   ;; dired-mode
   '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
   '(dired-flagged ((t (:inherit (diff-hl-delete)))))
   '(dired-symlink ((t (:foreground "#A093C7"))))

   ;; neotree
   `(neo-root-dir-face ((t (:foreground ,orange :background ,background))))
   `(neo-file-link-face ((t (:foreground ,foreground))))
   `(neo-dir-link-face ((t (:foreground ,blue))))
   `(neotree-dir-face ((t (:foreground ,blue))))

   ;; imenu-list
   `(imenu-list-entry-face-0 ((t (:foreground ,orange))))
   `(imenu-list-entry-subalist-face-0 ((t (:inherit unspecified :weight bold))))
   `(imenu-list-entry-face-1 ((t (:foreground ,blue))))
   `(imenu-list-entry-subalist-face-1 ((t (:inherit unspecified :weight bold))))
   `(imenu-list-entry-face-2 ((t (:foreground ))))
   `(imenu-list-entry-subalist-face-2 ((t (:inherit unspecified :weight bold))))

   ;; mozc-cand-posframe
   '(mozc-cand-posframe-normal-face ((t (:background "#282D43" :foreground "#C7C9D1"))))
   '(mozc-cand-posframe-focused-face ((t (:background "#393F60" :foreground "#C7C9D1"))))
   '(mozc-cand-posframe-footer-face ((t (:background "#282D43" :foreground "#454D73"))))

   ;; popup
   '(popup-tip-face ((t (:background "#393F60" :foreground "#C7C9D1"))))

   ;; isearch
   `(isearch ((t (:foreground ,background :weight bold :background ,blue))))
   `(isearch-fail ((t (:foreground ,background :weight bold :background ,red))))
   `(lazy-highlight ((t (:foreground ,blue :weight bold :background ,gray))))

   ;; ivy
   `(ivy-current-match ((t (:background ,current-line))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,nil :foreground ,blue :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,nil :foreground ,red :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,nil :foreground ,green :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,nil :foreground ,orange :weight bold))))

   ;; counsel
   `(counsel-key-binding ((t (:foreground ,blue :weight bold))))

   ;; eshell
   `(eshell-ls-archive ((t (:foreground ,purple :weight bold))))
   `(eshell-ls-backup ((t (:foreground ,orange))))
   `(eshell-ls-clutter ((t (:foreground ,red :weight bold))))
   `(eshell-ls-directory ((t (:foreground ,blue :weight bold))))
   `(eshell-ls-executable ((t (:foreground ,green :weight bold))))
   `(eshell-ls-missing ((t (:foreground ,red :weight bold))))
   `(eshell-ls-product ((t (:foreground ,orange))))
   `(eshell-ls-special ((t (:foreground "#FD5FF1" :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,cyan :weight bold))))
   `(eshell-ls-unreadable ((t (:foreground ,foreground))))
   `(eshell-prompt ((t (:inherit minibuffer-prompt :weight bold))))

   ;; org-mode
   `(org-agenda-date-today
	 ((t (:foreground ,foreground :slant italic :weight bold))) t)
   `(org-agenda-structure
	 ((t (:inherit font-lock-comment-face))))
   `(org-archived ((t (:foreground ,foreground :weight bold))))
   `(org-checkbox ((t (:background ,background :foreground ,foreground
								   :box (:line-width 1 :style released-button)))))
   `(org-date ((t (:foreground ,purple))))
   `(org-deadline-announce ((t (:foreground ,red))))
   `(org-done ((t (:foreground ,green))))
   `(org-formula ((t (:foreground ,orange))))
   `(org-headline-done ((t (:foreground ,green))))
   `(org-hide ((t (:foreground ,comment))))
   `(org-level-1 ((t (:foreground ,orange :weight bold :height 1.2))))
   `(org-level-2 ((t (:foreground ,purple))))
   `(org-level-3 ((t (:foreground ,cyan))))
   `(org-level-4 ((t (:foreground ,green))))
   `(org-level-5 ((t (:foreground ,red))))
   `(org-level-6 ((t (:foreground ,orange))))
   `(org-level-7 ((t (:foreground ,comment))))
   `(org-level-8 ((t (:foreground ,foreground))))
   `(org-link ((t (:foreground ,cyan))))
   `(org-scheduled ((t (:foreground ,green))))
   `(org-scheduled-previously ((t (:foreground ,red))))
   `(org-scheduled-today ((t (:foreground ,blue))))
   `(org-sexp-date ((t (:foreground ,blue :underline t))))
   `(org-special-keyword ((t (:inherit font-lock-comment-face))))
   `(org-table ((t (:foreground ,blue))))
   `(org-tag ((t (:bold t :weight bold))))
   `(org-time-grid ((t (:foreground ,blue))))
   `(org-todo ((t (:bold t :foreground ,cyan :weight bold))))
   `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
   `(org-warning ((t (:bold t :foreground ,orange :weight bold :underline nil))))
   `(org-column ((t (:background ,background))))
   `(org-column-title ((t (:background ,background :underline t :weight bold))))
   `(org-mode-line-clock-overrun ((t (:foreground ,background :background ,red))))
   `(org-footnote ((t (:foreground ,cyan :underline t))))

   ;; markdown
   `(markdown-code-face ((t (:foreground ,purple :background ,background))))
   `(markdown-header-face ((t (:foreground ,blue :weight bold :height 1.1))))
   `(markdown-header-face-1 ((t (:foreground ,blue :weight bold :height 1.1 ))))
   `(markdown-header-face-2 ((t (:foreground ,blue :weight bold :height 1.1 ))))
   `(markdown-header-face-3 ((t (:foreground ,blue :weight bold :height 1.0 ))))
   `(markdown-header-face-4 ((t (:foreground ,blue :weight bold :height 1.0 ))))
   `(markdown-header-face-5 ((t (:foreground ,blue :weight bold :height 0.9 ))))
   `(markdown-header-face-6 ((t (:foreground ,blue :weight bold :height 0.9 ))))
   `(markdown-header-face-7 ((t (:foreground ,blue :weight bold :height 0.8 ))))
   `(markdown-header-face-8 ((t (:foreground ,blue :weight bold :height 0.8 ))))
   `(markdown-header-delimiter-face ((t (:foreground ,blue :weight bold :height 1.1))))
   `(markdown-url-face ((t (:inherit link))))
   `(markdown-link-face ((t (:foreground ,blue :underline t))))

   ;; makefile-mode
   `(makefile-targets ((t (:foreground ,blue :weight bold))))

   ;; howm-mode
   `(howm-view-hilit-face ((t (:foreground ,red :weight bold)))) ;; Face for matched word
   `(howm-view-name-face ((t (:foreground ,blue :background ,background))))
   `(howm-view-empty-face ((t (:background ,background))))
   `(howm-mode-title-face ((t (:foreground ,red)))) ;; =

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,orange))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,blue))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,cyan))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,purple))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,red))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,comment))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,gray))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,foreground))))
   '(rainbow-delimiters-unmatched-face ((t (:background "#E27878" :foreground "#111" :weight bold))))

   ;; company
   `(company-tooltip ((t (:foreground ,foreground :background ,current-line))))
   `(company-tooltip-common ((t (:foreground ,purple :weight bold))))
   `(company-tooltip-common-selection ((t (:inherit company-tooltip-common))))
   `(company-tooltip-annotation ((t ((:foreground ,purple)))))
   `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation))))
   `(company-tooltip-selection ((t (:foreground ,foreground :background ,gray))))
   `(company-scrollbar-fg ((t (:background ,comment))))
   `(company-scrollbar-bg ((t (:background ,background))))
   `(company-preview ((t (:background ,comment :foreground "blue"))))
   `(company-preview-common ((t (:inherit company-preview :foreground ,purple))))
   `(company-preview-search ((t (:inherit company-preview :background ,background))))

   ;; doom-modeline
   `(doom-modeline-bar ((t (:background nil))))
   `(doom-modeline-bar-inactive ((t (:background nil))))

  (custom-theme-set-variables
   'iceberg
   ;; TODO ANSI colors
   ))


;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
				  (file-name-as-directory
				   (file-name-directory load-file-name))))

(provide-theme 'iceberg)


;;; iceberg-theme.el ends here
