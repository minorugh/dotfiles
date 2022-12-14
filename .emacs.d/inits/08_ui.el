;;; 08_ui.el --- Better lookings and appearances. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf doom-themes
  :ensure t
  :bind ("s-t" . my:cycle-theme)
  :init
  (leaf iceberg-emacs
	:el-get minorugh/iceberg.emacs)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/el-get/iceberg.emacs")
  (setq my-themes (list 'iceberg 'doom-dracula)	curr-theme my-themes)
  (load-theme (car curr-theme) t)
  (defun my:cycle-theme ()
	"Cycle custom theme."
	(interactive)
	(disable-theme (car curr-theme))
	(setq curr-theme (cdr curr-theme))
	(if (null curr-theme) (setq curr-theme my-themes))
	(load-theme (car curr-theme) t)
	(message "%s" (car curr-theme))))


(leaf doom-modeline
  :ensure t
  :hook (emacs-startup-hook . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-minor-modes nil)
  (line-number-mode 0)
  (column-number-mode 0)
  :init
  (leaf hide-mode-line
	:ensure t
	:hook ((imenu-list-minor-mode-hook neotree-mode-hook) . hide-mode-line-mode))
  (leaf nyan-mode
	:ensure t
	:when window-system
	:global-minor-mode t
	:config
	(autoload 'nyan-mode "nyan-mode" nil t)
	(setq nyan-cat-face-number 4)
	(nyan-start-animation)))


(leaf all-the-icons
  :ensure t
  :config
  (setq all-the-icons-scale-factor 0.9)
  (all-the-icons-ivy-rich-mode)
  :init
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t))
  (leaf all-the-icons-ivy-rich :ensure t)
  (leaf all-the-icons-dired
	:el-get jtbm37/all-the-icons-dired
	:hook (dired-mode-hook . all-the-icons-dired-mode)))


(leaf darkroom
  :ensure t
  :bind ("<f12>" . my:darkroom-mode-in)
  :config
  (defun my:darkroom-mode-in ()
    (interactive)
    (display-line-numbers-mode 0)
    (diff-hl-mode 0)
    (flymake-mode 0)
    (setq line-spacing 0.4)
    (darkroom-mode 1)
    (bind-key "<f12>" 'my:darkroom-mode-out darkroom-mode-map))

  (defun my:darkroom-mode-out ()
    (interactive)
    (darkroom-mode 0)
    (flymake-mode 1)
    (diff-hl-mode 1)
    (setq line-spacing 0.1)
    (display-line-numbers-mode 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 08_ui.el ends here

