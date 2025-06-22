;;; 40-neotree.el --- Neotree configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf neotree :ensure t
  :doc "Tree plugin like NerdTree for Vim"
  :after projectile
  :defun neo-buffer--execute neo-global--select-mru-window dimmer-off dimmer-on
  :bind (("<f10>" . my:neotree-find)
	 (:neotree-mode-map
	  ("RET"     . neotree-enter-hide)
	  ("j"       . next-line)
	  ("k"       . previous-line)
	  ("a"       . neotree-hidden-file-toggle)
	  ("<left>"  . neotree-select-up-node)
	  ("<right>" . neotree-change-root)
	  ("<f10>"   . neotree-toggle)))
  :init
  (setq neo-keymap-style 'concise)
  ;; conciseにするとキーバインドをシンプルにできる
  ;;  C ルートディレクトリ変更
  ;;  c 作成
  ;;  + 作成
  ;;  d 削除
  ;;  r リネーム
  ;;  e エンター
  :config
  (with-eval-after-load 'doom-themes
    (doom-themes-neotree-config)) ;; need all-the-icons
  (setq neo-mode-line-type nil)
  (setq neo-create-file-auto-open t)
  ;; (setq neo-theme 'nerd-icons)
  (defun my:neotree-find ()
    "Neotree-find with dimmer-off."
    (interactive)
    (dimmer-off)
    (neotree-find))

  (defun neotree-text-scale ()
    "Neotree text scale.
  see https://github.com/jaypei/emacs-neotree/issues/218"
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1)
    (message nil))
  (add-hook 'neo-after-create-hook
	    (lambda (_)
	      (call-interactively 'neotree-text-scale)))

  (defun neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide)
    (dimmer-on))

  (defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))

(provide '40-neotree)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 40-neotree.el ends here
