;;; 50-neotree.el --- Neotree configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf neotree
  :ensure t
  :doc "Tree plugin like NerdTree for Vim."
  :hook (neotree-mode-hook . (lambda () (setq-local mode-line-format nil)))
  :bind ((:neotree-mode-map
	  ("RET"     . neotree-enter-hide)
	  ("j"       . next-line)
	  ("k"       . previous-line)
	  ("a"       . neotree-hidden-file-toggle)
	  ("<left>"  . neotree-select-up-node)
	  ("<right>" . neotree-change-root)
	  ("<f6>"   . my-neotree-toggle)))
  :init
  (setq neo-keymap-style 'concise)
  ;; concise にするとキーバインドをシンプルにできる
  ;;  C ルートディレクトリ変更
  ;;  c 作成
  ;;  + 作成
  ;;  d 削除
  ;;  r リネーム
  ;;  e エンター
  :config
  (with-eval-after-load 'doom-themes
    (doom-themes-neotree-config))
  (setq neo-mode-line-type nil)
  (setq neo-create-file-auto-open t)

(defun my-neotree-toggle ()
  "Toggle Neotree, jumping to current file/dir.
Bound to F7; see 10-functions.el."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((path (or (buffer-file-name)
                    (and (eq major-mode 'dired-mode) (dired-current-directory))
                    default-directory)))
      (neotree-show)
      (neotree-find path))))

  (defun neotree-text-scale ()
    "Neotree text scale.
see https://github.com/jaypei/emacs-neotree/issues/218"
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1)
    (message nil))
  (add-hook 'neo-after-create-hook
	    (lambda (_)
	      (neotree-text-scale)))

  (defun neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun neotree-enter-hide (&optional arg)
    "Enters file and hides neotree directly."
    (interactive "P")
    (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-neotree.el ends here
