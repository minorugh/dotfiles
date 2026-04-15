;;; 20-region-action.el --- Region action configurations. -*- lexical-binding: t -*-

(leaf my-region-actions
  :doc "Key bindings valid only during region selection"
  :tag "builtin"
  ;; set-mark-command（C-SPCなど）が呼ばれた時に初めてこのleafの中身をロードする
  :commands set-mark-command
  :advice (:before set-mark-command my-setup-region-actions)
  :config
  (defvar my-selected-mode-map (make-sparse-keymap))
  (defvar my-ime-flag nil)

  (define-minor-mode my-selected-mode
    "Mode automatically enabled only during region selection."
    :lighter " [SEL]"
    :keymap my-selected-mode-map)

  ;; キーバインドの一括設定
  (bind-keys :map my-selected-mode-map
             (";" . comment-dwim)
             ("c" . kill-ring-save)
             ("g" . my-google-search)
             ("w" . my-weblio-search)
             ("d" . deepl-translate))

  (defun my-get-region-or-word ()
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'word t)))

  (defun my-google-search (str)
    (interactive (list (my-get-region-or-word)))
    (browse-url (format "https://google.com" (url-hexify-string str))))

  (defun my-weblio-search (str)
    (interactive (list (my-get-region-or-word)))
    (browse-url (format "https://weblio.jp" (url-hexify-string str))))

  (defun my-selected-mode-update ()
    (if (use-region-p)
        (unless my-selected-mode (my-selected-mode 1))
      (when my-selected-mode (my-selected-mode -1))))

  ;; フックの登録
  (add-hook 'post-command-hook #'my-selected-mode-update)
  (add-hook 'activate-mark-hook (lambda () (setq my-ime-flag current-input-method) (deactivate-input-method)))
  (add-hook 'deactivate-mark-hook (lambda () (unless (null my-ime-flag) (toggle-input-method)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-region-action.el ends here
