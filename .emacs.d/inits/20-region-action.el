;;; 20-region-action.el --- Region action configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(defvar my-selected-mode-map (make-sparse-keymap)
  "Keymap valid only during region selection.")
(defvar my-ime-flag nil
  "Non-nil means IME was active before region activation.")

(define-minor-mode my-selected-mode
  "Mode automatically enabled only during region selection."
  :keymap my-selected-mode-map)

(defun my-region-actions-initialize ()
  "Load region actions on first region selection."
  (remove-hook 'activate-mark-hook #'my-region-actions-initialize))

(leaf my-region-actions
  :tag "builtin"
  :doc "Key bindings valid only during region selection"
  :config
  (bind-key "s" 'consult-line-region my-selected-mode-map)
  (bind-key ";" 'comment-dwim     my-selected-mode-map)
  (bind-key "c" 'kill-ring-save   my-selected-mode-map)
  (bind-key "g" 'my-google-search my-selected-mode-map)
  (bind-key "w" 'my-weblio-search my-selected-mode-map)
  (bind-key "d" 'deepl-translate  my-selected-mode-map)

  (defun my-get-region-or-word ()
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'word t)))

  (defun my-google-search (str)
    (interactive (list (my-get-region-or-word)))
    (browse-url (format "https://google.com/search?q=%s" (url-hexify-string str))))

  (defun my-weblio-search (str)
    (interactive (list (my-get-region-or-word)))
    (browse-url (format "https://www.weblio.jp/content/%s" (url-hexify-string str))))

  (defun my-selected-mode-update ()
    (if (use-region-p)
        (unless my-selected-mode (my-selected-mode 1))
      (when my-selected-mode (my-selected-mode -1))))

  (add-hook 'post-command-hook    #'my-selected-mode-update)
  (add-hook 'activate-mark-hook
            (lambda () (setq my-ime-flag current-input-method) (deactivate-input-method)))
  (add-hook 'deactivate-mark-hook
            (lambda () (unless (null my-ime-flag) (toggle-input-method)))))

(setq my-ime-flag current-input-method)
(deactivate-input-method)

(add-hook 'activate-mark-hook #'my-region-actions-initialize)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-region-action.el ends here
