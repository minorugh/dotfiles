;;; 20-region-action.el --- Region action configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(defvar my-selected-mode-map (make-sparse-keymap)
  "Keymap valid only during region selection.")
(defvar my-ime-flag nil
  "Non-nil means IME was active before region activation.")

(leaf my-region-actions
  :doc "リージョン選択中のみ有効なキーバインド（標準機能のみ）"
  :tag "builtin"
  :preface
  (defun my-get-region-or-word ()
    (if (use-region-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (thing-at-point 'word t)))

  (defun my-google-search (str)
    (interactive (list (my-get-region-or-word)))
    ;; ?q=%s を追加することで検索ワードがGoogleに渡ります
    (browse-url (format "https://google.com/search?q=%s" (url-hexify-string str))))

  (defun my-weblio-search (str)
    (interactive (list (my-get-region-or-word)))
    ;; content/%s を追加することで直接その単語のページに飛びます
    (browse-url (format "https://www.weblio.jp/content/%s" (url-hexify-string str))))

  ;; --- 選択中専用のマイナーモードを自作（これが一番確実） ---
  (define-minor-mode my-selected-mode
    "リージョン選択中のみ自動で有効になるモード"
    :lighter " [SEL]"
    :keymap my-selected-mode-map)

  ;; キー割り当て
  (define-key my-selected-mode-map (kbd ";") 'comment-dwim)
  (define-key my-selected-mode-map (kbd "c") 'kill-ring-save)
  (define-key my-selected-mode-map (kbd "g") 'my-google-search)
  (define-key my-selected-mode-map (kbd "w") 'my-weblio-search)
  (define-key my-selected-mode-map (kbd "d") 'deepl-translate)
  ;; DeepLの自作関数があればここに追加
  (defun my-selected-mode-update ()
    "リージョンの状態に合わせてモードをオンオフする"
    (if (use-region-p)
        (unless my-selected-mode (my-selected-mode 1))
      (when my-selected-mode (my-selected-mode -1))))

  :config
  ;; リージョンの変化（選択開始・解除）を監視してモードを切り替える
  (add-hook 'post-command-hook #'my-selected-mode-update)

  ;; IME制御（既存のものを維持）
  (add-hook 'activate-mark-hook (lambda () (setq my-ime-flag current-input-method) (deactivate-input-method)))
  (add-hook 'deactivate-mark-hook (lambda () (unless (null my-ime-flag) (toggle-input-method)))))

;; (leaf selected :ensure t
;;   :doc "Keymap for when region is active."
;;   :url "http://github.com/Kungsgeten/selected.el"
;;   :hook (after-init-hook . selected-global-mode)
;;   :bind (("C-c g" . my-google-this)
;; 	 (:selected-keymap
;; 	  (";" . comment-dwim)
;; 	  ("c" . clipboard-kill-ring-save)
;; 	  ("y" . clipboard-kill-ring-save)
;; 	  ("s" . swiper-thing-at-point)
;; 	  ("d" . deepl-translate)
;; 	  ("t" . google-translate-auto)
;; 	  ("w" . my-weblio)
;; 	  ("g" . my-google-this)))
;;   :init
;;   (defun my-activate-selected ()
;;     (selected-global-mode 1)
;;     (selected--on)
;;     (remove-hook 'activate-mark-hook #'my-activate-selected))

;;   (defun my-ime-on ()
;;     (interactive)
;;     (when (null current-input-method)
;;       (toggle-input-method)))

;;   (defun my-ime-off ()
;;     (interactive)
;;     (deactivate-input-method))

;;   (defun my-weblio (str)
;;     "Search weblio."
;;     (interactive (list (region-or-read-string nil)))
;;     (browse-url (format "https://www.weblio.jp/content/%s"
;; 			(upcase (url-hexify-string str)))))

;;   (defun region-or-read-string (prompt &optional initial history default inherit)
;;     "If region is specified, get the string, otherwise call `read-string'."
;;     (if (not (region-active-p))
;; 	(read-string prompt initial history default inherit)
;;       (prog1
;; 	  (buffer-substring-no-properties (region-beginning) (region-end))
;; 	(deactivate-mark)
;; 	(message ""))))

;;   (leaf google-this :ensure t
;;     :doc "Google search at region or under point."
;;     :config
;;     (defun my-google-this ()
;;       "Run without confirmation."
;;       (interactive)
;;       (google-this (current-word) t)))

;;   :config
;;   (defvar my-ime-flag nil)
;;   (add-hook 'activate-mark-hook   #'my-activate-selected)
;;   (add-hook 'activate-mark-hook   (lambda () (setq my-ime-flag current-input-method) (my-ime-off)))
;;   (add-hook 'deactivate-mark-hook (lambda () (unless (null my-ime-flag) (my-ime-on)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-region-action.el ends here
