;;; 20-region-action.el --- Region action configurations. -*- lexical-binding: t -*-
;;; Commentary:
;; Emacs-state でのリージョン選択中だけ有効なキーマップと IME 自動 OFF 機能。
;; Evil の visual-state-map と同等のキーバインドを emacs-state でも使えるようにする。
;;; Code:

;;; --- マイナーモード（リージョン選択中のみ有効）---

(defvar my-selected-mode-map (make-sparse-keymap)
  "Keymap valid only during region selection.")

(define-minor-mode my-selected-mode
  "Minor mode auto-enabled during region selection."
  :keymap my-selected-mode-map)

;;; --- キーバインド（evil-visual-state-map に対応）---

(define-key my-selected-mode-map (kbd ";") #'comment-dwim)
(define-key my-selected-mode-map (kbd "c") #'kill-ring-save)
(define-key my-selected-mode-map (kbd "s") #'swiper-region)
(define-key my-selected-mode-map (kbd "g") #'my-google-search)
(define-key my-selected-mode-map (kbd "w") #'my-weblio-search)
(define-key my-selected-mode-map (kbd "d") #'deepl-translate)

;;; --- ユーティリティ関数 ---

(defun my-get-region-or-word ()
  "Return active region text, or word at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun my-google-search (str)
  "Google search for STR."
  (interactive (list (my-get-region-or-word)))
  (browse-url (format "https://google.com/search?q=%s" (url-hexify-string str))))

(defun my-weblio-search (str)
  "Weblio search for STR."
  (interactive (list (my-get-region-or-word)))
  (browse-url (format "https://www.weblio.jp/content/%s" (url-hexify-string str))))

;;; --- リージョン連動でモードを ON/OFF ---

(defun my-selected-mode-update ()
  "Toggle `my-selected-mode' based on whether a region is active."
  (if (use-region-p)
      (unless my-selected-mode (my-selected-mode 1))
    (when my-selected-mode (my-selected-mode -1))))

(add-hook 'post-command-hook #'my-selected-mode-update)

;;; --- IME 自動 OFF（emacs-state では mozc ON のままだとリージョン選択後のキー入力が
;;;                  my-selected-mode-map より mozc に横取りされるため）---

(defvar my-ime-flag nil
  "Non-nil means IME was active before region activation.")

(add-hook 'activate-mark-hook
          (lambda ()
            (setq my-ime-flag current-input-method)
            (deactivate-input-method)))

(add-hook 'deactivate-mark-hook
          (lambda ()
            (unless (null my-ime-flag)
              (toggle-input-method))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-region-action.el ends here
