;;; 10-selected.el --- Region selected configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;; Region Selection Mode
;; ============================================================

(defvar my-selected-mode-map (make-sparse-keymap)
  "Keymap active only while a region is selected.")

(define-minor-mode my-selected-mode
  "Minor mode auto-enabled during region selection."
  :keymap my-selected-mode-map)

;; キーバインド（モード定義直後にまとめる）
(define-key my-selected-mode-map (kbd ";") #'comment-dwim)
(define-key my-selected-mode-map (kbd "c") #'kill-ring-save)
(define-key my-selected-mode-map (kbd "s") #'swiper-region)
(define-key my-selected-mode-map (kbd "g") #'my-google-search)
(define-key my-selected-mode-map (kbd "w") #'my-weblio-search)
(define-key my-selected-mode-map (kbd "d") #'deepl-translate)

;; 有効化トリガー
(defun my-selected-mode-update ()
  "Toggle `my-selected-mode' based on whether a region is active."
  (if (use-region-p)
      (unless my-selected-mode (my-selected-mode 1))
    (when my-selected-mode (my-selected-mode -1))))

(add-hook 'post-command-hook #'my-selected-mode-update)

;; Web Search Helpers ────────────────────────────────────────────
;; my-selected-mode-map から呼ばれるので上で define-key より前に defun が必要。
;; ここに置く場合は (declare-function ...) か、ファイル先頭に移動する。

(defun my-get-region-or-word ()
  "Return active region text, or word at point."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'word t)))

(defun my-google-search (str)
  "Google STR."
  (interactive (list (my-get-region-or-word)))
  (browse-url (format "https://google.com/search?q=%s" (url-hexify-string str))))

(defun my-weblio-search (str)
  "Weblio search for STR."
  (interactive (list (my-get-region-or-word)))
  (browse-url (format "https://www.weblio.jp/content/%s" (url-hexify-string str))))


;; IME 自動 OFF（リージョン選択時）────────────────────────────────
;; emacs-state では mozc ON のままだとリージョン選択後のキー入力が
;; my-selected-mode-map より mozc に横取りされるため、選択開始時に
;; 一時的に IME を OFF にし、選択解除後に元の状態へ戻す。

(defvar-local my-ime-flag nil
  "Non-nil means IME was active before region activation.")

(add-hook 'activate-mark-hook
          (lambda ()
            (setq my-ime-flag current-input-method)
            (deactivate-input-method)))

(add-hook 'deactivate-mark-hook
          (lambda ()
            (when my-ime-flag
              (toggle-input-method))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-selected.el ends here
