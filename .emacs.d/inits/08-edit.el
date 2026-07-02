;;; 08-edit.el --- Editing configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Auto Save
;; ============================================================

(leaf super-save
  :ensure t
  :doc "Smart auto-save buffers on focus loss and idle."
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg")))


;; ============================================================
;;  Scratch Buffer Persistence
;; ============================================================

(leaf save-scratch
  :doc "Save *scratch* contents at shutdown and restore at startup."
  :hook ((kill-emacs-hook . save-scratch-buffer)
         (after-init-hook . restore-scratch-buffer))
  :init
  (defun save-scratch-buffer ()
    "Write *scratch* contents to disk."
    (with-current-buffer "*scratch*"
      (write-region (point-min) (point-max)
                    (locate-user-emacs-file "tmp/scratch"))))

  (defun restore-scratch-buffer ()
    "Restore *scratch* contents from disk if the file exists."
    (let ((f (locate-user-emacs-file "tmp/scratch")))
      (when (file-exists-p f)
        (with-current-buffer "*scratch*"
          (erase-buffer)
          (insert-file-contents f))))))


;; ============================================================
;;  Undo
;; ============================================================

(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-redo)))


;; ============================================================
;;  Diff / Ediff
;; ============================================================

(leaf ediff
  :tag "builtin"
  :doc "Side-by-side diff editing."
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options          "-twB"))


;; ============================================================
;;  Electric Modes
;; ============================================================

(leaf elec-pair
  :tag "builtin"
  :doc "Auto-pair parentheses.  Disabled in text-mode (yasnippet handles it)."
  :hook ((after-init-hook . electric-pair-mode)
         (text-mode-hook  . (lambda () (electric-pair-local-mode -1)))))

(leaf electric-indent
  :tag "builtin"
  :doc "Auto-indent on newline.  Already ON; declared for documentation."
  :hook (after-init-hook . electric-indent-mode))

(leaf my-indent-buffer
  :bind* ("C-c i" . my-indent-buffer)
  :init
  (defun my-indent-buffer ()
    "Untabify and indent the entire buffer."
    (interactive)
    (save-excursion
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (message "Untabified and indented buffer."))))


;; ============================================================
;;  Sequential C-a / C-e
;; ============================================================

(defvar my-seq--count 0)
(defvar my-seq--start nil)

(defmacro my-define-seq-command (name &rest commands)
  "依存なしの sequential-command 代替.
NAME を連続で呼ぶたびに COMMANDS を順番に実行し、最後まで来たら巡回する."
  (let ((cmdvec (apply #'vector commands)))
    `(defun ,name ()
       ,(format "Sequential command: %s" (mapconcat #'symbol-name commands " → "))
       (interactive)
       (if (eq last-command this-command)
           (setq my-seq--count (1+ my-seq--count))
         (setq my-seq--start (cons (point) (window-start))
               my-seq--count 0))
       (call-interactively (aref ,cmdvec (mod my-seq--count ,(length cmdvec)))))))

(defun my-seq-return ()
  "巡回開始前の位置に戻る."
  (interactive)
  (goto-char (car my-seq--start))
  (set-window-start (selected-window) (cdr my-seq--start)))

(my-define-seq-command my-seq-home
                       beginning-of-line back-to-indentation beginning-of-buffer my-seq-return)

(my-define-seq-command my-seq-end
                       end-of-line end-of-buffer my-seq-return)


;; ============================================================
;;  Expand Region
;; ============================================================

(leaf expand-region
  :ensure t
  :bind (("C-@"   . er/expand-region)
         ("C-M-@" . er/contract-region)))


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

(defvar my-ime-flag nil
  "Non-nil means IME was active before region activation.")

(add-hook 'activate-mark-hook
          (lambda ()
            (setq my-ime-flag current-input-method)
            (deactivate-input-method)))

(add-hook 'deactivate-mark-hook
          (lambda ()
            (when my-ime-flag
              (toggle-input-method))))


;; ============================================================
;;  Flychek  (on-the-fly syntax checking)
;; ============================================================

(leaf flycheck
  :ensure t
  :hook ((lisp-interaction-mode-hook . (lambda () (flycheck-mode -1)))
         (prog-mode-hook     . flycheck-mode)
         (markdown-mode-hook . flycheck-mode))
  :config
  (setq flycheck-emacs-lisp-initialize-packages t)
  ;; Fixing leaf-keywords "Unrecognized keyword" error in flycheck
  (eval-and-compile (require 'flycheck))
  (setq flycheck-emacs-lisp-package-initialize-form
        (flycheck-sexp-to-string
         '(progn
            (with-demoted-errors "Error during package initialization: %S"
              (package-initialize))
            (leaf-keywords-init)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 08-edit.el ends here
