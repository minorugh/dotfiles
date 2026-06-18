;;; 08-edit.el --- Editing configurations.      -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Compilation
;;;
;;;  Smart handler: auto-closes window on success, surfaces
;;;  ##> markers as echo-area messages, scrolls output in real time.
;;; ============================================================

(defun compile-autoclose (buffer string)
  "Auto-close compile window if BUFFER finished successfully.
STRING is the exit status message from the compilation process."
  (if (and (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string))
      (let ((msg (with-current-buffer buffer
                   (save-excursion
                     (goto-char (point-max))
                     (if (re-search-backward "^##>\\(.*\\)$" nil t)
                         (match-string 1)
                       "Compile successful.")))))
        (message "%s" msg)
        (if (string-equal msg "")
            ;; ##> 単体のとき → バッファを全画面表示
            (run-at-time 0.1 nil (lambda ()
                                   (switch-to-buffer buffer)
                                   (delete-other-windows)))
          ;; ##> + メッセージ or 通常成功 → ウィンドウを閉じる
          (delete-windows-on buffer)))
    ;; 失敗時
    (message "Compilation exited abnormally: %s" string)))

(setq compilation-finish-functions #'compile-autoclose)
(setq compilation-scroll-output    t)
(setq compilation-always-kill      t)

;; ##> 単体行を不可視化（バッファには残りシグナルとして機能する）
(defun my-dim-compilation-marker ()
  "Make bare ##> lines invisible in the compilation buffer."
  (save-excursion
    (goto-char compilation-filter-start)
    (while (re-search-forward "^##>[ \t]*$" nil t)
      (put-text-property (line-beginning-position)
                         (line-end-position)
                         'invisible t))))
(add-hook 'compilation-filter-hook #'my-dim-compilation-marker)


;;; ============================================================
;;;  Auto Save
;;; ============================================================

(leaf super-save
  :ensure t
  :doc "Smart auto-save buffers on focus loss and idle."
  :hook (after-init-hook . super-save-mode)
  :config
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration       1)
  (setq super-save-remote-files        nil)
  (setq super-save-exclude             '(".gpg")))


;;; ============================================================
;;;  Scratch Buffer Persistence
;;; ============================================================

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


;;; ============================================================
;;;  Undo
;;; ============================================================

(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
         ("C-/" . undo-fu-only-redo)))

(leaf undohist
  :ensure t
  :doc "Persist undo history across sessions."
  :hook (after-init-hook . undohist-initialize)
  :config
  (setq undohist-directory     (locate-user-emacs-file "tmp/undohist"))
  (setq undohist-ignored-files '("/tmp/" "COMMIT_EDITMSG")))


;;; ============================================================
;;;  Tempbuf
;;;  I had removed it once,
;;;  but brought it back to work with the rsync lock feature (60-xsrv-dired).
;;; ============================================================

(leaf tempbuf
  :doc "Auto kill unused buffers in the background"
  :vc (:url "https://github.com/minorugh/tempbuf")
  :hook ((find-file-hook  . turn-on-tempbuf-mode)
         (dired-mode-hook . turn-on-tempbuf-mode))
  :config
  (setq tempbuf-kill-message nil))


;;; ============================================================
;;;  Diff / Ediff
;;; ============================================================

(leaf ediff
  :tag "builtin"
  :doc "Side-by-side diff editing."
  :hook (ediff-mode-hook . dimmer-off)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options          "-twB"))


;;; ============================================================
;;;  Electric Modes
;;; ============================================================

(leaf elec-pair
  :tag "builtin"
  :doc "Auto-pair parentheses.  Disabled in text-mode (yasnippet handles it)."
  :hook ((after-init-hook . electric-pair-mode)
         (text-mode-hook  . (lambda () (electric-pair-local-mode -1)))))

(leaf electric-indent
  :tag "builtin"
  :doc "Auto-indent on newline.  Already ON; declared for documentation."
  :hook (after-init-hook . electric-indent-mode))

(leaf indent-helper
  :bind* ("C-c i" . indent-buffer)
  :init
  (defun indent-buffer ()
    "Indent the entire buffer."
    (interactive)
    (save-excursion
      (indent-region (point-min) (point-max))
      (message "Indented buffer."))))


;;; ============================================================
;;;  Region / Selection
;;; ============================================================

(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))

;; my-selected-mode
;; --------------------
(defvar my-selected-mode-map (make-sparse-keymap)
  "Keymap active only while a region is selected.")

(define-minor-mode my-selected-mode
  "Minor mode auto-enabled during region selection."
  :keymap my-selected-mode-map)

(defun my-selected-mode-update ()
  "Toggle `my-selected-mode' based on whether a region is active."
  (if (use-region-p)
      (unless my-selected-mode (my-selected-mode 1))
    (when my-selected-mode (my-selected-mode -1))))

(add-hook 'post-command-hook #'my-selected-mode-update)

;; Web Search Helpers
;; --------------------
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

;; Region Keybindings
;; --------------------
(define-key my-selected-mode-map (kbd ";") #'comment-dwim)
(define-key my-selected-mode-map (kbd "c") #'kill-ring-save)
(define-key my-selected-mode-map (kbd "s") #'swiper-region)
(define-key my-selected-mode-map (kbd "g") #'my-google-search)
(define-key my-selected-mode-map (kbd "w") #'my-weblio-search)
(define-key my-selected-mode-map (kbd "d") #'deepl-translate)


;;; ============================================================
;;;  IME 自動 OFF  (リージョン選択時)
;;;
;;;  emacs-state では mozc ON のままだとリージョン選択後のキー入力が
;;;  my-selected-mode-map より mozc に横取りされるため、選択開始時に
;;;  一時的に IME を OFF にし、選択解除後に元の状態へ戻す。
;;; ============================================================

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


;;; ============================================================
;;;  Flymake  (on-the-fly syntax checking)
;;; ============================================================

(leaf flymake
  :tag "builtin"
  :doc "On-the-fly syntax checking."
  :hook ((prog-mode-hook     . flymake-mode)
         (markdown-mode-hook . flymake-mode)
         (lisp-interaction-mode-hook . (lambda () (flymake-mode 0))))
  :config
  ;; Suppress "untrusted content" warning in flymake-log
  (with-eval-after-load 'elisp-mode
    (advice-add 'elisp-flymake-byte-compile :around
                (lambda (orig-fun report-fn &rest args)
                  (condition-case nil
                      (apply orig-fun report-fn args)
                    (user-error nil))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 08-edit.el ends here
