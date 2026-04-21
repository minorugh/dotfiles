;;; 10-compile.el --- Compilation functions.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ================================
;; Makefile
;; ================================
(defun my-makefile-toggle-readonly ()
  "Toggle read-only mode in Makefile buffer."
  (interactive)
  (read-only-mode 'toggle)
  (message "Makefile: %s" (if buffer-read-only "read-only" "EDITABLE")))

(defun my-make-git ()
  "Run `make git' in the repository root of the current buffer."
  (interactive)
  (let* ((dir (or buffer-file-name default-directory))
         (root (locate-dominating-file dir "Makefile")))
    (if root
        (let ((default-directory root))
          (compile "make git"))
      (message "Makefile not found"))))

(leaf makefile-mode
  :tag "builtin"
  :doc "Makefile editing: TAB inserts tab, qq/C-c C-e toggle read-only."
  :hook (makefile-mode-hook . my-makefile-setup)
  :init
  (defun my-makefile-setup ()
    "Setup for Makefile editing."
    (setq-local indent-tabs-mode t)
    (local-set-key (kbd "TAB")     'self-insert-command)
    (local-set-key (kbd "C-c C-e") 'my-makefile-toggle-readonly)
    (key-chord-define (current-local-map) "qq" 'my-makefile-toggle-readonly)))

(leaf my-makefile
  :doc "ivy-based Makefile target selector."
  :require t
  :chord ("@@" . my-open-cron-makefile)
  :hook ((makefile-mode-hook dired-mode-hook)
         . (lambda () (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)))
  :init
  (defun my-open-cron-makefile ()
    "Open ~/src/github.com/minorugh/dotfiles/cron/Makefile and invoke my-make-ivy."
    (interactive)
    (let ((file (expand-file-name "~/src/github.com/minorugh/dotfiles/cron/Makefile")))
      (find-file file)
      (evil-local-set-key 'normal (kbd "@") #'my-make-ivy)
      (run-at-time 0.1 nil #'my-make-ivy))))

;; ================================
;; Compilation
;; ================================
(leaf compilation
  :doc "Auto-close compilation window on success."
  :bind (:compilation-mode-map
	 ("q" .  quit-window))
  :config
  (setq compilation-scroll-output t)
  (setq compilation-always-kill   t)

  ;; Makefile の @echo ルール:
  ;;   @echo "##>" >&2           → 全画面表示（q で閉じる）
  ;;   @echo "##> メッセージ" >&2 → ウィンドウを閉じてminibufferにメッセージ表示
  ;;   echo なし                 → "Compile successful." をminibufferに表示して閉じる
  (defun compile-autoclose (buffer string)
    "Auto-close compile window if BUFFER finished successfully.
Echo the last @echo output line to the minibuffer."
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
              (run-at-time 0.1 nil (lambda ()
                                     (switch-to-buffer buffer)
                                     (delete-other-windows)))
            (delete-windows-on buffer)))
      (message "Compilation exited abnormally: %s" string)))
  (setq compilation-finish-functions #'compile-autoclose)

  ;; ##> 単体行を不可視化（バッファには残るのでシグナルとして機能する）
  (defun my-dim-compilation-marker ()
    "Make bare ##> lines invisible in compilation buffer."
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward "^##>[ \t]*$" nil t)
        (put-text-property (line-beginning-position)
                           (line-end-position)
                           'invisible t))))
  (add-hook 'compilation-filter-hook #'my-dim-compilation-marker))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-compile.el ends here
