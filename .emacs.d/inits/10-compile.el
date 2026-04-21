;;; 10-compile.el --- Compilation functions.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; Makefile commads
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
      (run-at-time 0.1 nil #'my-make-ivy)))

  (defun my-make-git ()
    "Run `make git' in the repository root of the current buffer."
    (interactive)
    (let* ((dir (or buffer-file-name default-directory))
           (root (locate-dominating-file dir "Makefile")))
      (if root
          (let ((default-directory root))
            (compile "make git"))
	(message "Makefile not found"))))

  (defun my-makefile-toggle-readonly ()
    "Toggle read-only mode in Makefile buffer."
    (interactive)
    (read-only-mode 'toggle)
    (message "Makefile: %s" (if buffer-read-only "read-only" "EDITABLE")))

  (add-hook 'makefile-mode-hook
            (lambda ()
              (font-lock-mode 1)
              (local-set-key (kbd "C-c C-e") 'my-makefile-toggle-readonly)
              (key-chord-define (current-local-map) "qq" 'my-makefile-toggle-readonly))))


;; Compilation
(leaf compilation
  :doc "Auto-close compilation window on success."
  :chord (("::" . my-switch-to-compilation))
  :init
  (defun my-switch-to-compilation ()
    (interactive)
    (if-let ((buf (get-buffer "*compilation*")))
        (progn
          (switch-to-buffer buf)
          (local-set-key (kbd "q") #'quit-window))
      (message "*compilation* buffer does not exist.")))

  (defun compile-autoclose (buffer string)
    "Auto-close compile window if BUFFER finished successfully."
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
              ;; ##> 単体のとき → 全画面表示
              (run-at-time 0.1 nil (lambda ()
                                     (switch-to-buffer buffer)
                                     (delete-other-windows)))
            ;; ##> + コメント or echo のないとき → close window
            (delete-windows-on buffer)))
      ;; 失敗時
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
  (add-hook 'compilation-filter-hook #'my-dim-compilation-marker)

  (setq compilation-scroll-output t)
  (setq compilation-always-kill   t))

;; Other
(leaf quickrun :ensure t
  :bind ([f5] . quickrun))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-compile.el ends here
