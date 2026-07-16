;;; 09-makefile.el --- Makefile integration and target launcher. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ============================================================
;;  Compilation
;;
;;  Smart handler: auto-closes window on success, surfaces
;;  ##> markers as echo-area messages, scrolls output in real time.
;; ============================================================

(leaf *compile
  :hook (compilation-filter-hook . my-dim-compilation-marker)
  :preface
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

  ;; ##> 単体行を不可視化（バッファには残りシグナルとして機能する）
  (defun my-dim-compilation-marker ()
    "Make bare ##> lines invisible in the compilation buffer."
    (save-excursion
      (goto-char compilation-filter-start)
      (while (re-search-forward "^##>[ \t]*$" nil t)
        (put-text-property (line-beginning-position)
                           (line-end-position)
                           'invisible t))))
  :init
  (setq compilation-finish-functions #'compile-autoclose)
  (message "DEBUG: compilation-finish-functions set to %S" compilation-finish-functions)
  (setq compilation-finish-functions #'compile-autoclose)
  (setq compilation-scroll-output    t)
  (setq compilation-always-kill      t))


;; ============================================================
;;  Makefile Functions
;;
;;  Ivy-powered target launcher with live preview.
;;  Works in makefile-mode, dired, and any buffer under a
;;  Makefile root.  Toggle read-only/evil-state in one keystroke.
;; ============================================================

(leaf *makefile-functions
  :after (evil key-chord)
  :hook ((makefile-mode-hook . my-makefile-mode-setup)
         (dired-mode-hook    . my-dired-mode-setup))
  :preface
  (defun my-makefile-mode-setup ()
    "Setup keybindings for `makefile-mode'."
    (local-set-key (kbd "C-c C-e") #'my-makefile-toggle-readonly)
    (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated)
    (key-chord-define (current-local-map) "qq" #'my-makefile-toggle-readonly))

  (defun my-dired-mode-setup ()
    "Setup keybindings for `dired-mode'."
    (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated)))


;; ============================================================
;;  Makefile Target Picker  (Ivy integrated)
;; ============================================================

(leaf *make-target
  :after ivy
  :preface
  ;; Resolve Makefile path from dired, buffer file, or default-directory
  (defun my-make--find-makefile ()
    "Return path to Makefile for the current context, or nil if not found."
    (let ((dir (cond
                ((derived-mode-p 'dired-mode) (dired-current-directory))
                ((and buffer-file-name
                      (string= (file-name-nondirectory buffer-file-name) "Makefile"))
                 (file-name-directory buffer-file-name))
                (t default-directory))))
      (let ((mk (expand-file-name "Makefile" dir)))
        (when (file-exists-p mk) mk))))

  (defun my-make-ivy-integrated ()
    "Select and run a Makefile target via Ivy with live preview."
    (interactive)
    (let ((makefile (my-make--find-makefile)))
      (unless makefile (user-error "Makefileが見つかりません"))
      (let ((candidates nil)
            (orig-buf   (current-buffer))
            (orig-point (point))
            (map        (copy-keymap ivy-minibuffer-map)))
        ;; Real-time preview on arrow keys
        (keymap-set map "<down>" 'ivy-next-line-and-call)
        (keymap-set map "<up>"   'ivy-previous-line-and-call)
        ;; C-c C-c to execute make
        (keymap-set map "C-c C-c"
                    (lambda ()
                      (interactive)
                      (ivy-exit-with-action
                       (lambda (x)
                         (let ((target (cdr x)))
                           (compile (format "make -C %s %s"
                                            (file-name-directory makefile)
                                            target)))))))
        ;; Parse targets annotated with ## from Makefile
        (with-current-buffer (find-file-noselect makefile)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
              (let* ((target     (match-string 1))
                     (desc       (match-string 2))
                     (pos        (match-beginning 1))
                     (target-fmt (propertize (format "%-09s" target)
                                             'face 'font-lock-function-name-face))
                     (desc-fmt   (propertize desc 'face 'font-lock-comment-face)))
                (push (cons (concat target-fmt " " desc-fmt)
                            (propertize target 'pos pos 'makefile makefile))
                      candidates)))))
        (if (not candidates)
            (message "ターゲットが見つかりませんでした。")
          (ivy-read "Targets: "
                    (nreverse candidates)
                    :keymap map
                    :action (lambda (x)
                              (let ((pos (get-text-property 0 'pos (cdr x)))
                                    (mk  (get-text-property 0 'makefile (cdr x))))
                                (find-file mk)
                                (goto-char pos)
                                (recenter)))
                    :unwind (lambda ()
                              (unless (eq ivy-exit 'done)
                                (switch-to-buffer orig-buf)
                                (goto-char orig-point)
                                (recenter)))
                    :caller 'my-make-ivy-integrated))))))


;; ============================================================
;;  Makefile Utilities
;; ============================================================

(leaf *make-utils
  :hook (buffer-list-update-hook . my-makefile-buffer-list-update-hook)
  :preface
  (defun my-makefile-toggle-readonly ()
    "Toggle read-only mode and switch evil state accordingly."
    (interactive)
    (read-only-mode 'toggle)
    (if (eq evil-state 'normal)
        (evil-emacs-state)
      (evil-normal-state))
    (unless buffer-read-only (message "EDITABLE")))

  (defun my-make-git ()
    "Run `make git' in the repository root."
    (interactive)
    (let* ((dir  (or buffer-file-name default-directory))
           (root (locate-dominating-file dir "Makefile")))
      (if root
          (let ((default-directory root))
            (compile "make git"))
        (message "Makefile not found"))))

  (defun my-makefile-buffer-list-update-hook ()
    "カレントから外れた Makefile バッファを自動 read-only に戻す."
    (dolist (buf (buffer-list))
      (unless (eq buf (current-buffer))
        (with-current-buffer buf
          (when (and (derived-mode-p 'makefile-mode)
                     (not buffer-read-only))
            (read-only-mode 1)
            (evil-normal-state)))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-makefile.el ends here
