;;; 09-makefile.el --- Makefile integration and target launcher (test). -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

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

  (defun my-make-show-log (logfile status)
    "Load LOGFILE (produced by make-run.sh's gnome-terminal branch)
into a compilation-mode buffer and display it. STATUS is the
exit code of the underlying make invocation. Intended to be
invoked remotely via `emacsclient -e'."
    (let ((buf (get-buffer-create "*make-run-log*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (when (file-readable-p logfile)
            (insert-file-contents logfile))
          (goto-char (point-max)))
        (compilation-mode)
        ;; タイムスタンプ等が file:line 形式に誤爆してリンク化・赤字化される
        ;; のを防ぐため、このバッファでは compilation のエラー検出自体を無効化する
        ;; (静的なログ表示なので Makefile:NN のクリックジャンプは諦める)
        (setq-local compilation-error-regexp-alist nil)
        (font-lock-flush)
        (font-lock-ensure)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^##>[ \t]*$" nil t)
            (put-text-property (line-beginning-position)
                               (line-end-position)
                               'invisible t)))
        (setq buffer-read-only t))
      (if (zerop status)
          ;; 成功時: バッファは裏に留め、ミニバッファへメッセージのみ表示
          (let ((msg (with-current-buffer buf
                       (save-excursion
                         (goto-char (point-max))
                         (if (re-search-backward "^##>\\(.*\\)$" nil t)
                             (string-trim (match-string 1))
                           "Compile successful.")))))
            (message "%s" (if (string= msg "") "Compile successful." msg)))
        ;; 失敗時: ログを前面に出して確認できるようにする（他ウィンドウは畳む）
        (switch-to-buffer buf)
        (delete-other-windows)
        (message "make: exited abnormally with code %d (see *compilation-log*)" status)))
    (when (file-exists-p logfile)
      (delete-file logfile))
    nil)

  :init
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
;;
;;  ##! ターゲットも含め、実行は常に make-run.sh 経由に統一。
;;  対話性の有無・Emacs起動かどうかの判定は make-run.sh 側が
;;  行うため、ここでは ivy 候補の ⚠ マーク表示にのみ ##! を使う。
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
        (keymap-set map "C-c C-c"
                    (lambda ()
                      (interactive)
                      (ivy-exit-with-action
                       (lambda (x)
                         (let* ((target (cdr x))
                                (dir    (file-name-directory makefile))
                                (cmd    (format "make-run.sh %s %s"
                                                (shell-quote-argument dir)
                                                target)))
                           (compile cmd))))))
        ;; Parse targets annotated with ## from Makefile
        (with-current-buffer (find-file-noselect makefile)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward
                    "^\\([^:# \t\n]+\\):.*?##\\(!?\\)[ \t]*\\(.*\\)$" nil t)
              (let* ((target       (match-string 1))
                     (interactive-p (string= (match-string 2) "!"))
                     (desc         (match-string 3))
                     (pos          (match-beginning 1))
                     (target-fmt   (propertize (format "%-09s" target)
                                               'face 'font-lock-function-name-face))
                     (desc-fmt     (propertize desc 'face 'font-lock-comment-face))
                     (mark-fmt     (if interactive-p
                                       (propertize " ⚠" 'face 'warning)
                                     "")))
                (push (cons (concat target-fmt " " desc-fmt mark-fmt)
                            (propertize target
                                       'pos pos 'makefile makefile
                                       'interactive-p interactive-p))
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
