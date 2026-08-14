;;; 09-makefile.el --- Makefile integration and target launcher (test). -*- lexical-binding: t -*-
;;; Commentary:
;;
;; 全体構成(上から下へ依存する):
;;
;;   *compile ....... 出力の「見せ方」。##> マーカー規約の共通ヘルパーと、
;;                     ライブ compile 用ハンドラ(compile-autoclose)、
;;                     make-run.sh 用の静的ログビューア(my-make-show-log)
;;                     を持つ。my-make-show-log は emacsclient 経由で
;;                     make-run.sh 自身から呼ばれる(このファイル内からは
;;                     直接呼ばない)。
;;        ↓ 使う
;;   *make-run ...... 出力の「見せ方」を使って make-run.sh を「実行する」
;;                     関数を2種類提供する:
;;                       - my-make-run-compile (compile 経由、ivy 用)
;;                       - my-make-run-async   (撃ちっぱなし、hydra 用)
;;                     後者は 80-hydra-dired.el の my-make-git からも呼ばれる。
;;        ↓ 使う
;;   *makefile-mode . makefile-mode / Dired での UX(@ キーで *make-target
;;                     を起動、read-only の自動管理など)。
;;        ↓ 使う
;;   *make-target ... @ で起動する ivy ターゲットピッカー本体。選択した
;;                     ターゲットを my-make-run-compile 経由で実行する。
;;
;; 実行経路は最終的に3系統:
;;   (1) @ → ivy → C-c C-c            → my-make-run-compile → compile
;;   (2) hydra "]" (my-make-git)       → my-make-run-async   → start-process
;;   (3) ターミナルから make-run.sh を直接叩く(Emacs 非経由)
;; いずれも make-run.sh 側で ##! 判定・gnome-terminal 委譲を行う。
;;
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Compilation Output Handling
;;
;;  ##> マーカー規約(共通ヘルパー) + ライブ compile 用スマートハンドラ
;;  (成功時は自動クローズ、失敗時はメッセージ表示) + make-run.sh の
;;  gnome-terminal 実行結果を静的ログとして表示する my-make-show-log。
;;
;;  ##> マーカーはターゲット側のレシピで自由に `echo '##> ...'` すれば
;;  拾われる(##> だけの単体行はバッファ上では不可視化されるが、
;;  「メッセージなしの成功」を示すシグナルとして機能する)。
;; ============================================================

(leaf *compile
  :hook (compilation-filter-hook . my-dim-compilation-marker)
  :preface
  ;; ---- ##> マーカー共通ヘルパー --------------------------------
  ;; ライブ compile 出力(my-dim-compilation-marker)と
  ;; make-run.sh の静的ログ(my-make-show-log)の両方から使う。
  (defun my-make--hide-marker-lines (start end)
    "Make bare ##> lines invisible between START and END."
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^##>[ \t]*$" end t)
        (put-text-property (line-beginning-position)
                           (line-end-position)
                           'invisible t))))

  (defun my-make--marker-message (&optional buffer)
    "Return the trailing ##> marker message in BUFFER (default: current
buffer), or nil if the buffer contains no ##> marker line at all.
A bare `##>' line (no text after it) returns the empty string."
    (with-current-buffer (or buffer (current-buffer))
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^##>\\(.*\\)$" nil t)
          (string-trim (match-string 1))))))

  ;; ---- ライブ compile 用 ----------------------------------------
  (defun my-dim-compilation-marker ()
    "Make bare ##> lines invisible in the compilation buffer (live output)."
    (my-make--hide-marker-lines compilation-filter-start (point-max)))

  (defun compile-autoclose (buffer string)
    "Auto-close compile window if BUFFER finished successfully.
STRING is the exit status message from the compilation process."
    (if (and (string-match "compilation" (buffer-name buffer))
             (string-match "finished" string))
        (let ((msg (or (my-make--marker-message buffer) "Compile successful.")))
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

  ;; ---- make-run.sh の静的ログ表示(emacsclient から呼ばれる) ----
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
        (my-make--hide-marker-lines (point-min) (point-max))
        (setq buffer-read-only t))
      (if (zerop status)
          ;; 成功時: バッファは裏に留め、ミニバッファへメッセージのみ表示
          (let ((msg (my-make--marker-message buf)))
            (message "%s" (if (or (null msg) (string= msg ""))
                              "Compile successful."
                            msg)))
        ;; 失敗時: ログを前面に出して確認できるようにする（他ウィンドウは畳む）
        (switch-to-buffer buf)
        (delete-other-windows)
        (message "make: exited abnormally with code %d (see *make-run-log*)" status)))
    (when (file-exists-p logfile)
      (delete-file logfile))
    nil)

  :init
  (setq compilation-finish-functions #'compile-autoclose)
  (setq compilation-scroll-output    t)
  (setq compilation-always-kill      t))


;; ============================================================
;;  Makefile Target Runner
;;
;;  make-run.sh の呼び出し口をここに一本化する。
;;   - my-make-run-compile: ivy で選んだターゲット用。`compile' 経由で
;;     追跡するので、非対話(##!なし)ターゲットならその場で出力が流れる。
;;   - my-make-run-async: hydra 等からの「撃ちっぱなし」用。compile
;;     バッファは作らず、start-process + INSIDE_EMACS 偽装で直接実行
;;     する(##! ターゲット専用の使い方を想定)。
;;     80-hydra-dired.el の my-make-git から呼ばれる。
;; ============================================================

(leaf *make-run
  :preface
  (defun my-make--command (dir target)
    "Build the shell command string that runs TARGET in DIR via make-run.sh."
    (format "make-run.sh %s %s"
            (shell-quote-argument (expand-file-name dir))
            target))

  (defun my-make-run-compile (dir target)
    "Run TARGET in DIR via make-run.sh, tracked in a `compile' buffer.
Use this for interactively picked targets (e.g. the ivy picker), where
a plain (non ##!) target's output should stream live into *compilation*."
    (compile (my-make--command dir target)))

  (defun my-make-run-async (dir target)
    "Fire-and-forget: run TARGET in DIR via make-run.sh in the background,
without creating a `compile' buffer. Suitable for quick shortcuts (e.g.
hydra menu items) where only the eventual *make-run-log* (on failure)
or echo message (on success) matters.
INSIDE_EMACS is faked so make-run.sh's from_emacs check still succeeds
even though no real `compile' process is involved."
    (let ((process-environment (cons "INSIDE_EMACS=t" process-environment)))
      (start-process "make-run" nil
                     "make-run.sh" (expand-file-name dir) target))))


;; ============================================================
;;  Makefile Mode Integration
;;
;;  makefile-mode / dired から @ でターゲットピッカーを起動できる
;;  ようキーバインドを仕込み、Makefile の read-only 切り替えや
;;  フォーカスを外れた際の自動 read-only 復帰もあわせて担当する。
;; ============================================================

(leaf *makefile-mode
  :after (evil key-chord)
  :hook ((makefile-mode-hook      . my-makefile-mode-setup)
         (dired-mode-hook         . my-dired-mode-setup)
         (buffer-list-update-hook . my-makefile-buffer-list-update-hook))
  :preface
  (defun my-makefile-mode-setup ()
    "Setup keybindings for `makefile-mode'."
    (local-set-key (kbd "C-c C-e") #'my-makefile-toggle-readonly)
    (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated)
    (key-chord-define (current-local-map) "qq" #'my-makefile-toggle-readonly))

  (defun my-dired-mode-setup ()
    "Setup keybindings for `dired-mode'."
    (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated))

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


;; ============================================================
;;  Makefile Target Picker  (Ivy integrated)
;;
;;  ##! ターゲットも含め、実行は常に make-run.sh 経由(my-make-run-compile)
;;  に統一。対話性の有無・Emacs起動かどうかの判定は make-run.sh 側が
;;  行うため、ここでは ivy 候補の ⚠ マーク表示にのみ ##! を使う。
;; ============================================================

(leaf *make-target
  :after ivy
  :config
  ;; @ Increase the mini-buffer height only when the picker is active
  (add-to-list 'ivy-height-alist '(my-make-ivy-integrated . 20))
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
                                (dir    (file-name-directory makefile)))
                           (my-make-run-compile dir target))))))
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
		    :update-fn 'auto
                    :caller 'my-make-ivy-integrated))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 09-makefile.el ends here
