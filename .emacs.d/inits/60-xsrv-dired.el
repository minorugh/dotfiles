;;; 60-xsrv-dired.el --- Xserver deploy/backup operations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; ============================================================
;;;  Deploy  (local dired → xserver)
;;;
;;;  キーバインドは 60-dired.el で定義。
;;; ============================================================

(defun xsrv-deploy-dired ()
  "Deploy file at point in `dired' to xserver via deploy.pl."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (file-name-nondirectory file)))
    (cond
     ((file-directory-p file)
      (message "Error: ディレクトリは deploy できません。"))
     ((string-match-p "\\(^Makefile$\\|^README\\|\\.mk$\\|\\.bak$\\)" name)
      (message "Error: %s は deploy 対象外です。" name))
     ((not (or (string-prefix-p "/home/minoru/Dropbox/GH/" file)
               (string-prefix-p "/home/minoru/Dropbox/minorugh.com/" file)))
      (message "Error: deploy 対象外のファイルです。"))
     (t
      (when (x-popup-dialog
             t
             `(,(format "本当に deploy しますか？\n\n  %s" name)
               ("Deploy する" . t)
               ("やめる"      . nil)))
        (shell-command
         (format "perl ~/Dropbox/GH/common/deploy.pl %s" file)))))))


;;; ============================================================
;;;  Download  (xsrv-GH / xsrv-minorugh → local)
;;;
;;;  キーバインドは 60-dired.el で定義。
;;; ============================================================

(defun xsrv-download-dired ()
  "Download file at point from xsrv-GH or xsrv-minorugh to local Dropbox."
  (interactive)
  (let* ((file         (dired-get-filename))
         (name         (file-name-nondirectory file))
         (xsrv-gh-root  "/home/minoru/src/github.com/minorugh/xsrv-GH/")
         (xsrv-mn-root  "/home/minoru/src/github.com/minorugh/xsrv-minorugh/")
         (local-gh-root "/home/minoru/Dropbox/GH/")
         (local-mn-root "/home/minoru/Dropbox/minorugh.com/")
         (local-root (cond
                      ((string-prefix-p xsrv-gh-root file) local-gh-root)
                      ((string-prefix-p xsrv-mn-root file) local-mn-root)
                      (t (user-error "Error: xsrv-GH/xsrv-minorugh の Dired から実行してください"))))
         (xsrv-root (if (string-prefix-p xsrv-gh-root file) xsrv-gh-root xsrv-mn-root))
         (rel  (file-relative-name file xsrv-root))
         (dest (concat local-root rel)))
    (when (x-popup-dialog
           t
           `(,(format "ローカルにダウンロードしますか？\n\n  %s" name)
             ("Download する" . t)
             ("やめる"        . nil)))
      (if (and (file-exists-p dest)
               (not (y-or-n-p (format "%s は既にあります。上書きしますか?" name))))
          (message "キャンセルしました。")
        (copy-file file dest t)
        (message "Downloaded: %s" rel)
        (dolist (root (list xsrv-root local-root))
          (let ((buf (get-buffer (file-name-nondirectory
                                  (directory-file-name root)))))
            (when buf
              (with-current-buffer buf
                (revert-buffer)))))))))


;;; ============================================================
;;;  git-peek  (差分プレビュー)
;;; ============================================================

(leaf git-peek
  :vc (:url "https://github.com/minorugh/git-peek" :only-if-missing t)
  :config
  (setq git-peek-save-dir (expand-file-name "~/tmp/"))

  (defun my-git-peek-smart ()
    "Run `git-peek' with save-dir adjusted for xsrv dired context.
xsrv 配下なら差分表示後に 2ペインを復元する。"
    (interactive)
    (let* ((dir          (expand-file-name default-directory))
           (orig         git-peek-save-dir)
           (xsrv-gh-root (expand-file-name "~/src/github.com/minorugh/xsrv-GH/"))
           (xsrv-mn-root (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/"))
           (xsrv-p       (or (string-prefix-p xsrv-gh-root dir)
                             (string-prefix-p xsrv-mn-root dir)))
           (new-save-dir (cond
                          ((string-prefix-p xsrv-gh-root dir)
                           (concat (expand-file-name "~/Dropbox/GH/")
                                   (file-relative-name dir xsrv-gh-root)))
                          ((string-prefix-p xsrv-mn-root dir)
                           (concat (expand-file-name "~/Dropbox/minorugh.com/")
                                   (file-relative-name dir xsrv-mn-root)))
                          (t orig))))
      (setq git-peek-save-dir new-save-dir)
      (let ((fn nil))
	(setq fn (lambda ()
                   (setq git-peek-save-dir orig)
                   (when xsrv-p
                     (my-open-xsrv-2pane dir new-save-dir))
                   (remove-hook 'git-peek-finish-hook fn)))
	(add-hook 'git-peek-finish-hook fn))
      (git-peek))))


;;; ============================================================
;;;  Buffer Colorize  (xsrv-GH / xsrv-minorugh 配下のバッファを識別)
;;; ============================================================

(defvar my-xsrv-buffer-color "#233B6C"
  "Background color applied to buffers under xsrv-GH or xsrv-minorugh.")

(defun my-xsrv--maybe-colorize ()
  "Xsrv-GH/xsrv-minorugh 配下のバッファなら `buffer-face-mode' で背景色を適用する."
  (when (and default-directory
             (or (string-prefix-p (expand-file-name "~/src/github.com/minorugh/xsrv-GH/")
                                  (expand-file-name default-directory))
                 (string-prefix-p (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/")
                                  (expand-file-name default-directory))))
    (buffer-face-set `(:background ,my-xsrv-buffer-color))))

(add-hook 'dired-mode-hook         #'my-xsrv--maybe-colorize)
(add-hook 'dired-after-readin-hook #'my-xsrv--maybe-colorize)
(add-hook 'find-file-hook          #'my-xsrv--maybe-colorize)


;;; ============================================================
;;;  Dynamic file protection & rsync lock
;;;
;;;  Dropbox/GH 配下の動的フォルダーを自動 read-only 化。
;;;  read-only 解除時に rsync lock を発行し、
;;;  該当バッファが全て kill されたら自動 unlock する。
;;;  tempbuf.el との連携で unlock し忘れを防ぐ。
;;; ============================================================

(defconst my:xsrv-dynamic-dirs
  (mapcar (lambda (d) (expand-file-name (concat "~/Dropbox/GH/" d)))
          '("apvoice/log/" "apvoice/voice/"
            "danwa/data/" "danwa/html/"
            "dia/divoice/"
            "d_select/voice/" "m_select/voice/" "s_select/voice/" "w_select/voice/"
            "d_kukai/back/" "d_kukai/data/" "d_kukai/html/" "d_kukai/score/"
            "m_kukai/back/" "m_kukai/data/" "m_kukai/html/" "m_kukai/score/"
            "s_kukai/back/" "s_kukai/data/" "s_kukai/html/" "s_kukai/score/"
            "w_kukai/back/" "w_kukai/data/" "w_kukai/html/" "w_kukai/score/"))
  "Rsync lock の対象となる動的フォルダーの絶対パスリスト.")

(defconst my:xsrv-lockfile (expand-file-name "~/xsrv-rsync.lock"))

(defun my:xsrv-dynamic-p (file)
  "FILE が動的フォルダー配下であれば t を返す."
  (when file
    (cl-some (lambda (dir) (string-prefix-p dir file))
             my:xsrv-dynamic-dirs)))

(defun my:xsrv-lock ()
  "Rsync lock ファイルを発行する."
  (unless (file-exists-p my:xsrv-lockfile)
    (write-region "" nil my:xsrv-lockfile)
    (message "[xsrv] rsync lock ON")))

(defun my:xsrv-unlock-if-clean ()
  "動的フォルダー配下の編集中バッファがゼロなら lock を解除する."
  (unless (cl-some (lambda (buf)
                     (with-current-buffer buf
                       (and (buffer-file-name)
                            (my:xsrv-dynamic-p (buffer-file-name))
                            (not buffer-read-only))))
                   (buffer-list))
    (when (file-exists-p my:xsrv-lockfile)
      (delete-file my:xsrv-lockfile)
      (message "[xsrv] rsync lock OFF"))))

(defun my:xsrv-find-file-hook ()
  "動的フォルダー配下のファイルを自動 read-only にする."
  (when (my:xsrv-dynamic-p (buffer-file-name))
    (read-only-mode 1)
    (key-chord-define (or (current-local-map) global-map) "qq" #'my-makefile-toggle-readonly)))

(defun my:xsrv-read-only-hook ()
  "Read-only 解除時に lock を発行、復帰時に unlock チェックする."
  (when (my:xsrv-dynamic-p (buffer-file-name))
    (if buffer-read-only
        (my:xsrv-unlock-if-clean)
      (my:xsrv-lock))))

(defun my:xsrv-kill-buffer-hook ()
  "動的ファイルのバッファ kill 時に unlock チェックする."
  (when (my:xsrv-dynamic-p (buffer-file-name))
    (my:xsrv-unlock-if-clean)))

(add-hook 'find-file-hook       #'my:xsrv-find-file-hook)
(add-hook 'read-only-mode-hook  #'my:xsrv-read-only-hook)
(add-hook 'kill-buffer-hook     #'my:xsrv-kill-buffer-hook)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 60-xsrv-dired.el ends here
