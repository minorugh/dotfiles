;;; 50-xsrv-dired.el --- Xserver deploy/backup operations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup: xserver → xsrv-GH / xsrv-minorugh
;; key bindings in 40-hydra-dired.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-xsrv-backup (src-dir peer-dir)
  "Synchronize from xserver and open two-pane `dired'.
SRC-DIR: backup先, PEER-DIR: 2ペインで並べる相手."
  (interactive)
  (letrec ((finish-fn
            (lambda (_buf _msg)
              (remove-hook 'compilation-finish-functions finish-fn)
              (let ((xsrv-buf (dired src-dir)))
                (with-current-buffer xsrv-buf
                  (local-set-key (kbd "q") #'quit-window))
                (when (y-or-n-p "2ペインで開きますか?")
                  (split-window-right)
                  (other-window 1)
                  (dired peer-dir)
                  (other-window 1))))))
    (add-hook 'compilation-finish-functions finish-fn)
    (compile "~/.emacs.d/elisp/bin/xsrv-backup-smart.sh")))

(defun my-xsrv-backup-dwim ()
  "Diredのカレントディレクトリに応じてrsync backupを実行する."
  (interactive)
  (let ((dir (expand-file-name default-directory)))
    (cond
     ((string-prefix-p (expand-file-name "~/src/github.com/minorugh/xsrv-GH/") dir)
      (my-xsrv-backup "~/src/github.com/minorugh/xsrv-GH/"
                      "~/Dropbox/GH/"))
     ((string-prefix-p (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/") dir)
      (my-xsrv-backup "~/src/github.com/minorugh/xsrv-minorugh/"
                      "~/Dropbox/minorugh/"))
     (t
      (message "このディレクトリはbackup対象外です")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deploy from local dired
;; key bindings in 50-dired.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xsrv-deploy-dired ()
  "Deploy file at point in `dired' to xserver."
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
               ("やめる" . nil)))
        (shell-command (format "perl ~/Dropbox/GH/common/deploy.pl %s" file)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download: xsrv-GH → local GH
;; key bindings in 50-dired.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xsrv-download-dired ()
  "Download file at point in `dired' from xsrv-GH to local GH."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (file-name-nondirectory file))
         (xsrv-root "/home/minoru/src/github.com/minorugh/xsrv-GH/")
         (local-root "/home/minoru/Dropbox/GH/")
         (rel (file-relative-name file xsrv-root))
         (dest (concat local-root rel)))
    (when (x-popup-dialog
           t
           `(,(format "ローカルにダウンロードしますか？\n\n  %s" name)
             ("Download する" . t)
             ("やめる" . nil)))
      (if (and (file-exists-p dest)
               (not (y-or-n-p (format "%s は既にあります。上書きしますか?" name))))
          (message "キャンセルしました。")
        (copy-file file dest t)
        (message "Downloaded: %s" rel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-peek
;; key bindings in 40-hydra-dired.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `git-peek-save-dir' は git-peek パッケージ側で定義される変数。
;; `my-xsrv-git-peek' 内で let バインドするためコンパイラへ事前宣言。
(defvar git-peek-save-dir)

(defun my-xsrv-git-peek ()
  "Set the `git-peek' export destination to the corresponding path in Dropbox/GH."
  (interactive)
  (let* ((xsrv-root "/home/minoru/src/github.com/minorugh/xsrv-GH/")
         (local-root "/home/minoru/Dropbox/GH/"))
    (unless (string-prefix-p xsrv-root (expand-file-name default-directory))
      (user-error "Xsrv-GH の Dired から呼んでください"))
    (let ((git-peek-save-dir
           (concat local-root (file-relative-name default-directory xsrv-root))))
      (git-peek))))

(defun my-git-peek-smart ()
  "`xsrv-GH' からなら `my-xsrv-git-peek' それ以外は `git-peek'."
  (interactive)
  (if (string-prefix-p "/home/minoru/src/github.com/minorugh/xsrv-GH/"
		       (expand-file-name default-directory))
      (my-xsrv-git-peek)
    (git-peek)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer colorize for xsrv-GH dired & files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-xsrv-buffer-color "#051122"
  "Background color of buffers under Xsrv-GH/xsrv-minorugh.")

(defun my-xsrv--maybe-colorize ()
  "`xsrv-GH/xsrv-minorugh' 配下のバッファ（diredまたはファイル）なら `buffer-face-mode' で色付け."
  (when (and default-directory
             (or (string-prefix-p (expand-file-name "~/src/github.com/minorugh/xsrv-GH/")
                                  (expand-file-name default-directory))
                 (string-prefix-p (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/")
                                  (expand-file-name default-directory))))
    (buffer-face-set `(:background ,my-xsrv-buffer-color))))

;; Dired (folder list) colored when loaded/updated
(add-hook 'dired-mode-hook #'my-xsrv--maybe-colorize)
(add-hook 'dired-after-readin-hook #'my-xsrv--maybe-colorize)

;; Automatic coloring when opening "files" under folders
(add-hook 'find-file-hook #'my-xsrv--maybe-colorize)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-xsrv-dired.el ends here
