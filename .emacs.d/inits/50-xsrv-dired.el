;;; 50-xsrv-dired.el --- Xserver deploy/backup operations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup: xserver → xsrv-GH / xsrv-minorugh
;; key bindings in 40-hydra-dired.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-xsrv-backup (src-dir pair-dir)
  "SRC-DIR の `dired' で xsev-backup実行後に PAIR-DIRを復元する."
  (interactive)
  (letrec ((finish-fn
            (lambda (_buf _msg)
              (remove-hook 'compilation-finish-functions finish-fn)
              (my-open-xsrv-2pane src-dir pair-dir))))
    (add-hook 'compilation-finish-functions finish-fn)
    (compile "~/.emacs.d/elisp/bin/xsrv-backup-smart.sh")))

(defun my-xsrv-backup-dwim ()
  "Dired のカレントディレクトリに応じて rsync backup を実行する."
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
  "Download file at point in `dired' from xsrv-GH/xsrv-minorugh to local."
  (interactive)
  (let* ((file (dired-get-filename))
         (name (file-name-nondirectory file))
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
             ("やめる" . nil)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; git-peek
;; key bindings in 40-hydra-dired.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-git-peek-smart ()
  "Run `git-peek' with appropriate save-dir.
Show 2pane when called from xsrv `dired'."
  (interactive)
  (let* ((dir (expand-file-name default-directory))
         (orig (and (boundp 'git-peek-save-dir) git-peek-save-dir))
         (xsrv-gh-root (expand-file-name "~/src/github.com/minorugh/xsrv-GH/"))
         (xsrv-mn-root (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/"))
         (new-save-dir
          (cond
           ((string-prefix-p xsrv-gh-root dir)
            (concat (expand-file-name "~/Dropbox/GH/")
                    (file-relative-name dir xsrv-gh-root)))
           ((string-prefix-p xsrv-mn-root dir)
            (concat (expand-file-name "~/Dropbox/minorugh.com/")
                    (file-relative-name dir xsrv-mn-root)))
	   (t (expand-file-name "~/Dropbox/backup/tmp/")))))
    (setq git-peek-save-dir new-save-dir)
    (let ((fn nil))
      (setq fn (lambda ()
		 (when orig (setq git-peek-save-dir orig))
		 (if (or (string-prefix-p xsrv-gh-root dir)
			 (string-prefix-p xsrv-mn-root dir))
		     (my-open-xsrv-2pane dir new-save-dir)
		   (my-open-xsrv-2pane dir (expand-file-name "~/Dropbox/backup/tmp/")))
		 (remove-hook 'git-peek-finish-hook fn)))
      (add-hook 'git-peek-finish-hook fn))
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
