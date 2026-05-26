;;; 50-xsrv-dired.el --- Xserver deploy/backup operations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-xsrv-modeline-color "#3a6a8a"
  "Mode-line background color while xsrv-GH `dired' is active.")

(defvar my-xsrv--modeline-default nil
  "Saved mode-line background color before xsrv backup.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode-line color restore
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-xsrv--restore ()
  "Restore mode-line color."
  (when my-xsrv--modeline-default
    (set-face-background 'mode-line my-xsrv--modeline-default)
    (setq my-xsrv--modeline-default nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deploy from local dired
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
;; Backup: xserver → xsrv-GH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-xsrv-backup ()
  "Synchronize the latest data from `xserver' and check with dried."
  (interactive)
  (letrec ((finish-fn
            (lambda (_buf _msg)
              (remove-hook 'compilation-finish-functions finish-fn)
              (let ((xsrv-buf (dired "~/src/github.com/minorugh/xsrv-GH/")))
                (with-current-buffer xsrv-buf
                  (setq my-xsrv--modeline-default (face-background 'mode-line))
                  (set-face-background 'mode-line my-xsrv-modeline-color)
                  (add-hook 'kill-buffer-hook #'my-xsrv--restore nil t)
                  (local-set-key (kbd "q") (lambda ()
                                             (interactive)
                                             (quit-window t)
                                             (my-xsrv--restore))))
                (when (y-or-n-p "2ペインで開きますか?")
                  (split-window-right)
                  (other-window 1)
                  (dired "~/Dropbox/GH/")
                  (other-window 1))))))
    (add-hook 'compilation-finish-functions finish-fn)
    (compile "~/.emacs.d/elisp/bin/xsrv-backup-smart.sh")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Download: xsrv-GH → local GH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-xsrv-download-file ()
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


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-xsrv-dired.el ends here
