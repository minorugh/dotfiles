;;; 50-xsrv-dired.el --- Xserver remote operations via TRAMP dired. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-xsrv-host "minorugh@sv13268.xserver.jp#10022"
  "TRAMP host string for xserver.")

(defvar my-xsrv-modeline-color "#3a6a8a"
  "Mode-line background color while xsrv-dired is active.")

(defvar my-xsrv--modeline-default nil
  "Saved mode-line background color before xsrv-dired.")

(defconst my-xsrv--home "/home/minorugh/")
(defconst my-xsrv--gh   (concat my-xsrv--home "gospel-haiku.com/public_html/"))
(defconst my-xsrv--mn   (concat my-xsrv--home "minorugh.com/public_html/"))

(defvar my-xsrv-dirs
  `(("gospel-haiku" . ,my-xsrv--gh)
    ("minorugh.com" . ,my-xsrv--mn)
    ("home/user"    . ,my-xsrv--home)
    ("passwd"       . ,(concat my-xsrv--home "gospel-haiku.com/passwd/"))
    ("d_kukai/data" . ,(concat my-xsrv--gh "d_kukai/data/"))
    ("w_kukai/data" . ,(concat my-xsrv--gh "w_kukai/data/"))
    ("s_kukai/data" . ,(concat my-xsrv--gh "s_kukai/data/"))
    ("m_kukai/data" . ,(concat my-xsrv--gh "m_kukai/data/")))
  "Alist of label -> remote directory path for xsrv-dired.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deploy from local dired (50-dired.el から移植)
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
;; Remote dired via TRAMP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-xsrv-dired ()
  "Open remote xserver directory in `dired' via TRAMP."
  (interactive)
  (let* ((choice (completing-read "Remote dir: " (mapcar #'car my-xsrv-dirs)))
         (path   (cdr (assoc choice my-xsrv-dirs)))
         (buf    (dired (format "/ssh:%s:%s" my-xsrv-host path))))
    (with-current-buffer buf
      (setq my-xsrv--modeline-default (face-background 'mode-line))
      (set-face-background 'mode-line my-xsrv-modeline-color)
      (add-hook 'kill-buffer-hook #'my-xsrv--restore nil t)
      (local-set-key (kbd "q") #'my-xsrv--quit))))

(defun my-xsrv--quit ()
  "Quit xsrv `dired' and restore mode-line color."
  (interactive)
  (quit-window t)
  (my-xsrv--restore))

(defun my-xsrv--restore ()
  "Restore mode-line color."
  (when (and my-xsrv--modeline-default
             (not (cl-some (lambda (buf)
                             (with-current-buffer buf
                               (and (eq major-mode 'dired-mode)
                                    (file-remote-p default-directory))))
                           (buffer-list))))
    (set-face-background 'mode-line my-xsrv--modeline-default)
    (setq my-xsrv--modeline-default nil)))

(add-hook 'dired-after-readin-hook
          (lambda ()
            (when (file-remote-p default-directory)
              (setq-local dired-sort-inhibit t)
              (goto-char (point-min))
              (let ((inhibit-read-only t))
                (sort-regexp-fields t "^.*$" "[ ]*." (point-min) (point-max)))
              (when my-xsrv--modeline-default
                (set-face-background 'mode-line my-xsrv-modeline-color)
                (add-hook 'kill-buffer-hook #'my-xsrv--restore nil t)
                (local-set-key (kbd "q") #'my-xsrv--quit)))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-xsrv-dired.el ends here
