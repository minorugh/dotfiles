;;; 50-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
  :hook (dired-mode-hook . my-dired-omit-mode)
  :bind (:dired-mode-map
         ("<left>"  . my-dired-up)
         ("<right>" . my-dired-open)
         ("RET" . my-dired-open)
         ("w"   . wdired-change-to-wdired-mode)
         ("a"   . dired-omit-mode)
         ("s"   . my-dired-sudo-rm)
         ("o"   . my-dired-open-xdg)
         ("["   . dired-hide-details-mode)
         ("t"   . my-open-tig)
	 ("."   . xsrv-deploy-dired)
	 (","   . xsrv-download-dired)
         ("i"   . my-sxiv))
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFl")
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (setq dired-omit-files "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Emacs 30 + ivy: dired-do-copy の read-file-name を ivy に横取りさせない
  (add-to-list 'ivy-completing-read-handlers-alist
               '(dired-do-copy . completing-read-default))

  (defun my-dired-omit-mode ()
    "Disable `dired-omit-mode' only in specific directories."
    (let ((current (file-name-as-directory
                    (expand-file-name default-directory))))
      (dired-omit-mode
       (if (member current
                   (mapcar (lambda (d)
                             (file-name-as-directory
                              (expand-file-name d)))
                           '("~/"
                             "~/.env_source/"
                             "~/src/github.com/minorugh/dotfiles/"
                             "~/src/github.com/minorugh/dotfiles/env/")))
           -1
         1))))

(defun my-dired-open ()
  "Open file or directory at point."
  (interactive)
  (let ((file (dired-get-filename)))
    (cond
     ((file-directory-p file)
      (find-alternate-file file))
     ((file-remote-p file)
      (when (x-popup-dialog
             t
             `(,(format "リモートファイルを開きますか？\n\n  %s"
                        (file-name-nondirectory file))
               ("開く" . t)
               ("やめる" . nil)))
        (find-file file)))
     (t
      (find-file file)))))

  (defun my-dired-up ()
    "Go to parent directory in the same buffer."
    (interactive)
    (find-alternate-file ".."))

  (defun my-dired-open-xdg ()
    "In dired, open the file in associated application."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun my-dired-sudo-rm ()
    "Delete files marked with Dired with sudo."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (when (y-or-n-p "Delete marked files with sudo?")
        (dolist (file files)
          (let ((result (call-process "sudo" nil t nil "rm" "-rf" file)))
            (unless (zerop result)
              (message "sudo rm failed for: %s" file))))
        (revert-buffer))))

  (defun my-open-tig ()
    "Run tig for current context in gnome-terminal."
    (interactive)
    (let* ((path (or (and (derived-mode-p 'dired-mode)
                          (dired-get-filename nil t))
                     (buffer-file-name)
                     default-directory))
           (dir  (if (file-directory-p path)
                     path
                   (file-name-directory path)))
           (root (locate-dominating-file dir ".git")))
      (if root
          (start-process
           "tig" nil
           "gnome-terminal"
           "--maximize"
           "--working-directory" dir
           "--"
           "bash" "-c"
           (format "tig %s" (shell-quote-argument path)))
        (message "Not in a Git repo"))))

  ;; (defun xsrv-deploy-dired ()
  ;;   "Deploy file at point in dired to xserver."
  ;;   (interactive)
  ;;   (let* ((file (dired-get-filename))
  ;;          (name (file-name-nondirectory file)))
  ;;     (cond
  ;;      ((file-directory-p file)
  ;; 	(message "Error: ディレクトリは deploy できません。"))
  ;;      ((string-match-p "\\(^Makefile$\\|^README\\|\\.mk$\\|\\.bak$\\)" name)
  ;;       (message "Error: %s は deploy 対象外です。" name))
  ;;      ((not (or (string-prefix-p "/home/minoru/Dropbox/GH/" file)
  ;; 		 (string-prefix-p "/home/minoru/Dropbox/minorugh.com/" file)))
  ;; 	(message "Error: deploy 対象外のファイルです。"))
  ;;      (t
  ;; 	(when (x-popup-dialog
  ;;              t
  ;;              `(,(format "本当に deploy しますか？\n\n  %s" name)
  ;; 		 ("Deploy する" . t)
  ;; 		 ("やめる" . nil)))
  ;;         (shell-command (format "perl ~/Dropbox/GH/common/deploy.pl %s" file)))))))

  (defun my-sxiv ()
    "Open images in current directory with sxiv (fullscreen)."
    (interactive)
    (let* ((files (directory-files default-directory nil
                                   "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"))
           (cmd (format "sxiv -t -f %s"
                        (mapconcat #'shell-quote-argument files " "))))
      (start-process-shell-command "sxiv" nil cmd))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-dired.el ends here
