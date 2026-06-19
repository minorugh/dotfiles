;;; 30-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Dired Core
;;; ============================================================

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
	 ("]"   . my-dired-gitk)
         ("."   . xsrv-deploy-dired)    ; see 50-xsrv-dired.el
         (","   . xsrv-download-dired)  ; see 50-xsrv-dired.el
         ("b"   . my-xsrv-backup-dwim)  ; see 50-xsrv-dired.el
         ("i"   . my-sxiv))
  :config
  (setq dired-dwim-target         t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies   'always)
  (setq dired-recursive-deletes  'always)
  (setq dired-listing-switches   "-AlhF --group-directories-first --no-group")
  (setq dired-omit-files         "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Emacs 30 + ivy: dired-do-copy の read-file-name を ivy に横取りさせない
  (add-to-list 'ivy-completing-read-handlers-alist
               '(dired-do-copy . completing-read-default))


;;; ============================================================
;;;  Omit Mode  (特定ディレクトリでは omit を無効化)
;;; ============================================================

  (defun my-dired-omit-mode ()
    "Disable `dired-omit-mode' in specific directories; enable elsewhere."
    (let ((current (file-name-as-directory
                    (expand-file-name default-directory))))
      (dired-omit-mode
       (if (member current
                   (mapcar (lambda (d)
                             (file-name-as-directory (expand-file-name d)))
                           '("~/"
                             "~/.env_source/"
                             "~/src/github.com/minorugh/dotfiles/"
                             "~/src/github.com/minorugh/dotfiles/env/")))
           -1
         1))))


;;; ============================================================
;;;  Navigation
;;; ============================================================

  (defun my-dired-open ()
    "Open file or directory at point.  Confirm before opening remote files."
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
                 ("開く"   . t)
                 ("やめる" . nil)))
          (find-file file)))
       (t
        (find-file file)))))

  (defun my-dired-up ()
    "Go to parent directory in the same buffer."
    (interactive)
    (find-alternate-file ".."))

  (defun my-dired-open-xdg ()
    "Open file at point with its associated application via xdg-open."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))


;;; ============================================================
;;;  File Operations
;;; ============================================================

  (defun my-dired-sudo-rm ()
    "Delete dired-marked files with sudo."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (when (y-or-n-p "Delete marked files with sudo?")
        (dolist (file files)
          (let ((result (call-process "sudo" nil t nil "rm" "-rf" file)))
            (unless (zerop result)
              (message "sudo rm failed for: %s" file))))
        (revert-buffer))))


;;; ============================================================
;;;  External Tools
;;; ============================================================

  (defun my-dired-gitk ()
    "Run gitk for the current Git repository."
    (interactive)
    (let* ((file (dired-get-file-for-visit))
           (root (locate-dominating-file file ".git")))
      (if root
          (start-process "gitk" nil "gitk")
	(message "Not in a Git repository"))))

  (defun my-sxiv ()
    "Open all images in the current directory with sxiv (fullscreen tiling)."
    (interactive)
    (let* ((files (directory-files default-directory nil
                                   "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"))
           (cmd (format "sxiv -t -f %s"
                        (mapconcat #'shell-quote-argument files " "))))
      (start-process-shell-command "sxiv" nil cmd))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 30-dired.el ends here
