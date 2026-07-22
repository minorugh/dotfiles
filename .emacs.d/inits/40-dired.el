;;; 40-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Dired Core
;; ============================================================

(leaf dired
  :preface
  (autoload 'my-open-tig "my-tig-bridge" nil t)
  :hook (dired-mode-hook . my-dired-omit-mode)
  :bind (:dired-mode-map
         ("<left>"   . my-dired-up)
         ("<right>"  . my-dired-open)
         ("RET" . my-dired-open)
         ("w"   . wdired-change-to-wdired-mode)
         ("a"   . dired-omit-mode)
         ("s"   . my-dired-sudo-rm)
         ("o"   . my-dired-open-xdg)
         ("v"   . my-dired-open-vim)
	 ("n"   . my-dired-open-nano)
         ("["   . dired-hide-details-mode)
         ("t"   . my-open-tig)
         ("]"   . my-dired-gitk)
         ("p"   . my-dired-permission-help)
         ("."   . xsrv-deploy-dired)    ; see 40-remote.el
         (","   . xsrv-download-dired)  ; see 40-remote.el
         ("i"   . my-sxiv))
  :config
  (setq dired-dwim-target         t)
  (setq dired-recursive-copies   'always)
  (setq dired-recursive-deletes  'always)
  (setq dired-listing-switches   "-AlhF --group-directories-first --no-group")
  (setq dired-omit-files         "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$")
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Don't let Ivy override Emacs 30 file-name completion in dired copy.
  (add-to-list 'ivy-completing-read-handlers-alist
               '(dired-do-copy . completing-read-default)))


;; ============================================================
;;  Dired Extensions  (omit / navigation / file-ops / external / help)
;; ============================================================

(leaf *my-dired-extensions
  :after dired
  :config
  ;;  Omit Mode  (特定ディレクトリでは omit を無効化)
  ;; ----------------------------------------------------------
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
                             "~/src/github.com/minorugh/GH/"
                             "~/src/github.com/minorugh/minorugh.com/"
                             "~/src/github.com/minorugh/dotfiles/"
                             "~/src/github.com/minorugh/dotfiles/env/")))
           -1
         1))))

  ;;  Navigation
  ;; ----------------------------------------------------------
  (defun my-dired-open ()
    "Open file or directory at point."
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (find-alternate-file file)
        (find-file file))))

  (defun my-dired-up ()
    "Go to parent directory in the same buffer."
    (interactive)
    (find-alternate-file ".."))

  (defun my-dired-open-xdg ()
    "Open file at point with its associated application via xdg-open."
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun my-dired-open-vim ()
    "Open the file at point in gnome-terminal with Vim."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (start-process
       "gnome-terminal" nil
       "gnome-terminal"
       "--maximize"
       "--"
       "vim"
       (dired-get-file-for-visit))))

  (defun my-dired-open-nano ()
    "Open the file at point in a maximized gnome-terminal with GNU nano."
    (interactive)
    (let ((default-directory (dired-current-directory)))
      (start-process
       "gnome-terminal" nil
       "gnome-terminal"
       "--maximize"
       "--"
       "nano"
       (dired-get-file-for-visit))))

  ;;  File Operations
  ;; ----------------------------------------------------------
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

  ;;  External Tools
  ;; ----------------------------------------------------------
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
      (start-process-shell-command "sxiv" nil cmd)))

  ;; *Permission Help* を右サイドバーに固定表示
  ;; ----------------------------------------------------------
  (add-to-list 'display-buffer-alist
               '("\\*Permission Help\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 40)
                 (window-parameters . ((no-delete-other-windows . t)
                                       (mode-line-format . none)))))

  (defun my-dired-permission-help ()
    "Show a quick permission reference."
    (interactive)
    (let ((help-window-select t))   ;; 表示後にヘルプウィンドウへフォーカスを移す
      (with-help-window "*Permission Help*"
        (princ "Permission Quick Reference\n")
        (princ "==========================\n\n")

        (princ "Permission values\n")
        (princ "-----------------\n")
        (princ "r = 4\n")
        (princ "w = 2\n")
        (princ "x = 1\n\n")

        (princ "Common combinations\n")
        (princ "-------------------\n")
        (princ "rwx = 7\n")
        (princ "rw- = 6\n")
        (princ "r-x = 5\n")
        (princ "r-- = 4\n")
        (princ "--- = 0\n\n")

        (princ "Common permissions\n")
        (princ "------------------\n")
        (princ "-rw-r--r--  = 644\n")
        (princ "-rw-------  = 600\n")
        (princ "-rw----r--  = 604\n")
        (princ "-rw----rw-  = 606\n")
        (princ "-rwxr-xr-x  = 755\n")
        (princ "-rwx------  = 700\n")
        (princ "-rwx---r-x  = 705\n\n")

        (princ "File type (first character)\n")
        (princ "---------------------------\n")
        (princ "-  regular file\n")
        (princ "d  directory\n")
        (princ "l  symbolic link\n")
        (princ "c  character device\n")
        (princ "b  block device\n")
        (princ "p  FIFO (named pipe)\n")
        (princ "s  socket\n\n")

        (princ "Examples\n")
        (princ "--------\n")
        (princ "drwxr-xr-x\n")
        (princ " d   rwx   r-x   r-x\n")
        (princ " |    7     5     5\n")
        (princ " +--> directory\n")
        (princ " => directory 755\n")))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 40-dired.el ends here
