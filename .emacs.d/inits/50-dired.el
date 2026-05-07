;;; 50-dired.el --- Dired configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired
  :hook (dired-after-readin-hook . my-dired-hide-dotfiles)
  :bind (:dired-mode-map
         ("<left>"  . my-dired-up)
         ("<right>" . my-dired-open)
         ("RET" . my-dired-open)
         ("w"   . wdired-change-to-wdired-mode)
         ("a"   . my-dired-toggle-dotfiles)
         ("s"   . my-dired-sudo-rm)
         ("o"   . my-dired-open-xdg)
         ("."   . my-open-tig)
         ("i"   . my-sxiv))
  :init
  (defvar my-dired-show-dotfiles-dirs
    (mapcar (lambda (d) (file-name-as-directory (expand-file-name d)))
            '("~/" "~/.env_source/"
              "~/src/github.com/minorugh/dotfiles/"
              "~/src/github.com/minorugh/dotfiles/env/"))
    "Directories where dotfiles should NOT be hidden.")
  (defvar my-dired-hidden-table (make-hash-table :test 'equal)
    "Hash table tracking dotfile visibility per directory. t = hidden.")
  :config
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)
  (setq dired-recursive-copies  'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-listing-switches "-AFl")
  (setq ls-lisp-use-insert-directory-program nil)
  (setq ls-lisp-dirs-first t)
  (put 'dired-find-alternate-file 'disabled nil)

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

  (defun my-sxiv ()
    "Open images in current directory with sxiv (fullscreen)."
    (interactive)
    (let* ((files (directory-files default-directory nil
                                   "\\.\\(jpe?g\\|png\\|gif\\|bmp\\)$"))
           (cmd (format "sxiv -t -f %s"
                        (mapconcat #'shell-quote-argument files " "))))
      (start-process-shell-command "sxiv" nil cmd)))

  (defun my-dired-hide-dotfiles ()
    "Hide dotfiles by removing lines, without using `dired-omit-mode'."
    (unless (eq major-mode 'wdired-mode)
      (let* ((current (file-name-as-directory
                       (expand-file-name default-directory)))
             (hidden (gethash current my-dired-hidden-table
                              (not (member current my-dired-show-dotfiles-dirs)))))
        (puthash current hidden my-dired-hidden-table)
        (when hidden
          (save-excursion
            (goto-char (point-min))
            (let ((inhibit-read-only t))
              (while (not (eobp))
                (if (and (dired-get-filename 'no-dir t)
                         (string-match
                          "^\\.$\\|^\\.[^\\.].*$\\|\\.elc$"
                          (dired-get-filename 'no-dir t)))
                    (delete-region (line-beginning-position)
                                   (progn (forward-line 1) (point)))
                  (forward-line 1)))))))))

  (defun my-dired-toggle-dotfiles ()
    "Toggle dotfile visibility in current `dired' buffer."
    (interactive)
    (let ((current (file-name-as-directory
                    (expand-file-name default-directory))))
      (puthash current
               (not (gethash current my-dired-hidden-table t))
               my-dired-hidden-table)
      (revert-buffer))))

;; Open .cgi files in ~/Dropbox/passwd/ with text-mode instead of perl-mode.
(add-to-list 'auto-mode-alist '("/Dropbox/passwd/.*\\.cgi\\'" . text-mode))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 50-dired.el ends here
