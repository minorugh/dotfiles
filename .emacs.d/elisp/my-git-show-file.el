;;; my-git-show-file.el --- Extract a file from a past git commit -*- lexical-binding: t -*-
;;; Commentary:

;; Extract a specific file from a past git commit and save to ~/Dropbox/backup/tmp/
;; Requires: ivy

;;; Code:
(declare-function ivy-read "ivy")

;;;###autoload
(defun my-git-show-file ()
  "Extract files from past commits and save in '~/Dropbox/backup/tmp/'."
  (interactive)
  (let* ((root (or (locate-dominating-file default-directory ".git")
                   (error "Git repository not found")))
         (files
          (split-string
           (shell-command-to-string
            (format "git -C %s ls-files" root)) "\n" t))
         (file (ivy-read "Select File: " files))
         (commits
          (split-string
           (shell-command-to-string
            (format "git -C %s log --oneline -- %s" root file)) "\n" t))
         (commit (ivy-read "Select Commit: " commits))
         (hash (car (split-string commit " ")))
         (date (string-trim
                (shell-command-to-string
                 (concat "git -C " root
                         " show -s --format=%cd --date=format:%Y%m%d "
                         hash))))
         (dest-dir (expand-file-name "~/Dropbox/backup/tmp/"))
         (dest (concat dest-dir date "_" (file-name-nondirectory file))))
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t))
    (shell-command
     (format "git -C %s show %s:%s > %s" root hash file dest))
    (dired dest-dir)
    (message "Saved in: %s" dest)))

(provide 'my-git-show-file)
;;; my-git-show-file.el ends here
