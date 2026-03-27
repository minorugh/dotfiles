;;; my-github.el ---  Homebrew functions related to GitHub -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; GitHub deploy
;;;###autoload
(defun github-deploy ()
  "Insert current changelog buffer into CHANGELOG.md and open it."
  (interactive)
  (let* ((src (buffer-file-name))
         (basename (file-name-nondirectory src))
         (date (and (string-match "changelog-\\([0-9]\\{8\\}\\)\\.md" basename)
                    (match-string 1 basename)))
         (changelog (expand-file-name "~/src/github.com/minorugh/minorugh.github.io/CHANGELOG.md")))
    (if (not date)
        (message "Not a changelog file: %s" basename)
      (let* ((src-content (with-temp-buffer
                            (insert-file-contents src)
                            (buffer-string)))
             (changelog-content (with-temp-buffer
                                  (insert-file-contents changelog)
                                  (buffer-string)))
             (date-header (concat "## "
                                  (substring date 0 4) "-"
                                  (substring date 4 6) "-"
                                  (substring date 6 8) "\n\n")))
        (with-temp-file changelog
          (insert date-header)
          (insert src-content)
          (insert "\n---\n\n")
          (insert changelog-content))
        (find-file changelog)
        (message "CHANGELOG.md updated: %s" date)))))

(provide 'my-github)
;;; my-github.el ends here
