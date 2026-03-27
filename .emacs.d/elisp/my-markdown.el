;;; my-markdown.el --- Utility functions for Markdown/howm editing -*- lexical-binding: t -*-
;;; Commentary:

;; Miscellaneous helpers for Markdown and howm files.

;;; Code:

;;;###autoload
(defun my-howm-fix-code-comments ()
  "In code blocks, replace '# ' with '## '.
If region is active, process region only (anywhere in buffer).
Otherwise process whole file via Perl."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (copy-marker (region-end))))
        (save-excursion
          (goto-char beg)
          (while (< (point) end)
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
	      (when (string-match "^# " line)
                (delete-region (line-beginning-position) (line-end-position))
                (insert (replace-regexp-in-string "^# " "## " line))))
            (forward-line 1)))
        (set-marker end nil)
        (message "howm-fix-code-comments: region done"))
    (when (and buffer-file-name
	       (string-match (expand-file-name "~/Dropbox/howm/.*\\.md$")
                             buffer-file-name))
      (call-process "perl" nil nil nil
                    (expand-file-name "~/.emacs.d/elisp/howm-fix-code-comments.pl")
                    buffer-file-name)
      (revert-buffer t t t)
      (message "howm-fix-code-comments: done"))))


;;;###autoload
(defun gen-toc-term ()
  "Run gen_toc.pl for current Markdown file in gnome-terminal."
  (interactive)
  (when (string-match-p "\\.md\\'" (buffer-file-name))
    (save-buffer)
    (start-process
     "gentoc" nil "gnome-terminal" "--" "bash" "-c"
     (format "perl ~/.emacs.d/elisp/gen_toc.pl %s; read"
             (shell-quote-argument (buffer-file-name))))))

(provide 'my-markdown)
;;; my-markdown.el ends here
