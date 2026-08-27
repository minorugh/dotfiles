;;; markdown-utils.el --- Utility functions for Markdown/howm editing -*- lexical-binding: t -*-
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
     (format "perl ~/.emacs.d/bin/gen_toc.pl %s; read"
             (shell-quote-argument (buffer-file-name))))))


;;;###autoload
(defun md2pdf ()
  "Generate PDF from the current markdown buffer via pandoc + lualatex."
  (interactive)
  (let* ((filename (buffer-file-name))
         (pdffile  (concat (file-name-sans-extension filename) ".pdf")))
    (if (zerop (call-process-shell-command
                (concat "pandoc " filename
                        " -o " pdffile
                        " -V mainfont=IPAPGothic -V geometry:margin=20mm"
                        " -V fontsize=14pt --pdf-engine=lualatex")))
        (call-process "xdg-open" nil nil nil pdffile)
      (message "md2pdf: pandoc failed"))))


;;;###autoload
(defun md2docx ()
  "Generate DOCX from the current markdown buffer via pandoc."
  (interactive)
  (let* ((filename (buffer-file-name))
         (docxfile  (concat (file-name-sans-extension filename) ".docx")))
    (if (zerop (call-process-shell-command
                (concat "pandoc " filename
                        " -t docx -o " docxfile
                        " -V mainfont=IPAPGothic -V fontsize=16pt"
                        " --highlight-style=zenburn")))
        (call-process "xdg-open" nil nil nil docxfile)
      (message "md2docx: pandoc failed"))))


(provide 'markdown-utils)
;;; markdown-utils.el ends here
