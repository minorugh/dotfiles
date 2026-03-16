;;; 60-markdown.el --- Markdown mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :hook (kill-buffer-hook . my:delete-tmp-markdown-html)
  :bind ("C-c RET" . markdown-follow-link-at-point)
  :config
  (setq markdown-command "pandoc -f markdown -t html5"
	markdown-command-needs-filename t
	browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "google-chrome"
	markdown-content-type "application/xhtml+xml"
	markdown-fontify-code-blocks-natively t
	markdown-css-paths
	(list (expand-file-name "~/.emacs.d/elisp/css/markdown-cream.css"))
	markdown-xhtml-header-content
	(concat
	 "<link rel='stylesheet' href='"
	 (expand-file-name "~/.emacs.d/elisp/css/highlight.min.css")
	 "'>\n"
	 "<meta name='viewport' content='width=device-width, initial-scale=1'>\n"
	 "<script src='"
	 (expand-file-name "~/.emacs.d/elisp/css/highlight.min.js")
	 "'></script>\n"
	 "<script>\n"
	 "document.addEventListener('DOMContentLoaded', () => {\n"
	 "  document.body.classList.add('markdown-body');\n"
	 "  document.querySelectorAll('pre code').forEach((code) => {\n"
	 "    hljs.highlightElement(code);\n"
	 "  });\n"
	 "});\n"
	 "</script>\n"))
  (defun my:delete-tmp-markdown-html ()
    "Delete /tmp/burl*.html when killed markdown buffer."
    (when (and (derived-mode-p 'markdown-mode)
               (buffer-file-name))
      (let ((temp-files (file-expand-wildcards "/tmp/burl*.html")))
	(dolist (file temp-files)
          (when (file-exists-p file)
            (delete-file file)
            (message "Deleted temporary file: %s" file))))))
  :custom-face
  (markdown-code-face ((t (:inherit nil :background "gray10"))))
  (markdown-pre-face  ((t (:inherit font-lock-constant-face)))))


(defun md2pdf ()
  "Generate PDF from currently open markdown via pandoc + lualatex."
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
	 (pdffile  (concat (file-name-sans-extension filename) ".pdf")))
    (call-process-shell-command
     (concat "pandoc "
	     filename
	     " -o "
	     pdffile
	     " -V mainfont=IPAPGothic -V geometry:margin=20mm -V fontsize=14pt --pdf-engine=lualatex"))
    (start-process-shell-command "xdg-open-pdf" nil (concat "xdg-open " pdffile))))

(defun md2docx ()
  "Generate docx from currently open markdown."
  (interactive)
  (let* ((filename (buffer-file-name (current-buffer)))
	 (docxfile  (concat (file-name-sans-extension filename) ".docx")))
    (call-process-shell-command
     (concat "pandoc " filename
	     " -t docx -o "
	     docxfile
	     " -V mainfont=IPAPGothic -V fontsize=16pt --highlight-style=zenburn"))
    (start-process-shell-command "xdg-open-docx" nil (concat "xdg-open " docxfile))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 60-markdown.el ends here
