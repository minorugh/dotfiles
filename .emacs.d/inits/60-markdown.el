;;; 60-markdown.el --- Markdown mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :bind ("C-c RET"  . markdown-follow-link-at-point)
  :config
  (setq markdown-command "pandoc -f markdown -t html5"
	markdown-command-needs-filename t
	markdown-fontify-code-blocks-natively t
	markdown-header-scaling t
	markdown-indent-on-enter 'indent-and-new-item
	markdown-preview-use-browser t
	browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "google-chrome"
	markdown-content-type "application/xhtml+xml"
	markdown-css-paths
	(list (expand-file-name "~/.emacs.d/elisp/markdown.css"))
	markdown-xhtml-header-content
	(concat
	 "<link rel='stylesheet' href='"
	 (expand-file-name "~/.emacs.d/elisp/highlight.min.css")
	 "'>\n"
	 "<meta name='viewport' content='width=device-width, initial-scale=1'>\n"
	 "<script src='"
	 (expand-file-name "~/.emacs.d/elisp/highlight.min.js")
	 "'></script>\n"
	 "<script>\n"
	 "document.addEventListener('DOMContentLoaded', () => {\n"
	 "  document.body.classList.add('markdown-body');\n"
	 "  document.querySelectorAll('pre code').forEach((code) => {\n"
	 "    hljs.highlightElement(code);\n"
	 "  });\n"
	 "});\n"
	 "</script>\n"))
  :custom-face
  `((markdown-code-face . '((t (:inherit nil :background "gray10"))))
    (markdown-pre-face  . '((t (:inherit font-lock-constant-face))))))


(defun md2pdf ()
  "Use pandoc to generate PDF."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc " filename
	     " -f markdown -t html5 -o "
	     (file-name-sans-extension filename) ".pdf"))
    (shell-command-to-string
     (concat "evince " (file-name-sans-extension filename) ".pdf"))))

(defun md2docx ()
  "Generate docx from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc " filename
	     " -t docx -o "
	     (file-name-sans-extension filename)
	     ".docx -V mainfont=IPAPGothic -V fontsize=16pt --highlight-style=zenburn"))
    (shell-command-to-string
     (concat "xdg-open " (file-name-sans-extension filename) ".docx"))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 60-markdown.el ends here
