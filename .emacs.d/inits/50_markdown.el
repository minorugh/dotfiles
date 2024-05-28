;;; 50_markdown.el --- Markdown configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf markdown-mode :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (setq markdown-command-needs-filename t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-indent-on-enter 'indent-and-new-item)
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"))
  (setq markdown-xhtml-header-content "
	<meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
	<style>
	body {
	  box-sizing: border-box;
	  max-width: 980px;
	  width: 100%;
	  margin: 40px auto;
	  padding: 0 10px;
	  font-size: large;
	}
	</style>
	<script src='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
	<script>
	document.addEventListener('DOMContentLoaded', () => {
	  document.body.classList.add('markdown-body');
	  document.querySelectorAll('pre code').forEach((code) => {
	    if (code.className != 'mermaid') {
	      hljs.highlightBlock(code);
	    }
	  });
	});
	</script>
	")
  :custom-face `((markdown-code-face . '((t (:inherit nil :background "gray10"))))
		 (markdown-pre-face  . '((t (:inherit font-lock-constant-face))))))

(defun md2pdf ()
  "Use wkhtmltopdf without LaTex."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc "
	     filename
	     " -f markdown -t html5 -o "
	     (file-name-sans-extension filename)
	     ".pdf"))
    (shell-command-to-string
     (concat "evince "
	     (file-name-sans-extension filename)
	     ".pdf"))))

(defun md2docx ()
  "Generate docx from currently open markdown."
  (interactive)
  (let ((filename (buffer-file-name (current-buffer))))
    (shell-command-to-string
     (concat "pandoc "
	     filename
	     " -t docx -o "
	     (file-name-sans-extension filename)
	     ".docx -V mainfont=IPAPGothic -V fontsize=16pt --highlight-style=zenburn"))
    (shell-command-to-string
     (concat "xdg-open "
	     (file-name-sans-extension filename)
	     ".docx"))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_markdown.el ends here
