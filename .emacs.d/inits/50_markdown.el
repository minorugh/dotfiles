;;; 50_markdown.el --- Markdown configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'")
  :chord (:markdown-mode-map
		  (".." . hydra-markdown/body))
  :custom
  `((markdown-command . "pandoc")
	(markdown-command-needs-filename . t)
	(markdown-fontify-code-blocks-natively . t)
	(markdown-content-type . "application/xhtml+xml")
	(markdown-css-paths . '("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css"))
	(markdown-xhtml-header-content . "
  <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
  <style>
  body {
    box-sizing: border-box;
    max-width: 740px;
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
  "))
  :hydra
  (hydra-markdown
   (:color red :hint nil)
   "
    Markdown: _i_talic  消線:_x_  ft_n_ote  _t_able  _m_arkup  _v_iew._e_xp._p_df._d_ocx"
   ("i" markdown-insert-italic)
   ("x" markdown-insert-strike-through)
   ("t" markdown-insert-table)
   ("n" markdown-insert-footnote)
   ("m" markdown-toggle-markup-hiding)
   ("v" markdown-preview)
   ("e" markdown-export)
   ;; Pndoc
   ("p" md2pdf)
   ("d" md2docx)
   ("<muhenkan>" nil))
  :config
  (defun md2pdf ()
	"Generate pdf from currently open markdown."
	(interactive)
	(let ((filename (buffer-file-name (current-buffer))))
	  ;; Use wkhtmltopdf without latex
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
			   ".docx")))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_markdown.el ends here
