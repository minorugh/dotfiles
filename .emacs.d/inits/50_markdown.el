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
	(markdown-css-paths . '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/5.1.0/github-markdown.min.css"))
	(markdown-xhtml-header-content . "
  <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
  <link rel='icon' href=\"https://doc.gospel-haiku.com/favicon.png\">
  <link rel='apple-touch-icon' href=\"https://doc.gospel-haiku.com/favicon.png\" />
  <!-- Bootstrap Core CSS -->
  <link rel='stylesheet' href=\"https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css\">
  <!--<link rel='stylesheet' href=\"https://gospel-haiku.com/common/css/main.css\">-->
  <style>
  body {
    box-sizing: border-box;
    max-width: 960px;
    width: 100%;
    margin: 40px auto;
    padding: 0 10px;
  }
  /*pre {
    background-color:#f5f5f5;
    font-size:80%;
  }*/
  h1 {
    border: 0;
  }
  </style>
  <script src='http://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/highlight.min.js'></script>
  <script>
  document.addEventListener('DOMContentLoaded', () => {
    document.body.classList.add('markdown-body');
    document.querySelectorAll('pre[lang] > code').forEach((code) => {
      code.classList.add(code.parentElement.lang);
      hljs.highlightBlock(code);
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
