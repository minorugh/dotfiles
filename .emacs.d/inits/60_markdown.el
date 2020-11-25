;;; 60_markdown.el --- Markdown configurations.  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (leaf markdown-toc :ensure t)
  (setq markdown-command "pandoc")
  (setq markdown-italic-underscore t)
  (setq markdown-asymmetric-header t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-content-type "application/xhtml+xml")
  (setq markdown-css-paths '("https://cdn.jsdelivr.net/npm/github-markdown-css/github-markdown.min.css"
							 "https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/github.min.css"))
  (setq markdown-xhtml-header-content "
  <meta name='viewport' content='width=device-width, initial-scale=1, shrink-to-fit=no'>
  <style>
  body {
    box-sizing: border-box;
    max-width: 740px;
    width: 100%;
    margin: 40px auto;
    padding: 0 10px;
  }
  </style>
  <link rel='stylesheet' href='https://cdn.jsdelivr.net/gh/highlightjs/cdn-release/build/styles/default.min.css'>
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
  <script src='https://unpkg.com/mermaid@8.4.8/dist/mermaid.min.js'></script>
  <script>
  mermaid.initialize({
    theme: 'default',  // default, forest, dark, neutral
    startOnLoad: true
  });
  </script>
  ")

  :bind ("C-x m" . hydra-markdown/body)
  :hydra
  (hydra-markdown
   (:color red :hint nil)
   "
    Markdown: _i_talic  消線:_x_  _f_ootnote  _t_able  t_o_c  _e_dit-code:_a_bort  pre_v_iew  md2_p_df  md2_d_ocx"
   ("i" markdown-insert-italic)
   ("x" markdown-insert-strike-through)
   ("t" markdown-insert-table)
   ("o" markdown-toc-generate-or-refresh-toc)
   ("f" markdown-insert-footnote)
   ("e" markdown-edit-code-block)
   ("a" edit-indirect-abort)
   ("v" markdown-preview)
   ;; Pndoc
   ("p" md2pdf)
   ("d" md2docx)
   ("<muhenkan>" nil))

  :init
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

;;; 60_markdown.el ends here
