;;; 40_markdown.el --- Markdown configurations. -*- lexical-binding: t -*-
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
  `((markdown-italic-underscore . t)
    (markdown-asymmetric-header . t)
	(markdown-fontify-code-blocks-natively . t))
  :hydra
  (hydra-markdown
   (:color red :hint nil)
   "
    Markdown: _i_talic  消線:_x_  foot_n_ote  _t_able  _m_arkup  pre_v_iew  md2:_p_df:_d_ocx"
   ("i" markdown-insert-italic)
   ("x" markdown-insert-strike-through)
   ("t" markdown-insert-table)
   ("n" markdown-insert-footnote)
   ("m" markdown-toggle-markup-hiding)
   ("v" livedown-preview)
   ;; Pndoc
   ("p" md2pdf)
   ("d" md2docx)
   ("<muhenkan>" nil))
  :init
  (leaf emacs-livedown :el-get shime/emacs-livedown)
  (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn 'cdr))
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
;;; 40_markdown.el ends here
