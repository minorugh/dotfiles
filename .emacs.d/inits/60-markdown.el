;;; 60-markdown.el --- Markdown mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Related custom functions (~/.emacs.d/elisp/my_markdown.el):
;;   `my-howm-fix-code-comments' - Convert comment symbols in code blocks
;;   `gen-toc-term'              - Generate TOC via gnome-terminal
;;
;; These are autoload-registered via my-loaddefs.el.
;;; Code:

(leaf markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'"       . markdown-mode))
  :bind (("C-c RET" . markdown-follow-link-at-point)
	 ("C-c #"   . my-howm-fix-code-comments)
	 ("C-c C-c" . markdown-do-command)
	 ("M-RET"   . markdown-insert-list-item))
  :init
  :config
  (setq markdown-command "pandoc -f markdown+header_attributes-raw_html -t html5"
	markdown-fontify-code-blocks-natively t ;; コードブロックにハイライト
	markdown-header-scaling t               ;; 見出しのサイズを段階的に
	markdown-hide-markup nil                ;; マークアップを表示(tで隠す)
	markdown-command-needs-filename t
	markdown-preview-use-browser t
	browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "google-chrome"
	markdown-content-type "application/xhtml+xml"
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
	 "</script>\n"
	 "<style>
          /* 特定のdiv配下にある1階層目のul/liのドットを消す */
          .toc > ul {
            list-style: none; /* マーカーなし */
            padding-left: 0;  /* 必要に応じてインデントを調整 */
          }
          /* 1階層目の直下liにマーカーを付けない */
          .toc > ul > li {
            list-style-type: none;
          }
          /* 2階層目のul/liにマーカーを表示 */
          .toc > ul > li > ul {
            list-style-type: disc;
            padding-left: 25px;       /* 2階層目のインデント */
          }
          /* （オプション）2階層目の文字色やスタイルを調整 */
          .toc > ul > li > ul > li {
            color: #555;
         }
     </style>"
	 ))
  (custom-set-faces
   '(markdown-code-face ((t (:inherit nil :background "gray10"))))
   '(markdown-pre-face  ((t (:inherit font-lock-constant-face)))))

  (defun my-howm-fix-after-super-save (&rest _)
    (when (and (eq major-mode 'howm-mode)
               (buffer-file-name))
      (message "howm-fix running: %s" (buffer-file-name))
      (shell-command
       (format "perl ~/.emacs.d/bin/howm-fix-code-comments.pl %s"
               (shell-quote-argument (buffer-file-name))))))
  (advice-add 'super-save-command :after #'my-howm-fix-after-super-save)

  (defun my-delete-tmp-markdown-html ()
    "Delete /tmp/burl*.html when killed markdown buffer."
    (when (and (derived-mode-p 'markdown-mode)
	       (buffer-file-name))
      (let ((temp-files (file-expand-wildcards "/tmp/burl*.html")))
	(dolist (file temp-files)
	  (when (file-exists-p file)
	    (delete-file file)
	    (message "Deleted temporary file: %s" file))))))
  (add-hook 'kill-buffer-hook #'my-delete-tmp-markdown-html)

  (defun md2pdf ()
    "Generate PDF from currently open markdown via pandoc + lualatex."
    (interactive)
    (let* ((filename (buffer-file-name (current-buffer)))
           (pdffile  (concat (file-name-sans-extension filename) ".pdf")))
      (if (zerop (call-process-shell-command
                  (concat "pandoc " filename
                          " -o " pdffile
                          " -V mainfont=IPAPGothic -V geometry:margin=20mm -V fontsize=14pt --pdf-engine=lualatex")))
          (call-process "xdg-open" nil nil nil pdffile)
	(message "md2pdf: pandoc failed"))))

  (defun md2docx ()
    "Generate docx from currently open markdown."
    (interactive)
    (let* ((filename (buffer-file-name (current-buffer)))
           (docxfile  (concat (file-name-sans-extension filename) ".docx")))
      (if (zerop (call-process-shell-command
                  (concat "pandoc " filename
                          " -t docx -o " docxfile
                          " -V mainfont=IPAPGothic -V fontsize=16pt --highlight-style=zenburn")))
          (call-process "xdg-open" nil nil nil docxfile)
	(message "md2docx: pandoc failed")))))


(require 'my-markdown)


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 60-markdown.el ends here
