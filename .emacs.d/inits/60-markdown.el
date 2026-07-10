;;; 60-markdown.el --- Markdown mode configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Related custom functions (~/.emacs.d/elisp/my-markdown.el):
;;   `my-howm-fix-code-comments' -- Convert comment symbols in code blocks
;;   `gen-toc-term'              -- Generate TOC via gnome-terminal
;;
;;; Code:

;; ============================================================
;;  Markdown Mode
;; ============================================================

(autoload 'my-howm-fix-code-comments "my-markdown" nil t)
(autoload 'gen-toc-term "my-markdown" nil t)

(leaf markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode))
  :bind (("C-c RET" . markdown-follow-link-at-point)
         ("C-c #"   . my-howm-fix-code-comments)
         ("C-c t"   . gen-toc-term)
         ("C-c C-c" . markdown-do-command)
         ("M-RET"   . markdown-insert-list-item))
  :config
  (setq markdown-command                    "pandoc -f markdown+header_attributes-raw_html -t html5")
  (setq markdown-fontify-code-blocks-natively t)  ; コードブロックにシンタックスハイライト
  (setq markdown-header-scaling              t)    ; 見出しのサイズを段階的に
  (setq markdown-hide-markup                 nil)  ; マークアップを表示 (t で隠す)
  (setq markdown-command-needs-filename      t)
  (setq markdown-preview-use-browser         t)
  (setq browse-url-browser-function         'browse-url-generic)
  (setq browse-url-generic-program          "google-chrome")
  (setq markdown-content-type               "application/xhtml+xml")
  (setq markdown-css-paths
        (list (expand-file-name "~/.emacs.d/elisp/css/markdown-cream.css")))


  ;; ============================================================
  ;;  Preview HTML Header  (highlight.js + TOC スタイル)
  ;; ============================================================

  (setq markdown-xhtml-header-content
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
          /* TOC: 1階層目のリストマーカーを非表示 */
          .toc > ul { list-style: none; padding-left: 0; }
          .toc > ul > li { list-style-type: none; }
          /* TOC: 2階層目はディスク表示 */
          .toc > ul > li > ul { list-style-type: disc; padding-left: 25px; }
          .toc > ul > li > ul > li { color: #555; }
     </style>"))

  (custom-set-faces
   '(markdown-code-face ((t (:inherit nil :background "gray10"))))
   '(markdown-pre-face  ((t (:inherit font-lock-constant-face)))))


  ;; ============================================================
  ;;  Howm Fix Hook  (super-save 後にコメント記号を修正)
  ;; ============================================================

  (defun my-howm-fix-after-super-save (&rest _)
    "super-save 後に howm バッファのコードブロック内コメントを修正する."
    (when (and (eq major-mode 'howm-mode)
               (buffer-file-name))
      (message "howm-fix running: %s" (buffer-file-name))
      (shell-command
       (format "perl ~/.emacs.d/bin/howm-fix-code-comments.pl %s"
               (shell-quote-argument (buffer-file-name))))))

  (advice-add 'super-save-command :after #'my-howm-fix-after-super-save)


  ;; ============================================================
  ;;  Temp File Cleanup  (プレビュー用 /tmp/burl*.html を自動削除)
  ;; ============================================================

  (defun my-delete-tmp-markdown-html ()
    "Delete /tmp/burl*.html when a markdown buffer is killed."
    (when (and (derived-mode-p 'markdown-mode)
               (buffer-file-name))
      (dolist (file (file-expand-wildcards "/tmp/burl*.html"))
        (when (file-exists-p file)
          (delete-file file)
          (message "Deleted temporary file: %s" file)))))

  (add-hook 'kill-buffer-hook #'my-delete-tmp-markdown-html))


;; ============================================================
;;  Pandoc Export  (PDF / DOCX)
;; ============================================================

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


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 60-markdown.el ends here
