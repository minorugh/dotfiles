;;; evil-easy-hugo.el --- User custom configurations. -*- no-byte-compile: t; -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(defun easy-hugo-newpost (post-file)
  "Overwrite `easy-hugo-newpost' for evil-mode POST-FILE."
  (interactive (list (read-from-minibuffer
		      "Filename: "
		      `(,easy-hugo-default-ext . 1) nil nil nil)))
  (easy-hugo-with-env
   (let ((filename (expand-file-name post-file easy-hugo-postdir))
	 (file-ext (file-name-extension post-file)))
     (when (not (member file-ext easy-hugo--formats))
       (error "Please enter .%s or .org or .%s or .rst or .mmark or .%s file name"
	      easy-hugo-markdown-extension
	      easy-hugo-asciidoc-extension
	      easy-hugo-html-extension))
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" filename))
     (if (null easy-hugo-org-header)
	 (call-process easy-hugo-bin nil "*hugo*" t "new"
		       (file-relative-name filename
					   (expand-file-name "content" easy-hugo-basedir)))
       (progn
	 (if (or (string-equal file-ext easy-hugo-markdown-extension)
		 (string-equal file-ext easy-hugo-asciidoc-extension)
		 (string-equal file-ext "rst")
		 (string-equal file-ext "mmark")
		 (string-equal file-ext easy-hugo-html-extension))
	     (call-process easy-hugo-bin nil "*hugo*" t "new"
			   (file-relative-name filename
					       (expand-file-name "content" easy-hugo-basedir))))))
     (when (get-buffer "*hugo*")
       (kill-buffer "*hugo*"))
     (find-file filename)
     (when evil-mode
       (evil-insert-state))
     (when (and easy-hugo-org-header
		(string-equal file-ext "org"))
       (insert (easy-hugo--org-headers (file-name-base post-file))))
     (goto-char (point-max))
     (save-buffer))))


(provide 'evil-easy-hugo)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; evil-easy-hugo.el ends here
