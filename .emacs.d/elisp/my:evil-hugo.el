;;; my:evil-hugo.el --- User custom Evil configurations. -*- lexical-binding: t -*-
;;; Commentary:

;; This is a customized version of the easy-hugo-newpost function in easy-hugo.el.
;; When easy-hugo-newpost is invoked in evil-mode,
;; a new file is opened in evil-normal-state.
;; so this is modified to open it in evil-insert-state.

;;; Code:
;; (setq debug-on-error t)

(leaf *my:evil-easy-hugo
  :defun easy-hugo--org-headers easy-hugo-with-env evil-emacs-state
  :config
  (with-eval-after-load 'easy-hugo
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

	 ;; Customize from here
	 (when evil-mode
	   (evil-emacs-state))
	 ;; so far

	 (when (and easy-hugo-org-header
		    (string-equal file-ext "org"))
	   (insert (easy-hugo--org-headers (file-name-base post-file))))
	 (goto-char (point-max))
	 (save-buffer))))

    (defun easy-hugo-view ()
      "Open the file on the pointer with `view-mode'."
      (interactive)
      (easy-hugo-with-env
       (if (equal (buffer-name (current-buffer)) easy-hugo--buffer-name)
	   (progn
	     (unless (or (string-match "^$" (thing-at-point 'line))
			 (eq (point) (point-max))
			 (> (+ 1 easy-hugo--forward-char) (length (thing-at-point 'line))))
	       (let ((file (expand-file-name
			    (substring (thing-at-point 'line) easy-hugo--forward-char -1)
			    easy-hugo-postdir)))
		 (when (and (file-exists-p file)
			    (not (file-directory-p file)))
		   (view-file file)
		   ;; Customize from here
		   (when evil-mode
		     (evil-emacs-state))
		   ;; so far
		   ))))
	 (view-file buffer-file-name))))))


(provide 'my:evil-hugo)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my:evil-hugo.el ends here
