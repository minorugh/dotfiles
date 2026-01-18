;;; my:evil-hugo.el --- User custom Evil configurations. -*- lexical-binding: t -*-
;;; Commentary:

;; This is a customized version of the easy-hugo-newpost function in easy-hugo.el.
;; When easy-hugo-newpost is invoked in evil-mode, open new file in evil-insert-state.

;;; Code:
;; (setq debug-on-error t)

(leaf *my:evil-easy-hugo
  :defun easy-hugo--org-headers easy-hugo-with-env evil-emacs-state
  :after easy-hugo
  :config
  (defun easy-hugo-newpost (post-file)
    "Create a new post with hugo.
POST-FILE needs to have an extension '.md', '.org', '.ad', '.rst',
'.mmark', or '.html'."
    (interactive (list (read-from-minibuffer
			"Filename: "
			`(,easy-hugo-default-ext . 1) nil nil nil)))
    (easy-hugo-with-env
     (let ((filename (expand-file-name post-file easy-hugo-postdir))
	   (file-ext (file-name-extension post-file)))
       (when (not (member file-ext easy-hugo--formats))
	 (error "Please enter .%s, .%s, .%s, .%s, .%s, or .%s file name"
		easy-hugo-markdown-extension
		easy-hugo--org-extension
		easy-hugo-asciidoc-extension
		easy-hugo--rst-extension
		easy-hugo--mmark-extension
		easy-hugo-html-extension))
       (when (file-exists-p (file-truename filename))
	 (error "%s already exists!" filename))
       (call-process
	easy-hugo-bin nil "*hugo*" t "new"
	(file-relative-name filename
			    (expand-file-name "content" easy-hugo-basedir)))
       (when (get-buffer "*hugo*")
	 (kill-buffer "*hugo*"))
       (find-file filename)
       ;; Open with evil-emacs-stete if evil-mode
       (when evil-mode
	 (evil-emacs-state))
       (goto-char (point-max))
       (save-buffer)))))


(provide 'my:evil-easy-hugo)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my:evil-easy-hugo.el ends here
