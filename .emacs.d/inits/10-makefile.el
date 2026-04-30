;;; 10-makefile.el --- Makefile support: targets, ivy, compile  -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Personal settings that summarize Makefile operations.
;; Keybinding @ (evil normal) for target selection via ivy.
;; C-c C-e to toggle read-only.  qq (key-chord) also toggles read-only.
;;
;;; Code:

(leaf makefile-functions
  :after (evil key-chord)
  :init
  (defun my-makefile-mode-setup ()
    "Setup for `makefile-mode'."
    (local-set-key (kbd "C-c C-e") #'my-makefile-toggle-readonly)
    (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated)
    (key-chord-define (current-local-map) "qq" #'my-makefile-toggle-readonly))

  (defun my-dired-mode-setup ()
    "Setup for `dired-mode'."
    (evil-local-set-key 'normal (kbd "@") #'my-make-ivy-integrated))

  (add-hook 'makefile-mode-hook #'my-makefile-mode-setup)
  (add-hook 'dired-mode-hook    #'my-dired-mode-setup)

  ;; Return Makefile path: dired, buffer file, or default-directory.
  (defun my-make--find-makefile ()
    "Find Makefile for current context."
    (let ((dir (cond
		((derived-mode-p 'dired-mode) (dired-current-directory))
		((and buffer-file-name
                      (string= (file-name-nondirectory buffer-file-name) "Makefile"))
		 (file-name-directory buffer-file-name))
		(t default-directory))))
      (let ((mk (expand-file-name "Makefile" dir)))
	(when (file-exists-p mk) mk))))


  ;; Select and run targets via ivy.  Works in makefile-mode and dired.
  (defun my-make-ivy-integrated ()
    "Run Makefile target via ivy."
    (interactive)
    (require 'ivy)
    (let ((makefile (my-make--find-makefile)))
      (unless makefile (user-error "Makefileが見つかりません"))
      (let ((candidates nil)
            (orig-buf   (current-buffer))
            (orig-point (point))
            (map        (copy-keymap ivy-minibuffer-map)))
	;; Real-time preview
	(define-key map (kbd "<down>") 'ivy-next-line-and-call)
	(define-key map (kbd "<up>")   'ivy-previous-line-and-call)
	;; C-c C-c to run make
	(define-key map (kbd "C-c C-c")
		    (lambda ()
		      (interactive)
		      (ivy-exit-with-action
		       (lambda (x)
			 (let ((target (cdr x)))
			   (compile (format "make -C %s %s"
					    (file-name-directory makefile)
					    target)))))))
	;; Parses candidates from Makefile buffer
	(with-current-buffer (find-file-noselect makefile)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward "^\\([^:# \t\n]+\\):.*?##[ \t]*\\(.*\\)$" nil t)
              (let* ((target     (match-string 1))
                     (desc       (match-string 2))
                     (pos        (match-beginning 1))
                     (target-fmt (propertize (format "%-20s" target) 'face 'font-lock-function-name-face))
                     (desc-fmt   (propertize desc 'face 'font-lock-comment-face)))
		(push (cons (concat target-fmt " " desc-fmt)
                            (propertize target 'pos pos 'makefile makefile))
                      candidates)))))

	(if (not candidates)
            (message "ターゲットが見つかりませんでした。")
          (ivy-read "Targets: "
                    (nreverse candidates)
                    :keymap map
                    :action (lambda (x)
                              (let ((pos (get-text-property 0 'pos (cdr x)))
                                    (mk  (get-text-property 0 'makefile (cdr x))))
				(find-file mk)
				(goto-char pos)
				(recenter)))
                    :unwind (lambda ()
                              (unless (eq ivy-exit 'done)
				(switch-to-buffer orig-buf)
				(goto-char orig-point)
				(recenter)))
                    :caller 'my-make-ivy-integrated)))))

  ;; Toggle read-only and report status in minibuffer.
  (defun my-makefile-toggle-readonly ()
    "Toggle read-only mode."
    (interactive)
    (read-only-mode 'toggle)
    (message "Makefile: %s" (if buffer-read-only "read-only" "EDITABLE")))

  ;; Run `make git' from the nearest Makefile root.
  (defun my-make-git ()
    "Run make git in repository root."
    (interactive)
    (let* ((dir  (or buffer-file-name default-directory))
           (root (locate-dominating-file dir "Makefile")))
      (if root
          (let ((default-directory root))
            (compile "make git"))
	(message "Makefile not found")))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 10-makefile.el ends here
