;;; 80_twitter.el --- major mode for twitter on emacs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;(setq debug-on-error t)

(leaf switch-multiple-accounts
  :init
  (defun my:reload-twit ()
	"Reload twit buffers."
	(mapc
	 (lambda (buffer)
	   (twittering-deactivate-buffer buffer)
	   (kill-buffer buffer))
	 (twittering-get-buffer-list))
	(twittering-unregister-killed-buffer)
	;; Clear variables
	(setq twittering-private-info-file-loaded nil)
	(setq twittering-account-authorization nil)
	(setq twittering-oauth-access-token-alist nil)
	(setq twittering-buffer-info-list nil)
	(setq twittering-timeline-data-table (make-hash-table :test 'equal))
	(twit))

  (defun twit-1 ()
	"Log in to @minorugh."
	(interactive)
	(setq twittering-private-info-file
		  (expand-file-name "~/Dropbox/backup/gpg/twittering-mode1.gpg"))
	;; timeline to read on startup
	(setq twittering-initial-timeline-spec-string '("minoruGH" ":retweets_of_me" ":mentions" ":home"))
	(my:reload-twit))

  (defun twit-2 ()
	"Log in to @gospelhaiku."
	(interactive)
	(setq twittering-private-info-file
		  (expand-file-name "~/Dropbox/backup/gpg/twittering-mode2.gpg"))
	;; timeline to read on startup
	(setq twittering-initial-timeline-spec-string '("gospelhaiku" ":retweets_of_me" ":mentions" ":home"))
	(my:reload-twit)))


(leaf twittering-mode
  :ensure t
  :require t
  :config
  (bind-key "F" 'twittering-favorite twittering-mode-map)
  (bind-key "r" 'twittering-enter twittering-mode-map)
  (bind-key "Q" 'twittering-organic-retweet twittering-mode-map)
  (bind-key "T" 'twittering-native-retweet twittering-mode-map)
  (bind-key "M" 'twittering-direct-message twittering-mode-map)
  (bind-key "." 'twittering-current-timeline twittering-mode-map) ;; refresh
  (bind-key "f" 'twittering-kill-and-switch-to-next-timeline twittering-mode-map)
  (bind-key "b" 'twittering-kill-and-switch-to-previous-timeline twittering-mode-map)
  :init
  (setq twittering-use-master-password t)
  (setq twittering-use-ssl t)
  (setq twittering-timer-interval 40)
  (setq twittering-convert-fix-size 48)
  (setq twittering-update-status-function 'twittering-update-status-from-pop-up-buffer)
  (setq twittering-icon-mode t)
  (setq twittering-number-of-tweets-on-retrieval 50)  ;; Tweet display count
  (setq twittering-scroll-mode nil)
  (setq twittering-pop-to-buffer-function 'pop-to-buffer)
  ;; RT format
  (setq twittering-retweet-format '(nil _ " %u QT @%s: %t"))
  ;; TL flows downward
  (setq twittering-reverse-mode t)
  ;; Display format
  (setq twittering-status-format "%i%FACE[twittering-mode-name-face]{%s(%S) %p }%FACE[twittering-mode-reply-face]{%r%R}\n%FACE[twittering-mode-text-face]{%t}\n%FACE[twittering-mode-hide-face]{%C{%m/%d %H:%M:%S}(%@)}%FACE[twittering-mode-hide-face]{  from %f%L}%FACE[twittering-mode-sepa-face]{\n\n----------------------------------------------------------\n}")
  ;; URL shortening service to j.mp
  (setq twittering-tinyurl-service 'j.mp)
  (setq twittering-bitly-login "minorugh")
  (setq twittering-bitly-api-key "R_f0b3887698d4d171004f55af6e6a199e")

  ;; look for name
  (defface twittering-mode-name-face
    '((t (:foreground "#81a2be"))) nil)

  ;; look for tweet characters
  (defface twittering-mode-text-face
    '((t (:foreground "#ffffff"))) nil)

  ;; look for date and time
  (defface twittering-mode-hide-face
    '((t (:foreground "#f0c674"))) nil)

  ;; look for in reply to
  (defface twittering-mode-reply-face
    '((t (:foreground "#b5bd68"))) nil)

  ;; look for break
  (defface twittering-mode-sepa-face
    '((t (:foreground "#969896"))) nil)

  (defadvice twittering-visit-timeline (before kill-buffer-before-visit-timeline activate)
    "Delete current TL buffer before opening new TL."
    (twittering-kill-buffer))

  (defun twittering-kill-and-switch-to-next-timeline ()
    "Open next TL of twittering-initial-timeline-spec-string."
    (interactive)
    (when (twittering-buffer-p)
      (let* ((buffer-list twittering-initial-timeline-spec-string)
			 (following-buffers (cdr (member (buffer-name (current-buffer)) buffer-list)))
			 (next (if following-buffers
					   (car following-buffers)
					 (car buffer-list))))
		(unless (eq (current-buffer) next)
		  (twittering-visit-timeline next)))))

  (defun twittering-kill-and-switch-to-previous-timeline ()
    "Open previous TL of twittering-initial-timeline-spec-string."
    (interactive)
    (when (twittering-buffer-p)
      (let* ((buffer-list (reverse twittering-initial-timeline-spec-string))
			 (preceding-buffers (cdr (member (buffer-name (current-buffer)) buffer-list)))
			 (previous (if preceding-buffers
						   (car preceding-buffers)
						 (car buffer-list))))
		(unless (eq (current-buffer) previous)
		  (twittering-visit-timeline previous))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars callargs)
;; End:
;;; 80_twitter.el ends here
