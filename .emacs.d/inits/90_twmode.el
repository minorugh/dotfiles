;;; 90_twmode.el --- Twittering configurations.  -*- lexical-binding: t; no-byte-compile:t -*-
;;; Commentary:
;;
;; Handle multiple accounts in Twittering-mode
;;
;;; Code:
;;(setq debug-on-error t)

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
  (setq twittering-initial-timeline-spec-string '("gospelhaiku" ":mentions" ":home"))
  (my:reload-twit))

;; Switch multiple accounts
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



;; Twitterring-mode settings
(leaf twittering-mode
  :ensure t
  :bind ((:twittering-mode-map
		  ("F" . twittering-favorite)
		  ("r" . twittering-enter)
		  ("Q" . twittering-organic-retweet)
		  ("T" . twittering-native-retweet)
		  ("M" . twittering-direct-message)
		  ("." . twittering-current-timeline) ;; refresh
		  ("f" . twittering-kill-and-switch-to-next-timeline)
		  ("b" . twittering-kill-and-switch-to-previous-timeline)))
  :config
  (setq twittering-private-info-file
        (expand-file-name "~/Dropbox/backup/gpg/twittering-mode1.gpg"))
  ;; timeline to read on startup
  (setq twittering-initial-timeline-spec-string '("minoruGH" ":retweets_of_me" ":mentions" ":home"))
  :custom
  ((twittering-use-master-password . t)
   (twittering-use-ssl . t)
   (twittering-timer-interval . 40)
   (twittering-convert-fix-size . 48)
   (twittering-update-status-function . 'twittering-update-status-from-pop-up-buffer)
   (twittering-icon-mode . t)
   ;; Tweet display count
   (twittering-number-of-tweets-on-retrieval . 50)
   (twittering-scroll-mode . nil)
   (twittering-pop-to-buffer-function . 'pop-to-buffer)
   ;; RT format
   (twittering-retweet-format . '(nil _ " %u QT @%s: %t"))
   ;; TL flows downward
   (twittering-reverse-mode . t)
   ;; Display format
   (twittering-status-format . "%i%FACE[twittering-mode-name-face]{%s(%S) %p }%FACE[twittering-mode-reply-face]{%r%R}\n%FACE[twittering-mode-text-face]{%t}\n%FACE[twittering-mode-hide-face]{%C{%m/%d %H:%M:%S}(%@)}%FACE[twittering-mode-hide-face]{  from %f%L}%FACE[twittering-mode-sepa-face]{\n\n----------------------------------------------------------\n}")
   ;; URL shortening service to j.mp
   (twittering-tinyurl-service . 'j.mp)
   (twittering-bitly-login . "minorugh")
   (twittering-bitly-api-key . "R_f0b3887698d4d171004f55af6e6a199e"))
  :config
  (defalias 'epa--decode-coding-string 'decode-coding-string)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 90_twmode.el ends here
