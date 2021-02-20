;;; 90_my:chromium.el --- User chromium configuration.  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; For WSL
(leaf browse-url-in-WSL
  :url "https://adam.kruszewski.name/2017/09/emacs-in-wsl-and-opening-links/"
  :if (getenv "WSLENV")
  :config
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
		(cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
			browse-url-generic-args     cmd-args
			browse-url-browser-function 'browse-url-generic
			search-web-default-browser 'browse-url-generic))))

;; Chromium
(defun chromium-calendar ()
  "Open Google-calendar with chrome."
  (interactive)
  (browse-url "https://calendar.google.com/calendar/r"))

(defun chromium-weather ()
  "Open tenki.jp with chrome."
  (interactive)
  (browse-url "https://tenki.jp/week/6/31/"))

(defun chromium-google-news ()
  "Open Google-news with chrome."
  (interactive)
  (browse-url "https://news.google.com/topstories?hl=ja&gl=JP&ceid=JP:ja"))

(defun chromium-pocket ()
  "Open pocket with chrome."
  (interactive)
  (browse-url "https://getpocket.com/a/queue/"))

(defun chromium-keep ()
  "Open keep with chromium."
  (interactive)
  (browse-url "https://keep.google.com/u/0/"))

(defun chromium-keep-new ()
  "Open new keep with chrome."
  (interactive)
  (browse-url "https://keep.new/"))

(defun chromium-homepage ()
  "Open my homepage with crome."
  (interactive)
  (browse-url "https://gospel-haiku.com/"))

(defun chromium-gmail ()
  "Open gmail with chrome."
  (interactive)
  (browse-url "https://mail.google.com/mail/"))

(defun chromium-tweetdeck ()
  "Open tweetdeck with chrome."
  (interactive)
  (browse-url "https://tweetdeck.twitter.com/"))

(defun chromium-slack ()
  "Open slack with chrome."
  (interactive)
  (browse-url "https://emacs-jp.slack.com/messages/C1B73BWPJ/"))

(defun chromium-bible ()
  "Open slack with chrome."
  (interactive)
  (browse-url "https://j-bible.jimdofree.com/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 99_my:chromium.el ends here
