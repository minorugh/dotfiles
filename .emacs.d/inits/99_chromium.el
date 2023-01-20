;;; 99_chromium.el --- Chromium configurations.  -*- lexical-binding: t -*-
;;; Commentary: Extentions function for browse url
;;; Code:
;; (setq debug-on-error t)

(defun chromium-yahoo-japan ()
  "Chromium Yahoo."
  (interactive)
  (browse-url "https://www.yahoo.co.jp/"))

(defun chromium-weather ()
  "Chrimium Wether site."
  (interactive)
  (browse-url "https://tenki.jp/week/6/31/"))

(defun chromium-tweetdeck ()
  "Chromium tweetdeck site."
  (interactive)
  (browse-url "https://tweetdeck.twitter.com/"))

(defun chromium-homepage ()
  "Chromium hompage."
  (interactive)
  (browse-url "https://gospel-haiku.com/"))

(defun chromium-calendar ()
  "Chromium Google calendar."
  (interactive)
  (browse-url "https://calendar.google.com/calendar/r"))

(defun chromium-nhk-news ()
  "Chromium NHK news."
  (interactive)
  (browse-url "https://www.nhk.or.jp/news/"))

(defun chromium-pocket ()
  "Chromium pocket."
  (interactive)
  (browse-url "https://getpocket.com/a/queue/"))

(defun chromium-keep ()
  "Chromium keep."
  (interactive)
  (browse-url "https://keep.google.com/u/0/"))

(defun chromium-keep-new ()
  "Chromium keep new."
  (interactive)
  (browse-url "https://keep.new/"))

(defun chromium-github ()
  "Chromium github.io."
  (interactive)
  (browse-url "https://github.com/minorugh"))

(defun chromium-xsrv ()
  "Chromium github.io."
  (interactive)
  (browse-url "https://secure.xserver.ne.jp/xserver/sv13268/minorugh.xsrv.jp/?action_user_index=true"))

(defun chromium-gmail ()
  "Chromium gmail."
  (interactive)
  (browse-url "https://mail.google.com/mail/"))

(defun chromium-dropbox ()
  "Chromium gmail."
  (interactive)
  (browse-url "https://www.dropbox.com/h?role=personal/"))

(defun chromium-tegaki ()
  "Chromium tegaki."
  (interactive)
  (browse-url "https://mojinavi.com/tegaki"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 99_chromium.el ends here
