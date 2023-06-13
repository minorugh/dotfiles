;;; 99_chromium.el --- Chromium configurations.  -*- lexical-binding: t -*-
;;; Commentary: Extentions function for browse url
;;; Code:
;; (setq debug-on-error t)

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

(defun chromium-weather ()
  "Chromium tenki."
  (interactive)
  (browse-url "https://tenki.jp/week/6/31/"))

(defun chromium-keep-new ()
  "Chromium keep new."
  (interactive)
  (browse-url "https://keep.new/"))

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
