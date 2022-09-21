;;; 99_chromium.el --- Chromium configurations.  -*- lexical-binding: t -*-
;;; Commentary: Extentions function for browse url
;;; Code:
;; (setq debug-on-error t)

(defun chromium-translate ()
  "Open google translate with chromium."
  (interactive)
  (if (use-region-p)
	  (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
		(deactivate-mark)
		(if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
						  string)
			(browse-url (concat "https://translate.google.com/?source=gtx#en/ja/"
								(url-hexify-string string)))
		  (browse-url (concat "https://translate.google.com/?source=gtx#ja/en/"
							  (url-hexify-string string)))))
	(let ((string (read-string "Google Translate: ")))
	  (if (string-match
		   (format "\\`[%s]+\\'" "[:ascii:]")
		   string)
		  (browse-url
		   (concat "https://translate.google.com/?source=gtx#en/ja/" (url-hexify-string string)))
		(browse-url
		 (concat "https://translate.google.com/?source=gtx#ja/en/" (url-hexify-string string)))))))

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

(defun chromium-github.io ()
  "Chromium github.io."
  (interactive)
  (browse-url "https://minorugh.github.io/"))

(defun chromium-gmail ()
  "Chromium gmail."
  (interactive)
  (browse-url "https://mail.google.com/mail/"))

(defun chromium-tegaki ()
  "Chromium tegaki."
  (interactive)
  (browse-url "https://mojinavi.com/tegaki"))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 99_chromium.el ends here
