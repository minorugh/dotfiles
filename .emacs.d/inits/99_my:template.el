;;; 99_my:template.el --- User template configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My template configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my:diary-new-post ()
  "Open diary file and insert template."
  (interactive)
  (find-file (expand-file-name "diary.txt" "~/Dropbox/GH/dia/"))
  (view-mode 0)
  (goto-char 0)
  ;; Insert a new date if the date has changed
  (setq string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (unless (string-match (format-time-string "%Y%m:") string)
	(forward-line -1)
	(insert (format-time-string "%Y%m:\n")))
  ;; Insert template
  (goto-char (point-min))
  (forward-line)
  (insert
   ";--------------------------------------------------------\n"
   (format-time-string "*[%Y%m%d]%Y年%-m月%-d日\n")
   ";--------------------------------------------------------\n"
   (format-time-string "-*[%Y%m%d%H%M%S]\n")
   "-(\n\n-)\n\n")
  (forward-line -5)
  (forward-char 18))

(defun my:teirei-new-post ()
  "Open teirei file and insert template."
  (interactive)
  (find-file "~/Dropbox/GH/teirei/tex/teirei.txt")
  (view-mode 0)
  (my:minoru_sen))

(defun my:swan-new-post ()
  "Open swan file and insert template."
  (interactive)
  (find-file "~/Dropbox/GH/swan/tex/swan.txt")
  (view-mode 0)
  (my:minoru_sen))

(defun my:minoru_sen ()
  "Insert template."
  (interactive)
  (goto-char (point-min))
  (insert
   (format-time-string "%Y%m:\n")
   (format-time-string "%Y年%-m月%-d日（参加者 名）\n"))
  (forward-line -1)
  (forward-char 15))

(defun my:ap-new-post ()
  "Open ap file and insert template."
  (interactive)
  (goto-char (point-min))
  (forward-line)
  (insert
   ";--------------------------------------------------------------------\n"
   (format-time-string "*\n")
   ";--------------------------------------------------------------------\n"
   (format-time-string "-*[%Y%m%d%H%M%S]小路紫峡\n")
   "-(\n<small>()</small><br>\n<-mi>\n-)\n=[\n= Feedback\n-[\n\n-]\n=]\n-elink\n")
  (forward-line -14)
  (forward-char 1))

(defun my:apvoice-new-post ()
  "Open apsh file and insert template."
  (interactive)
  (find-file (expand-file-name "apvoice.txt" "~/Dropbox/GH/apvoice/"))
  (view-mode 0)
  (goto-char 0)
  ;; Insert a new date if the date has changed
  (setq string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (unless (string-match (format-time-string "%Y%m:") string)
	(forward-line -1)
	(insert (format-time-string "%Y%m:\n")))
  ;; Insert template
  (goto-char (point-min))
  (forward-line)
  (insert
   ";--------------------------------------------------------------------\n"
   (format-time-string "*[%Y%m%d]\n")
   ";--------------------------------------------------------------------\n"
   (format-time-string "-*[%Y%m%d%H%M%S]高野素十\n")
   "<div style=\"margin:1em\">\n()\n</div>\n-(\n--\n=[\n= 合評\n"
   (format-time-string "<iframe class=\"autoHeight\" src=\"apiframe.cgi?%Y%m%d\"></iframe>\n")
   "\; -[\n\; -]\n=]\n-)\n\n")
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 11))

(defun my:tselext-new-post ()
  "Open tselext file and insert template."
  (interactive)
  (find-file (expand-file-name "select.txt" "~/Dropbox/GH/tselext/"))
  (view-mode 0)
  (goto-char 0)
  ;; Insert a new date if the date has changed
  (setq string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (unless (string-match (format-time-string "%Y%m:") string)
	(forward-line -1)
	(insert (format-time-string "%Y%m:\n")))
  (forward-line 1)
  (setq string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (unless (string-match (format-time-string "*%Y年%-m月") string)
	(forward-line -1)
	(insert (format-time-string "*%Y年%-m月\n")))
  ;; Insert template
  (goto-char (point-min))
  (forward-line 2)
  (insert
   ";--------------------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   ";--------------------------------------------------------------------\n"
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n"
   ";--------------------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   ";--------------------------------------------------------------------\n"
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n"
   ";--------------------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   ";--------------------------------------------------------------------\n"
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n")
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 11))

(defun my:dselext-new-post ()
  "Open tselext file and insert template."
  (interactive)
  (find-file (expand-file-name "select.txt" "~/Dropbox/GH/d_selext/"))
  (view-mode 0)
  (goto-char 0)
  ;; Insert a new date if the date has changed
  (setq string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (unless (string-match (format-time-string "%Y%m:") string)
	(forward-line -1)
	(insert (format-time-string "%Y%m:\n")))
  (forward-line 1)
  (setq string (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
  (unless (string-match (format-time-string "*%Y年%-m月%-d日") string)
	;; (forward-line 1)
	(insert (format-time-string "*%Y年%-m月%-d日\n")))
  ;; Insert template
  (goto-char (point-min))
  (forward-line 2)
  (insert
   ";--------------------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n"
   ";--------------------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n"
   ";--------------------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n")
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 11))

(defun my:haiku-note ()
  "Open haiku note file."
  (interactive)
  (find-file (format-time-string "~/Dropbox/howm/haiku/haikunote.%Y.txt"))
  (goto-char (point-min)))

(defun my:haiku-note-post ()
  "Insert template."
  (interactive)
  (find-file (format-time-string "~/Dropbox/howm/haiku/haikunote.%Y.txt"))
  (goto-char (point-min))
  (forward-line 2)
  (insert
   (format-time-string "> %Y年%-m月%-d日 (%a)\n")
   (format-time-string "PLACE:\n\n"))
  (forward-line -2)
  (forward-char 6))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 99_my:template.el ends here
