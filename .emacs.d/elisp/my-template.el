;;; my-template.el --- User template configuration. -*- lexical-binding: t -*-
;;; Commentary:

;; Template functions for haiku writing files.
;; Loaded via (require 'my-template) in 40-hydra-dired.el and 40-hydra-menu.el

;;; Code:

(declare-function evil-emacs-state "evil-states")

;;;###autoload
(defun my-diary-new-post ()
  "Open diary file and insert template."
  (interactive)
  (find-file (expand-file-name "diary.txt" "~/Dropbox/GH/dia/"))
  (goto-char (point-min))
  (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format-time-string "%Y%m:") string)
      (forward-line -1)
      (insert (format-time-string "%Y%m:\n"))))
  (goto-char (point-min))
  (forward-line)
  (insert
   ";--------------------------------------------------------\n"
   (format-time-string "*[%Y%m%d]%Y年%-m月%-d日\n")
   ";--------------------------------------------------------\n"
   (format-time-string "-*[%Y%m%d%H%M%S]\n")
   "-(\n\n-)\n\n")
  (forward-line -5)
  (forward-char 18)
  (evil-emacs-state))

;;;###autoload
(defun my-tpdia-new-post ()
  "Open tpdia file and insert template."
  (interactive)
  (find-file (expand-file-name "dia.txt" "~/Dropbox/GH/tpdia/"))
  (goto-char (point-min))
  (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format-time-string "%Y%m:") string)
      (forward-line -1)
      (insert (format-time-string "%Y%m:\n"))))
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

;;;###autoload
(defun my-minoru_sen ()
  "Insert kukai header template."
  (interactive)
  (goto-char (point-min))
  (insert
   (format-time-string "%Y%m:\n")
   (format-time-string "%Y年%-m月%-d日（参加者 名）\n"))
  (forward-line -1)
  (forward-char 15))

;;;###autoload
(defun my-teirei-new-post ()
  "Open teirei file and insert template."
  (interactive)
  (find-file "~/Dropbox/GH/teirei/tex/teirei.txt")
  (my-minoru_sen))

;;;###autoload
(defun my-swan-new-post ()
  "Open swan file and insert template."
  (interactive)
  (find-file "~/Dropbox/GH/swan/tex/swan.txt")
  (my-minoru_sen))

;;;###autoload
(defun my-m_kukai-new-post ()
  "Open m_kukai file and insert template."
  (interactive)
  (find-file "~/Dropbox/GH/m_select/tex/mkukai.txt")
  (my-minoru_sen))

;;;###autoload
(defun my-ap-new-post ()
  "Insert ap template at current position."
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

;;;###autoload
(defun my-apvoice-new-post ()
  "Open apvoice file and insert template."
  (interactive)
  (find-file (expand-file-name "apvoice.txt" "~/Dropbox/GH/apvoice/"))
  (when evil-mode
    (evil-emacs-state))
  (goto-char (point-min))
  (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format-time-string "%Y%m:") string)
      (forward-line -1)
      (insert (format-time-string "%Y%m:\n"))))
  (goto-char (point-min))
  (forward-line)
  (insert
   ";--------------------------------------------------------------------\n"
   (format-time-string "*[%Y%m%d] \n")
   ";--------------------------------------------------------------------\n"
   (format-time-string "-*[%Y%m%d%H%M%S]阿波野青畝\n")
   "<div style=\"margin:1em\">\n()\n</div>\n-(\n<-hi>\n--\n=(\n<small>-- 自解(抄) --</small>\n--\n=)\n--\n<h3>合評</h3>\n"
   "<div class=\"border\">\n"
   (format-time-string "<iframe class=\"autoHeight\" src=\"apiframe.cgi?%Y%m%d\"></iframe>\n")
   "</div>\n-)\n\n")
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 11))

;;;###autoload
(defun my-tselext-new-post ()
  "Open tselext file and insert template."
  (interactive)
  (find-file (expand-file-name "select.txt" "~/Dropbox/GH/tselext/"))
  (goto-char (point-min))
  (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format-time-string "%Y%m:") string)
      (forward-line -1)
      (insert (format-time-string "%Y%m:\n"))))
  (forward-line 1)
  (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (unless (string-match (format-time-string "*%Y年%-m月") string)
      (forward-line -1)
      (insert (format-time-string "*%Y年%-m月\n"))))
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

;;;###autoload
(defun my-dselext-new-post ()
  "Open d_selext file and insert template."
  (interactive)
  (find-file (expand-file-name "select.txt" "~/Dropbox/GH/d_selext/"))
  (goto-char (point-min))
  (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
    (if (unless (string-match (format-time-string "%Y%m:") string)
          (forward-line -1)
          (insert (format-time-string "%Y%m:\n"))
          (insert (format-time-string "*%Y年%-m月%-d日\n")))
        (forward-line 1)
      (let ((string (buffer-substring-no-properties (pos-bol) (pos-eol))))
        (unless (string-match (format-time-string "*%Y年%-m月%-d日") string)
          (forward-line 1)
          (insert (format-time-string "*%Y年%-m月%-d日\n"))))))
  (goto-char (point-min))
  (forward-line 2)
  (insert
   ";-----------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n"
   ";-----------------------------------------------------------\n"
   "=<\n"
   (format-time-string "-*\n")
   (format-time-string "--*\n")
   "<-tframe>\n"
   "-(\n\n-)\n"
   "</div></div>\n"
   "=>\n"
   ";-----------------------------------------------------------\n"
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

;;;###autoload
(defun my-year-new-post ()
  "Insert year entry template at current position."
  (interactive)
  (insert
   ";----------------------------------------------------------\n"
   "--\n"
   (format-time-string "*さん\n")
   "--\n"
   "<div class=\"box\">\n"
   "<div class=\"vertical\">\n"
   "--\n"
   "参加年 = <br>\n"
   "性　別 = <br>\n"
   "都道府県 = \n"
   "--\n"
   "&ensp;\n"
   "--\n"
   "<insert p tag>\n"
   "--\n"
   "</div></div>\n"
   "--\n"
   (format-time-string "-*さんのコメント\n")
   "--\n"
   "-((\n"
   "<insert comment>\n"
   "-))\n"
   "--\n"))

;;;###autoload
(defun my-haiku-note ()
  "Open haiku note file."
  (interactive)
  (find-file (format-time-string "~/Dropbox/howm/haiku/haikunote.%Y.txt"))
  (when evil-mode
    (evil-emacs-state))
  (goto-char (point-min)))

;;;###autoload
(defun my-haiku-note-post ()
  "Open haiku note file and insert template."
  (interactive)
  (find-file (format-time-string "~/Dropbox/howm/haiku/haikunote.%Y.txt"))
  (when evil-mode
    (evil-emacs-state))
  (goto-char (point-min))
  (forward-line -2)
  (insert
   (format-time-string "> %Y年%-m月%-d日 (%a)\n")
   (format-time-string "PLACE:\n\n"))
  (forward-line -2)
  (forward-char 6))

(provide 'my-template)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; my-template.el ends here
