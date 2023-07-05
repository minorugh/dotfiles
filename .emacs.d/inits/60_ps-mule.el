;;; 60_ps-mule.el --- Postscript utility command cofiguration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *cus-ps-print
  :doc "Print from Emacs via Postscript"
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :config
  (setq ps-multibyte-buffer 'non-latin-printer
		ps-paper-type       'a4
		ps-printer-name     nil

		;; Header/Footer setup
		ps-print-header      nil   ;;buffer name, page number, etc.
		ps-print-footer      nil ;;page number

		;; font
		ps-font-size         9
		;; ps-header-font-family 'Helvetica    ;;default
		;; ps-line-number-font  "Times-Italic" ;;default
		ps-font-family      'Courier
		ps-line-number-font 'Courier

		;; line-number
		ps-line-number       t
		ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 60_ps-mule.el ends here
