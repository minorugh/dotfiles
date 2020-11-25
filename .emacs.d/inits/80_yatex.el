;;; 80_yatex.el --- yet another tex-mode for emacs  -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
;; (setq debug-on-error t)

(leaf yatex
  :ensure t
  :mode ("\\.tex\\'" . yatex-mode)
  :init
  (setq tex-command "platex")
  (setq dviprint-command-format "dvpd.sh %s")
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-default-pop-window-height 15)
  :config
  (when (require 'yatexprc nil t)
	(bind-key "M-c" 'YaTeX-typeset-buffer)
	(bind-key "M-l" 'YaTeX-lpr)))


;; Dviprint-command-format (YaTeX-lpr)
;; -----------------------------------------------------------------------
;; dvpd.sh for Linux
;; Create dvpd.sh and execute 'chmod +x', and place it in `/usr/local/bin'
;;
;; for Mac
;; | #!/bin/bash
;; | name=$1
;; | dvipdfmx $1 && open -a preview.app ${name%.*}.pdf
;; | # Delete unnecessary files
;; | rm *.aux *.dvi *.log
;;
;; for Linux
;; | #!/bin/bash
;; | name=$1
;; | dvipdfmx $1 && evince ${name%.*}.pdf
;; | # Delete unnecessary files
;; | rm *.aux *.dvi *.log
;;
;; for WSL
;; | #!/bin/bash
;; | name=$1
;; | dvipdfmx $1 && wslstart ${name%.*}.pdf
;; | # Delete unnecessary files
;; | rm *.aux *.dvi *.log
;;
;; ------------------------------------------------------------------------


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 80_yatex.el ends here
