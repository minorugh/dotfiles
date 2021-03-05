;;; 70_yatex.el --- Yatex configurations. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf yatex
  :ensure t
  :mode ("\\.tex\\'" . yatex-mode)
  :config
  (setq tex-command "platex")
  (setq dviprint-command-format "dvpd.sh %s")
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-default-pop-window-height 15)
  (leaf yatexprc
	:bind (("M-c" . YaTeX-typeset-buffer)
		   ("M-l" . YaTeX-lpr))))

;; Dviprint-command-format (YaTeX-lpr)
;; -----------------------------------------------------------------------
;; dvpd.sh for Linux
;; Create dvpd.sh and execute 'chmod +x', and place it in `/usr/local/bin'
;;
;; | #!/bin/bash
;; | name=$1
;; | dvipdfmx $1 && evince ${name%.*}.pdf
;; | # Delete unnecessary files
;; | rm *.aux *.dvi *.log


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 70_yatex.el ends here
