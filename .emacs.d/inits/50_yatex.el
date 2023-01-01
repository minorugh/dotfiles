;;; 50_yatex.el --- YaTex configuration. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf yatex
  :ensure t
  :mode ("\\.tex\\'" "\\.sty\\'" "\\.cls\\'")
  :hook (yatex-mode-hook . (lambda ()(interactive)(view-mode -1)))
  :custom
  `((tex-command . "platex")
	(dviprint-command-format . "dvpd.sh %s")
	(YaTeX-kanji-code . nil)
	(YaTeX-latex-message-code . 'utf-8)
	(YaTeX-default-pop-window-height . 15)))
(leaf yatexprc
  :after yatex
  :bind (("M-c" . YaTeX-typeset-buffer)
		 ("M-v" . YaTeX-lpr)))


;;-----------------------------
;; dvpd.sh for Linux
;;-----------------------------
;; #!/bin/zsh
;;
;; # 生成されたPDFをevinceで開く
;; name=$1
;; dvipdfmx {name%.*} && evince ${name%.*}.pdf
;;
;; # 不要ファイルを削除
;; rm *.au*
;; rm *.dv*
;; rm *.lo*

;;---------------------------
;; dvpd.sh for mac
;;---------------------------
;; #!/bin/zsh
;;
;; # 生成されたPDFをPreview.appで開く
;; name=$1
;; dvipdfmx {name%.*} && open -a Preview.app ${name%.*}.pdf
;;
;; # 不要ファイルを削除
;; rm *.au*
;; rm *.dv*
;; rm *.lo*


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_yatex.el ends here
