;;; 50_yatex.el --- YaTex configuration.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf yatex :ensure t
  :doc "Yet Another tex-mode for emacs"
  :url "https://github.com/emacsmirror/yatex"
  :mode ("\\.tex\\'" "\\.sty\\'" "\\.cls\\'")
  :config
  (setq tex-command              "platex")
  (setq dviprint-command-format  "dvpd.sh %s")
  (setq YaTeX-kanji-code         nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-default-pop-window-height 15)
  (leaf yatexprc
    :bind ((("M-c" . YaTeX-typeset-buffer)
	    ("M-v" . YaTeX-lpr)))))


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
;;; 60_yatex.el ends here
