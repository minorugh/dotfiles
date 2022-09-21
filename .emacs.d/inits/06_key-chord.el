;;; 06_key-chord.el --- Key chord configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :chord (("df" . counsel-descbinds)
		  ("l;" . init-loader-show-log)
		  ("@@" . howm-list-all)
		  ("jk" . open-junk-file))
  :custom
  `((key-chord-two-keys-delay . 0.15)
	(key-chord-safety-interval-backward . 0.1)
	(key-chord-safety-interval-forward  . 0.25)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 06_key-chord.el ends here
