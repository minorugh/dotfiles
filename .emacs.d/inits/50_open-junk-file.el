;;; 50_open-junk-file.el --- Open junk file configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf open-junk-file
  :ensure t
  :custom
  `((open-junk-file-format . "~/Dropbox/howm/junk/%Y%m%d.")
	(open-junk-file-find-file-function . 'find-file)))

;; Open last created disposable file in one shot
;; https://qiita.com/zonkyy/items/eba6bc64f66d278f0032
(leaf em-glob
  :require t
  :after open-junk-file
  :config
  (defvar junk-file-dir "~/Dropbox/howm/junk/")
  (defun open-last-junk-file ()
	"Open last created junk-file."
	(interactive)
	(find-file
	 (car
	  (last (eshell-extended-glob
			 (concat
			  (file-name-as-directory junk-file-dir)
			  "*.*.*")))))))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 50_open-junk-file.el ends here
