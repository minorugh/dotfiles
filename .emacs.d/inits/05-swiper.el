;;; 05-swiper.el --- Swiper configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf swiper :ensure t
  :defun --map s-join s-matches-p s-equals? --partition-by s-matches? s-split migemo-get-pattern
  :doc "Isearch with an overview"
  :bind (("C-s" . swiper-region)
	 ("s-s" . swiper-thing-at-point))
  :config
  (defun swiper-region ()
    "If region is selected, `swiper-thing-at-point'.
If the region isn't selected, `swiper'."
    (interactive)
    (if (not (use-region-p))
	(swiper)
      (swiper-thing-at-point))))


(leaf migemo :ensure t
  :doc "Japanese incremental search through dynamic pattern expansion"
  :if (executable-find "cmigemo")
  :hook (after-init-hook . migemo-init)
  :config
  (setq migemo-command    "cmigemo")
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict"))


(leaf swiper-migemo
  :doc "For swiper-migemo"
  :url "https://www.yewton.net/2020/04/21/migemo-ivy/"
  :config
  (defun my:ivy-migemo-re-builder (str)
    "Own function for my:ivy-migemo."
    (let* ((sep " \\|\\^\\|\\.\\|\\*")
	   (splitted (--map (s-join "" it)
			    (--partition-by (s-matches-p " \\|\\^\\|\\.\\|\\*" it)
					    (s-split "" str t)))))
      (s-join "" (--map (cond ((s-equals? it " ") ".*?")
			      ((s-matches? sep it) it)
			      (t (migemo-get-pattern it)))
			splitted))))

  (setq ivy-re-builders-alist '((t . ivy--regex-plus)
				(swiper . my:ivy-migemo-re-builder))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 05-swiper.el ends here
