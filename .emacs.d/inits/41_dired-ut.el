;;; 41_dired-ut.el --- Dired util configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf dired-ut
  :config
  (defun dired-open-file ()
    "In dired, open the file in associated application."
    (interactive)
    (let* ((file (dired-get-filename nil t)))
      (call-process "xdg-open" nil 0 nil file)))

  (defun call-sxiv ()
    "Show all images in the directory with sxiv.
see https://gist.github.com/kobapan/28908b564b610bd3e6f3fae78637ac8b"
    (interactive)
    (let ((image-files
	   (delq nil
		 (mapcar
		  (lambda (f)
		    (when (string-match "\.\\(jpe?g\\|png\\|gif\\|bmp\\)$" f)
		      f))
		  (directory-files default-directory)))))
      (start-process-shell-command
       "sxiv" nil
       (format "sxiv -f -t -n %s %s"
	       (length image-files)
	       (mapconcat 'identity image-files " ")))))

  (defun gitk-open ()
    "Open gitk with current dir.
see https://riptutorial.com/git/example/18336/gitk-and-git-gui"
    (interactive)
    (compile "gitk")
    (delete-other-windows))

  ;; -------------------------------------------------
  ;; 書き込み権限のないファイル/ディレクトリを開くときに、自動的にsudoで開く
  ;; sudoで開き直すときは、M+x sudo
  ;; -------------------------------------------------
  ;; (defun find-file--sudo (orig-fun &optional filename &rest r)
  ;;   (if (and (not (file-writable-p filename)) ; 書き込み権限がなかったら
  ;;            (y-or-n-p (concat filename " is read-only. Open it as root? "))) ; y だったら
  ;; 	(sudo filename) ; /sudo:: で開く
  ;;     (apply orig-fun `(,filename)) )) ; その他通常のfind-fileで開く
  ;; (advice-add 'find-file :around #'find-file--sudo)

  ;; (defun sudo (&optional file)
  ;;   "Open read-only FILE with sudo."
  ;;   (interactive)
  ;;   (if file ; find-fileから呼ばれたら
  ;; 	(find-file (concat "/sudo::" file)) ; /sudo:: で開く
  ;;     (let ((pos (point)))
  ;; 	(find-alternate-file ; /sudo:: で開き直す
  ;; 	 (concat "/sudo::" (or (buffer-file-name) list-buffers-directory)))
  ;; 	(goto-char pos))) ; カーソル位置復元
  ;;   (rename-buffer (concat "sudo:" (buffer-name))))
					; バッファ名の先頭にsudo:を付ける
  )


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 41_dired-ut.el ends here
