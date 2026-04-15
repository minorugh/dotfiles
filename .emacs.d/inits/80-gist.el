;;; 80-gist.el --- Gist configulations.  -*- lexical-binding: t -*-
;;; Commentary:

;; Submit code to Gist with the gist command
;;
;; install
;;   $ sudo apt-get install ruby
;;   $ sudo apt-get install gem
;;   $ sudo gem install gist
;; help
;;   $ gist -h
;;   $ gist --help
;; login
;;   $ gist --login

;;; Code:
;; (setq debug-on-error t)

(leaf my-gist
  :doc "Submit code to Gist with the gist command"
  :bind (("C-c g g" . gist-region-or-buffer)
         ("C-c g w" . open-gist-web-page)
         ("C-c g l" . open-lepton))
  :init
  (defun open-gist-web-page ()
    "Open GitHub Gist page with chrome."
    (interactive)
    (browse-url "https://gist.github.com/minorugh"))

  (defun open-lepton ()
    "Specify the full path, disable the sandbox if necessary, and start Lepton."
    (interactive)
    (start-process-shell-command
     "lepton" nil
     "~/Apps/Lepton-1.10.0.AppImage --no-sandbox")))

(eval-and-compile
  (leaf my-gist-configurations
    :doc "Post region or buffer to gist via compile."
    :config
    (defun gist-description ()
      "Add gist description."
      (shell-quote-argument (read-from-minibuffer "Add gist description: ")))

    (defun gist-filename ()
      "The character string entered in minibuffer is used as file-name.
If enter is pressed without file-name, that's will be buffer file name."
      (interactive)
      (let ((file (file-name-nondirectory (buffer-file-name (current-buffer)))))
	(read-from-minibuffer (format "File name (%s): " file) file)))

    (defun gist-region-or-buffer ()
      "If region is selected, post from the region.
If region isn't selected, post from the buffer."
      (interactive)
      (let ((file (buffer-file-name)))
	(if (not (use-region-p))
            (compile (concat "gist -od " (gist-description) " " file))
          (compile (concat "gist -oPd " (gist-description) " -f " (gist-filename)))))
      (delete-other-windows))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 80-gist.el ends here
