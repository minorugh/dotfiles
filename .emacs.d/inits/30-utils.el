;;; 30-utils.el --- Initialize utilities.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  WhichKey
;; ============================================================

(leaf which-key
  :tag "builtin"
  :doc "Display available keybindings in popup."
  :hook (after-init-hook . which-key-mode)
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-idle-delay 0.0))


;; ============================================================
;;  Quickrun
;; ============================================================

(leaf quickrun
  :ensure t
  :doc "Run commands quickly.  Bound to F5; see 07-funcsions.el.")


;; ============================================================
;;  Browse at remote
;; ============================================================

(leaf browse-at-remote
  :ensure t
  :doc "Open page client on GitHub from Emacs buffer")


;; ============================================================
;;  Tempbuf
;; ============================================================

(leaf tempbuf
  :tag "local"
  :doc "Kill unused buffers in the background."
  :preface
  (autoload 'turn-on-tempbuf-mode "tempbuf" nil t)
  :commands (tempbuf-mode turn-on-tempbuf-mode)
  :hook ((find-file-hook . turn-on-tempbuf-mode)
         (dired-mode-hook . turn-on-tempbuf-mode))
  :config
  (setq tempbuf-kill-message nil))


;; ============================================================
;;  Package Management
;; ============================================================

(leaf *package
  :tag "local"
  :doc "Browse ELPA snapshots and manage packages via hydra."
  :preface
  (defun package-log-open ()
    "Open elpa-changes.log."
    (interactive)
    (find-file "~/Dropbox/backup/elpa/LOG/elpa-changes.log"))
  :config
  (key-chord-define-global "p@" 'hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
Package: _l_og  _i_nstall  _d_elete  _u_pgrade  up-_a_ll  _v_c-up-all
  "
   ("l" package-log-open)
   ("i" package-install)
   ("u" package-upgrade)
   ("d" package-delete)
   ("a" package-upgrade-all)
   ("v" package-vc-upgrade-all)
   ("<muhenkan>" nil)))


;; ============================================================
;;  Gist / Lepton Integration
;; ============================================================

(leaf *my-gist-command
  :tag "local"
  :bind (("C-x g" . gist-region-or-buffer)
         ("C-x l" . my-open-lepton))
  :preface
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
    (delete-other-windows))

  (defun my-open-lepton ()
    "Specify the full path, disable the sandbox if necessary, and start Lepton."
    (interactive)
    (start-process-shell-command
     "lepton" nil
     "~/Apps/Lepton-1.10.0.AppImage --no-sandbox")))


;; ============================================================
;;  PostScript Printing
;; ============================================================

;; my-ps-print: PostScript printing with Japanese support.
(when (executable-find "lpr")
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type       'a4)
  (setq ps-printer-name      nil)
  (setq ps-print-header      nil)
  (setq ps-print-footer      nil)
  (setq ps-font-size         10)
  (setq ps-font-family      'Courier)
  (setq ps-line-number-font 'Courier)
  (setq ps-line-number       t)
  (setq ps-show-n-of-n       t)
  (setq ps-end-with-control-d t)
  (defalias 'ps-mule-header-string-charsets 'ignore))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 30-utils.el ends here
