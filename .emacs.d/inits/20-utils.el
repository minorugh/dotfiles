;;; 20-utils.el --- Initialize utilities.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;; ============================================================
;;  Keybinding Utilities
;; ============================================================

(leaf which-key
  :tag "builtin"
  :doc "Display available keybindings in popup."
  :hook (after-init-hook . which-key-mode)
  :config
  (setq which-key-max-description-length 40)
  (setq which-key-idle-delay 0.0))

(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :config
  (key-chord-define-global "l;" 'init-loader-show-log)
  ;; key-chord stall recovery
  (defun my-key-chord-ensure ()
    (when (and key-chord-mode
               (not (eq input-method-function 'key-chord-input-method)))
      (key-chord-mode -1)
      (key-chord-mode 1)))

  (add-hook 'input-method-activate-hook   #'my-key-chord-ensure)
  (add-hook 'input-method-deactivate-hook #'my-key-chord-ensure))

(leaf quickrun
  :ensure t
  :doc "Run commands quickly.  Bound to F5; see 07-funcs.el.")


;; ============================================================
;;  Tempbuf
;;  無シャットダウン運用でバッファが溜まり続けるため必須。
;; ============================================================

(leaf tempbuf
  :doc "Auto kill unused buffers in the background"
  :vc (:url "https://github.com/minorugh/tempbuf")
  :hook ((find-file-hook  . turn-on-tempbuf-mode)
         (dired-mode-hook . turn-on-tempbuf-mode))
  :config
  (setq tempbuf-kill-message nil))


;; ============================================================
;;  Package Management
;; ============================================================

(leaf my-elpa
  :doc "Browse ELPA snapshots and manage packages via hydra."
  :config
  (key-chord-define-global "p@" 'hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
Package: _l_og  _i_nstall  _d_elete  _u_pgrade  up-_a_ll  _v_c-up-all
  "
   ("l" my-open-elpa-log)
   ("i" package-install)
   ("u" package-upgrade)
   ("d" package-delete)
   ("a" package-upgrade-all)
   ("v" package-vc-upgrade-all)
   ("<muhenkan>" nil))
  :init
  (defun my-open-elpa-log ()
    "Open elpa-changes.log."
    (interactive)
    (find-file "~/Dropbox/backup/elpa/LOG/elpa-changes.log")))


;; ============================================================
;;  Gist / Lepton Integration
;; ============================================================

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

(defun open-lepton ()
  "Specify the full path, disable the sandbox if necessary, and start Lepton."
  (interactive)
  (start-process-shell-command
   "lepton" nil
   "~/Apps/Lepton-1.10.0.AppImage --no-sandbox"))


;; ============================================================
;;  YaTeX --- Japanese LaTeX Environment
;; ============================================================

(leaf yatex
  :ensure t
  :doc "Yet Another tex-mode for emacs."
  :url "https://github.com/emacsmirror/yatex"
  :mode ("\\.tex\\'" "\\.sty\\'" "\\.cls\\'")
  :config
  (setq tex-command              "platex")
  (setq dviprint-command-format  "dvpd.sh %s")
  (setq YaTeX-kanji-code         nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-default-pop-window-height 15))

(leaf yatexprc
  :ensure nil
  :doc "YaTeX process handler"
  :after yatex
  :bind (("M-c" . YaTeX-typeset-buffer)
	 ("M-v" . YaTeX-lpr)))


;; ============================================================
;;  PostScript Printing
;; ============================================================

(leaf ps-print
  :doc "PostScript printing with Japanese support."
  :url "https://tam5917.hatenablog.com/entry/20120914/1347600433"
  :if (executable-find "lpr")
  :config
  (setq ps-multibyte-buffer 'non-latin-printer)
  (setq ps-paper-type       'a4)
  (setq ps-printer-name      nil)
  (setq ps-print-header      nil)
  (setq ps-print-footer      nil)
  (setq ps-font-size         9)
  (setq ps-font-family      'Courier)
  (setq ps-line-number-font 'Courier)
  (setq ps-line-number       t)
  (setq ps-show-n-of-n       t)
  (defalias 'ps-mule-header-string-charsets 'ignore)
  (setq ps-end-with-control-d t))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 20-utils.el ends here
