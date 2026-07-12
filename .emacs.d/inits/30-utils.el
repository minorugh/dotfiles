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
;;  Key chord
;; ============================================================

(leaf key-chord
  :ensure t
  :hook (after-init-hook . key-chord-mode)
  :config
  (key-chord-define-global "l;" 'init-loader-show-log))

;; --------------------------------------------------------------
;; key-chord は key-chord-mode の有効化時に input-method-function を
;; 自前の関数 (key-chord-input-method) に書き換える仕組みで動作している。
;; ところが日本語入力メソッド（mozc / skk / Emacs標準の input-method 等）を
;; 有効/無効にすると、それらも同じ input-method-function を奪い合うため、
;; 「最後に取得した側が勝つ」形になり、IMEをオフにした後に
;; key-chord が反応しなくなる（stall する）ことがある。
;; https://github.com/emacsorphanage/key-chord#caveats 参照
;;
;; 対策として、入力メソッドの有効化・無効化のたびに key-chord-mode を
;; 一旦オフ→オンし直し、input-method-function を key-chord 側に
;; 強制的に取り戻す。
;; --------------------------------------------------------------
(defun my-key-chord-ensure ()
  "Key-chord stall recovery."
  (when (and key-chord-mode
             (not (eq input-method-function 'key-chord-input-method)))
    (key-chord-mode -1)
    (key-chord-mode 1)))
(add-hook 'input-method-activate-hook   #'my-key-chord-ensure)
(add-hook 'input-method-deactivate-hook #'my-key-chord-ensure)


;; ============================================================
;;  Quickrun
;; ============================================================

(leaf quickrun
  :ensure t
  :doc "Run commands quickly.  Bound to F5; see 07-funcs.el.")


;; ============================================================
;;  Browse at remote
;; ============================================================

(leaf browse-at-remote
  :ensure t
  :doc "Open page client on GitHub from Emacs buffer")


;; ============================================================
;;  Tempbuf
;;  無シャットダウン運用でバッファが溜まり続けるため必須。
;; ============================================================

(leaf tempbuf
  :doc "実体: ~/.emacs.d/elisp/tempbuf.el"
  :commands (tempbuf-mode turn-on-tempbuf-mode)
  :hook ((find-file-hook . turn-on-tempbuf-mode)
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

(defun my-open-lepton ()
  "Specify the full path, disable the sandbox if necessary, and start Lepton."
  (interactive)
  (start-process-shell-command
   "lepton" nil
   "~/Apps/Lepton-1.10.0.AppImage --no-sandbox"))

(keymap-global-set "C-x g" #'gist-region-or-buffer)
(keymap-global-set "C-x l" #'my-open-lepton)


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
