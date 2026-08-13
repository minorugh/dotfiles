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
  :doc "Run commands quickly.  Bound to F5 via `my-quickrun'; see 07-functions.el."
  :config
  (defun my-quickrun ()
    "In Dired, run the executable file at point in an external terminal,
keeping the terminal open afterward.  Otherwise (outside Dired), run
`quickrun' on the current buffer."
    (interactive)
    (if (derived-mode-p 'dired-mode)
        (let* ((file (dired-get-file-for-visit))
               (default-directory (dired-current-directory)))
          (my-launch-gnome-terminal
           "--" "bash" "-c"
           (format "%s; echo; read -n1 -r -p '[Enter/任意キーで閉じる]'"
                   (shell-quote-argument file))))
      (quickrun))))


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
  :bind (("C-x l" . my-open-lepton))
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
;;  Changelog Search
;; ============================================================

(leaf *changelog-search
  :tag "local"
  :doc "Search ~/Dropbox/CHANGELOG interactively via search.pl (migemo対応)."
  :preface
  (defvar my-changelog-dir "~/Dropbox/CHANGELOG/"
    "Changelog-*.md を管理しているディレクトリ.")

  (defvar my-changelog-search-script
    (expand-file-name "search.pl" my-changelog-dir)
    "Changelog 全文検索用 Perl スクリプトのパス.")

  (defvar my-changelog-search-history nil
    "`my-changelog-search' の検索履歴。")

  (defun my-changelog-search (query &optional case-sensitive)
    "QUERY で ~/Dropbox/CHANGELOG 以下を全文検索してgrep-mode バッファに表示する,
プレフィクス引数 (C-u) を付けると大文字小文字を区別する検索になる。
カーソル位置の単語をデフォルト値として提案する。
search.pl 側で migemo によるローマ字検索に対応済み。"
    (interactive
     (list (read-string
            (format "Search changelog%s: "
                    (if current-prefix-arg " (case-sensitive)" ""))
            nil 'my-changelog-search-history
            (thing-at-point 'word t))
           current-prefix-arg))
    (unless (file-exists-p my-changelog-search-script)
      (user-error "Search.pl が見つかりません: %s" my-changelog-search-script))
    (let* ((default-directory (expand-file-name my-changelog-dir))
           (flag (if case-sensitive "-c" "-i"))
           (bufname (format "*changelog-search: %s*" query))
           (cmd (format "perl %s %s %s"
                        (shell-quote-argument my-changelog-search-script)
                        flag
                        (shell-quote-argument query))))
      (compilation-start cmd 'grep-mode (lambda (_mode) bufname))
      (when-let ((win (get-buffer-window bufname t)))
        (select-window win)))))


;; ============================================================
;;  PostScript Printing
;; my-ps-print: PostScript printing with Japanese support.
;; ============================================================

(when (executable-find "lpr")
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
  (setq ps-end-with-control-d t)
  (defalias 'ps-mule-header-string-charsets 'ignore))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 30-utils.el ends here
