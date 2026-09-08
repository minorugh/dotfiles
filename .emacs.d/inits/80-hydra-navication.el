;;; 80-hydra-navication.el --- Hydra navigation and work menus. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ============================================================
;;  Hydra Dired  (ファイルナビゲーションランチャー)
;; ============================================================
(defun my-hydra-dired ()
  "Disable mozc if active, leave `evil-emacs-state', then open hydra-dired."
  (interactive)
  (when current-input-method
    (toggle-input-method))
  (unless (evil-normal-state-p)
    (evil-normal-state))
  (hydra-dired/body))

(with-eval-after-load 'mozc
  (keymap-set mozc-mode-map "<henkan>" #'my-hydra-dired))


(leaf *hydra-dired
  :after evil
  :bind (("<henkan>" . my-hydra-dired))
  :init
  (defvar my-cron-hint
    (concat (propertize "^" 'face 'hydra-face-blue) "cron"))
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
 Quick.dired
  _d_ropbox  _e_macs.d^^^^^^  _i_nits^^  _s_rc  root_/_  _._files^  make._c_._b_._k_._m_._u_  fz_8_._9_._0_  _p_assxc  _x_env^^  S_n_ote  _v_ersion
  _r_estart  _g_it:_[__-__]_  GH._h__j_  _t_ig  ch_l_og  _<home>_^  h_o_wm_,_  md.vie_w_^^^^  _@_remote^^^^  _f_lyerr  2p_;__:_  %s`my-cron-hint
"
   ("x" my-env-recover)
   ("^" my-make-launch-cron)
   ("@" browse-at-remote)
   ("t" open-tig)
   ("f" flymake-show-buffer-diagnostics)
   ("8" (filezilla "s"))
   ("9" (filezilla "g"))
   ("0" (filezilla "m"))
   ("p" keepassxc)
   ("g" my-git-discard-changes)
   ("n" (browse-url "https://app.simplenote.com/"))
   ("<home>" (my-open "~/" :omit))
   (":" my-open-xsrv-2pane-gh)
   (";" my-open-xsrv-2pane-minorugh)
   ("." (my-open "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("s" (my-open "~/src/"))
   ("/" (my-open "/" :omit))
   ("c" (my-make "clean"))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" my-restart-emacs)
   ("w" markdown-preview)
   ("v" emacs-version-check)
   ("V" debian-version-check)
   ("o" howm-list-all)
   ("," my-howm-create-with-category)
   ("L" (my-open "~/Dropbox/CHANGELOG"))
   ("l" my-changelog-search)
   ("[" my-git-peek-smart)
   ("-" git-peek-deleted)
   ("]" my-make-git)
   ("_" delete-other-windows)
   ("q" top-level)
   ("<henkan>"  hydra-work/body)
   ("<muhenkan>" nil))
  :preface
  ;; ------------------------------------------------------------
  ;;  File / Directory Helpers
  ;; ------------------------------------------------------------
  (defun my-make (target &optional dir)
    "Run make TARGET in DIR (default: current directory)."
    (interactive "sTarget: ")
    (let ((default-directory (expand-file-name (or dir default-directory))))
      (compile (concat "make " target))))

  (defun my-make-git ()
    "Run the main using `compile`, run everything else in the terminal."
    (interactive)
    (if my-main-machine-p
        (my-make "git")
      (my-make-run-async default-directory "git")))

  ;; ------------------------------------------------------------
  ;;  Git Helpers (discard changes)
  ;; ------------------------------------------------------------
  (defun my-git--root ()
    "Return the git root directory for the current buffer, or error."
    (or (locate-dominating-file default-directory ".git")
        (user-error "Gitリポジトリが見つかりません")))

  (defun my-git--colorize-diff-line ()
    "行頭の記号に応じて前景色だけのfaceを付ける."
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (face (pcase (char-after bol)
                   (?@ 'diff-hunk-header)
                   (?+ (if (looking-at-p "\\+\\+\\+") 'diff-file-header 'diff-added))
                   (?- (if (looking-at-p "---") 'diff-file-header 'diff-removed)))))
      (when face
        (put-text-property bol eol 'face (list :foreground (face-foreground face nil t))))))

  (defun my-git--colorize-diff-buffer ()
    "バッファ内の各行に `my-git--colorize-diff-line' を適用する."
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (my-git--colorize-diff-line)
        (forward-line 1))))

  (defun my-git-discard-changes ()
    "このファイルのdiffを表示し、確認の上で直近のコミット内容に戻す."
    (interactive)
    (let* ((orig-buf (current-buffer))
           (file buffer-file-name)
           (default-directory (my-git--root))
           (diff-output (shell-command-to-string
                         (format "git --no-pager diff -- %s"
                                 (shell-quote-argument file)))))
      (if (string-empty-p (string-trim diff-output))
          (message "変更なし")
        (let ((buf (get-buffer-create "*git-diff-preview*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert diff-output)
              (fundamental-mode)
              (my-git--colorize-diff-buffer)
              (setq buffer-read-only t)
              (goto-char (point-min))))
          (display-buffer buf)
          (if (y-or-n-p "Discardしますか? ")
              (progn
                (shell-command (format "git checkout -- %s"
                                       (shell-quote-argument file)))
                (with-current-buffer orig-buf (revert-buffer t t t))
                (message "discardしました"))
            (message "discardをキャンセルしました"))
          (let ((win (get-buffer-window buf)))
            (when win (delete-window win)))
          (kill-buffer buf)))))


  ;; ------------------------------------------------------------
  ;;  Open Path Helper
  ;; ------------------------------------------------------------
  ;; OPTIONS for my open path function
  ;; :pos 'top | 'bottom | integer  :omit  :emacs
  ;; :pos -10  → point-max then back 10 lines
  ;; :pos  1   → point-min then forward 1 line
  (defun my-open (path &rest opts)
    "Open PATH in dired or find-file."
    (find-file (expand-file-name (format-time-string path)))
    (pcase (plist-get opts :pos)
      ('top    (goto-char (point-min)))
      ('bottom (goto-char (point-max)))
      ((pred integerp)
       (let ((n (plist-get opts :pos)))
         (goto-char (if (< n 0) (point-max) (point-min)))
         (forward-line n))))
    (when (memq :omit  opts) (dired-omit-mode 0))
    (when (memq :emacs opts) (evil-emacs-state)))


  ;; ------------------------------------------------------------
  ;;  External Tools / System
  ;; ------------------------------------------------------------
  (defun my-restart-emacs ()
    "Save buffers, stop server, kill this Emacs, then launch emacs-start.sh."
    (interactive)
    (save-some-buffers t)
    (server-mode -1)
    (call-process "bash" nil nil nil "-c"
                  "nohup bash -c 'emacs-start.sh' &>/dev/null &")
    (kill-emacs))
  ;; # emacs-start.sh
  ;; # keychain の SSH agent 環境変数を明示的に読み込むことで再起動後も引き継がれる
  ;; [ -f "$HOME/.keychain/$(hostname)-sh" ] && source "$HOME/.keychain/$(hostname)-sh"
  ;; exec zsh -lc "/usr/local/bin/emacs --maximized"

  (defun my-make-launch-cron ()
    "Open the Makefile in `dotfiles/cron` to immediately launch the target picker."
    (interactive)
    (find-file (expand-file-name "~/src/github.com/minorugh/dotfiles/cron/Makefile"))
    (my-make-ivy-integrated))

  (defun my-env-recover ()
    "Reload xmodmap, re-import SSH_AUTH_SOCK from keychain file."
    (interactive)
    (shell-command "xmodmap ~/.Xmodmap > /dev/null 2>&1")
    (let ((keychain-file (expand-file-name
                          (concat "~/.keychain/" (system-name) "-sh"))))
      (when (file-exists-p keychain-file)
        (with-temp-buffer
          (insert-file-contents keychain-file)
          (goto-char (point-min))
          (while (re-search-forward "^\\([^=]+\\)=\\([^;]+\\);" nil t)
            (setenv (match-string 1)
                    (match-string 2))))))
    (message "ENV RECOVERED: xmodmap + SSH_AUTH_SOCK"))

  (defun emacs-version-check ()
    "GNU Emacsの最新安定版をミニバッファに表示する。"
    (interactive)
    (let ((latest (string-trim
                   (shell-command-to-string
                    "curl -sL https://ftp.gnu.org/gnu/emacs/ | grep -oE 'emacs-[0-9]+\\.[0-9]+(\\.[0-9]+)?\\.tar\\.gz' | sort -V | tail -1"))))
      (if (string-empty-p latest)
          (message "Emacs最新版の情報が取得できませんでした。")
        (message "最新の安定版は %s です。" (string-remove-suffix ".tar.gz" latest)))))

  (defun debian-version-check ()
    "保存済みのDebian netinstall isoが最新版かどうかをミニバッファに表示する。"
    (interactive)
    (message "%s"
             (with-temp-buffer
               (insert (shell-command-to-string
			"make -s -C ~/Dropbox/RESTORE/make-install-usb version-check"))
               (or (my-make--marker-message (current-buffer))
                   (string-trim (buffer-string))))))

  (defun keepassxc ()
    "Open KeePassXC via keepass.sh, detached from Emacs."
    (interactive)
    (call-process "setsid" nil 0 nil "keepassxc.sh"))

  (defun filezilla (&optional site)
    "Open FileZilla with SITE, detached from Emacs.
SITE: \"g\" = gospel-haiku.com, \"m\" = minorugh.com, \"s\" = site manager."
    (interactive)
    (let* ((sites '(("g" . "0/gospel-haiku.com")
                    ("m" . "0/minorugh.com")
                    ("s" . "-s")))
           (arg  (or (cdr (assoc site sites)) "-s"))
           (args (if (string= arg "-s")
                     '("-s")
                   (list (format "--site=%s" arg)))))
      (apply #'call-process "setsid" nil 0 nil "filezilla" args))))


;; ============================================================
;;  Hydra Work  (俳句作業メニュー)
;; ============================================================

(defun my-hydra-work ()
  "Disable mozc if active, leave `evil-emacs-state', then open hydra-work."
  (interactive)
  (when current-input-method
    (toggle-input-method))
  (unless (evil-normal-state-p)
    (evil-normal-state))
  (hydra-work/body))

(with-eval-after-load 'mozc
  (keymap-set mozc-mode-map "<f14>" #'my-hydra-work))

(leaf *hydra-work
  :after evil
  :bind ("<f14>" . my-hydra-work)
  :hydra
  (hydra-work
   (:hint nil :exit t :body-pre (require 'insert-template))
   "
 Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _M_:月例^^  _p_rint.buf  yas._n_._v_._i_  _c_aption.._u_p.d_o_wn
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _Y_:年度^^  _g_ist._l_ept  _e_asy-hugo^^  _j_unk._h_owm._+_scale
"
   ("+" text-scale-adjust)
   ("c" my-capitalize-word)
   ("u" my-upcase-word)
   ("o" my-downcase-word)
   ("n" yas-new-snippet)
   ("v" yas-visit-snippet-file)
   ("i" yas-insert-snippet)
   ("p" ps-print-buffer)
   ("P" ps-print-region)
   ("y" (my-open "~/Dropbox/GH/year/%Y.txt" :pos -10))
   (":" (my-open "~/Dropbox/GH/year/draft.dat" :pos 1))
   ("Y" my-year-new-post)
   ("M" (my-open "~/Dropbox/GH/m_select/tex/mkukai.txt" :pos 'top))
   ("a" (my-open "~/Dropbox/GH/apvoice/apvoice.txt" :pos 'top :emacs))
   ("A" my-apvoice-new-post)
   ("K" (my-open "~/Dropbox/GH/w_kukai/info/kendai.csv" :pos 'top :emacs))
   ("e" easy-hugo)
   ("j" (my-open "~/Dropbox/howm/junk/"))
   ("h" (my-open "~/Dropbox/howm/"))
   ("d" (my-open "~/Dropbox/GH/dia/diary.txt" :pos 'top))
   ("D" my-diary-new-post)
   ("g" gist-region-or-buffer)
   ("G" (browse-url "https://gist.github.com/minorugh"))
   ("l" my-open-lepton)
   ("t" (my-open "~/Dropbox/GH/teirei/tex/teirei.txt" :pos 'top))
   ("T" my-teirei-new-post)
   ("s" (my-open "~/Dropbox/GH/s_select/tex/swan.txt" :pos 'top))
   ("S" my-swan-new-post)
   ("k" (my-open "~/Dropbox/GH/kinnei/draft.dat"))
   ("m" (my-open "~/Dropbox/GH/d_select/tex/minoru_sen.txt" :pos 'top))
   ("w" (my-open "~/Dropbox/GH/w_select/tex/minoru_sen.txt" :pos 'top))
   ("f" (my-open "~/Dropbox/GH/d_selext/select.txt" :pos 'top))
   ("F" my-dselext-new-post)
   ("]" my-haiku-note)
   ("[" my-haiku-note-post)
   ("q" top-level)
   ("<f14>"     hydra-dired/body)
   ("<henkan>"  hydra-dired/body)
   ("<muhenkan>" nil))
  :preface
  ;; ------------------------------------------------------------
  ;;  Word Case Helpers
  ;; ------------------------------------------------------------
  (defun my-upcase-word (arg)
    "Convert previous word (or ARG words) to upper case."
    (interactive "p")
    (upcase-word (- arg)))

  (defun my-downcase-word (arg)
    "Convert previous word (or ARG words) to lower case."
    (interactive "p")
    (downcase-word (- arg)))

  (defun my-capitalize-word (arg)
    "Capitalize previous word (or ARG words)."
    (interactive "p")
    (capitalize-word (- arg))))


;; ============================================================
;;  Package Management
;; ============================================================

(leaf *package
  :tag "local"
  :doc "Browse ELPA snapshots and manage packages via hydra."
  :config
  (key-chord-define-global "p@" 'hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
Package: _l_og  _i_nstall  _d_elete  _u_pgrade  up-_a_ll
  "
   ("l" (my-open "~/Dropbox/backup/elpa/LOG/elpa-changes.log" :pos 'bottom))
   ("i" package-install)
   ("u" package-upgrade)
   ("d" package-delete)
   ("a" package-upgrade-all)
   ("<muhenkan>" nil)))


;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; 80-hydra-navication.el ends here
