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

;; GUI: <henkan>、TTY: Keychron のマクロが送る M-^
(with-eval-after-load 'evil
  (evil-define-key '(normal insert visual emacs) 'global
    (kbd "M-^")       #'my-hydra-dired
    (kbd "<henkan>") #'my-hydra-dired))

(leaf *hydra-dired
  :after evil
  ;; :bind (("<henkan>" . my-hydra-dired))
  :init
  (defvar my-cron-hint
    (concat (propertize "^" 'face 'hydra-face-blue) "cron"))
  :hydra
  (hydra-dired
   (:hint nil :exit t)
   "
 Quick.dired
  _d_ropbox  _e_macs.d^^^^^^  _i_nits^^  _s_rc  _/_root^^^^^^^^  _._files^  make._b_._k_._m_._u_  fz_8_._9_._0_  _p_assXC  _x_env^^  S_n_ote
  _r_estart  _g_it:_[__-__]_  GH._h__j_  _t_ig  %s`my-cron-hint  _<home>_^  h_o_wm_,_ md_v_iew^^  _@_remote^^^^  _f_lyerr  2p_;__:_  c_l_.i_w_
"
   ("x" my-env-recover)
   ("^" my-make-launch-cron)
   ("@" browse-at-remote)
   ("f" flymake-show-buffer-diagnostics) ; see 20-flymake.el
   ("8" (filezilla "s"))
   ("9" (filezilla "g"))
   ("0" (filezilla "m"))
   ("p" keepassxc)
   ("n" my-simplenote)
   ("<home>" (my-open "~/" :omit))
   (":" my-open-xsrv-2pane-gh)          ; see 90-xsrv-deploy.el
   (";" my-open-xsrv-2pane-minorugh)    ; see 90-xsrv-deploy.el
   ("." (my-open "~/src/github.com/minorugh/dotfiles/"))
   ("d" (my-open "~/Dropbox/"))
   ("i" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/inits/"))
   ("e" (my-open "~/src/github.com/minorugh/dotfiles/.emacs.d/"))
   ("h" (my-open "~/Dropbox/GH/"))
   ("j" (my-open "~/Dropbox/minorugh.com/"))
   ("s" (my-open "~/src/github.com/minorugh/"))
   ("/" (my-open "/" :omit))
   ("k" (my-make "-k"))
   ("b" (my-make "bk"))
   ("m" (my-make "mv"))
   ("u" (my-make "up"))
   ("r" my-restart-emacs)
   ("v" markdown-preview)               ; see 60-markdown.el
   ("o" howm-list-all)                  ; see 50-howm.el
   ("," my-howm-create-with-category)   ; see 50-howm.el
   ("l" (my-open-main "~/src/github.com/minorugh/cl/"))
   ("w" (my-open-main "~/src/github.com/minorugh/piclink/"))
   ("[" my-make-auto-commit)
   ("]" my-make-git)
   ("-" git-peek)
   ("g" counsel-git-grep)
   ("t" my-open-tig)
   ("_" my-hydra-work)
   ("q" top-level)
   ("M-^" my-hydra-work)
   ("<henkan>"  my-hydra-work)
   ("M-]" nil)
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

  (defun my-open-main (path)
    "Open PATH only on the main machine."
    (if my-main-machine-p
        (my-open path)
      (user-error "P1 専用です: %s" path)))

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

  (defun my-make-launch-cron ()
    "Open the Makefile in `dotfiles/cron` to immediately launch the target picker."
    (interactive)
    (find-file (expand-file-name "~/src/github.com/minorugh/dotfiles/cron/Makefile"))
    (my-make-ivy-integrated))

  (defun my-env-recover ()
    "Reload xmodmap, restart Dropbox and KeePassXC."
    (interactive)
    (shell-command "xmodmap ~/.Xmodmap > /dev/null 2>&1")
    (start-process "dropbox-restart" nil "dropbox-restart.sh")
    (start-process-shell-command
     "keepassxc-restart" nil
     "pkill -x keepassxc; while pgrep -x keepassxc > /dev/null; do sleep 1; done; keepassxc.sh")
    (message "ENV RECOVERED: xmodmap, dropbox, keepassxc"))

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
      (apply #'call-process "setsid" nil 0 nil "filezilla" args)))

  (defun my-simplenote ()
    "Launch Simplenote, or copy its active note into a new howm memo.
If no Simplenote window exists, start simplenote.sh detached from Emacs.
Otherwise raise the window, send C-a C-c, return to Emacs with emacs-toggle,
create a howm memo, paste the clipboard, then close the Simplenote window."
    (interactive)
    (let ((wid (string-trim
                (shell-command-to-string
                 "xdotool search --name '^Simplenote' | tail -n1"))))
      (if (string-empty-p wid)
          (call-process "setsid" nil 0 nil "simplenote.sh")
        (call-process "xdotool" nil nil nil "windowactivate" "--sync" wid)
        (sleep-for 0.3)
        (call-process "xdotool" nil nil nil "key" "ctrl+a" "ctrl+c")
        (sleep-for 0.3)
        (call-process "emacs-toggle")
        (my-howm--insert-category "note: ")
        (yank)
        (call-process "wmctrl" nil nil nil "-i" "-c" wid)))))


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

(leaf *hydra-work
  :after evil
  :hydra
  (hydra-work
   (:hint nil :exit t :body-pre (require 'post-template))
   "
 Work.menu
  _d_:日記  _m_:毎日  _w_:若鮎  _t_:定例  _M_:月例^^  _p_rint.buf  yas._n_._v_._i_  _c_aption.._u_p.d_o_wn  _b_log2jpg
  _a_:合評  _f_:週秀  _s_:吟行  _k_:近詠  _Y_:年度^^  _g_ist._l_ept  _e_asy-hugo^^  _j_unk._h_owm._+_scale  webp_2_jpg
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
   ("b" my-resize-blog)
   ("2" my-webp-to-jpg)
   ("_" my-hydra-dired)
   ("q" top-level)
   ("M-]" nil)
   ("<muhenkan>" nil))
  :preface
  ;; ------------------------------------------------------------
  ;;  Word Case Helpers
  ;; ------------------------------------------------------------
  (defun my-key-chord-reset ()
    "Clear `key-chord-last-unmatched' so that the next chord is detected."
    (interactive)
    (setq key-chord-last-unmatched nil))

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
    (capitalize-word (- arg)))

  (defun my-resize-blog ()
    "Resize marked image(s) in Dired to blog size (max width 800px)."
    (interactive)
    (dolist (file (dired-get-marked-files))
      (let* ((base (file-name-sans-extension file))
             (ext (file-name-extension file))
             (output (format "%s_blog.%s" base ext)))
        (call-process "convert" nil nil nil
                      file
                      "-resize" "800x>"
                      "-strip"
                      "-quality" "85"
                      output)))
    (revert-buffer))

  (defun my-webp-to-jpg ()
    "Convert marked WebP file(s) in Dired to JPG."
    (interactive)
    (dolist (file (dired-get-marked-files))
      (let* ((base (file-name-sans-extension file))
             (output (concat base ".jpg")))
        (call-process "convert" nil nil nil
                      file
                      "-strip"
                      "-quality" "85"
                      output)))
    (revert-buffer)))


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
