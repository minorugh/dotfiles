;;; 30_hydra-menu.el --- Hydra configuration for work menu.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-work-menu
  :doc "Selection menu for project work"
  :bind ("<henkan>" . hydra-work/body)
  :hydra
  (hydra-work
   (:hint nil :exit t)
   "
   Project.menu
  _d_:日記  _m_:毎日  _k_:兼題  _t_:定例  _f_:週秀  _[__]_:創作  _e_:hugo^^  _p_s._r_  _x_srv._g_^^
  _a_:合評  _n_:近詠  _s_:吟行  _._:添削  _w_:添日  _y__,_:年度  g_i_st._l_  re_@_p^^  _b_kup._c_._u_
"
   ("p" ps-print-buffer)
   ("r" ps-print-region)
   ("y" my:year)
   ("Y" my:year-new-post)
   ("," my:year-draft)
   ("a" my:apvoice)
   ("A" my:apvoice-new-post)
   ("b" make-backup)
   ("u" make-ghuser)
   ("c" make-commit)
   ("@" browse-at-remote)
   ("e" easy-hugo)
   ("d" my:diary)
   ("D" my:diary-new-post)
   ("w" my:tpdia)
   ("W" my:tpdia-new-post)
   ("i" gist-region-or-buffer)
   ("l" (browse-url "https://gist.github.com/minorugh"))
   ("t" my:teirei)
   ("T" my:teirei-new-post)
   ("s" my:swan)
   ("S" my:swan-new-post)
   ("N" my:kinnei)
   ("n" my:kinnei-draft)
   ("m" my:d_kukai)
   ("k" my:m_kukai)
   ("." my:tpost)
   ("f" my:dselext)
   ("F" my:dselext-new-post)
   ("+" text-scale-adjust)
   ("]" my:haiku-note)
   ("[" my:haiku-note-post)
   ("x" my:xsrv-dir)
   ("X" chromium-xsrv)
   ("g" my:github-dir)
   ("G" chromium-github)
   ("<henkan>" hydra-quick/body)
   ("<muhenkan>" nil))
  :init
  (defun lepton ()
	"Open lepton."
	(interactive)
	(compile "~/Appimage/Lepton-1.10.0.AppImage")
	(delete-other-windows))

  (defun make-backup ()
	"Backup all."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make backup")))

  (defun make-ghuser ()
	"Sync GH data by rsync."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make rsync-user")))

  (defun make-commit ()
	"Auto commit."
	(interactive)
	(let* ((default-directory (expand-file-name "~/Dropbox")))
	  (compile "make git-commit"))))


(leaf *hydra-quick-menu
  :doc "Slection menu for quick access"
  :bind ("M-." . hydra-quick/body)
  :hydra
  (hydra-quick
   (:hint nil :exit t)
   "
   Quick.dired
  _d_ropbox  _e_macs.d^^  _i_nits  root_/_  _s_rc  _._files  scale_+_  _z_illa  make._c__k__g_|_b__m__u_  _>_draft
  _r_estart  magit_[__]_  _t_ramp  GH_h_  _o_rg  _<home>_  _f_lychk  _p_assx  howm._,__;__@_|md_v_^^^^  _a_genda
  "
   ("p" keepassxc)
   ("a" (org-agenda nil "a"))
   ("+" text-scale-adjust)
   ("o" my:org-dir)
   ("t" counsel-tramp)
   ("q" my:tramp-quit)
   ("<home>" my:home-dir)
   ("d" my:dropbox)
   ("D" chromium-dropbox)
   ("." my:dotfiles-dir)
   ("i" my:inits-dir)
   ("e" my:emacs-dir)
   ("h" my:gh-dir)
   ("r" restart-emacs)
   ("v" markdown-preview)
   ("@" howm-list-all)
   ("," my:howm-create-memo)
   (";" my:howm-create-tech)
   ("c" hydra-make/body)
   ("k" my:make-k)
   ("g" my:make-git)
   ("b" my:make-bklog)
   ("m" my:make-move)
   ("u" my:make-upsftp)
   (">" my:make-draft)
   ("/" my:root-dir)
   ("f" flycheck-list-errors)
   ;; ("f" make-frame-command)
   ;; ("0" delete-frame)
   ("_" delete-other-windows)
   ("[" hydra-git/body)
   ("]" my:magit-status)
   ("s" my:scr-dir)
   ("z" filezilla)
   ("M-." hydra-work/body)
   ("<muhenkan>" nil))
  :init
  (defun keepassxc ()
	"Open keepassxc with auto passwd input."
	(interactive)
	(compile "secret-tool lookup type kdb | keepassxc --pw-stdin ~/Dropbox/backup/passwd/keypassX/20191105.kdbx")
	(delete-other-windows))

  (defun my:magit-status ()
	"Display message if magit in dashboard."
	(interactive)
	(if (string= "*dashboard*" (buffer-name))
		(message "Can't magit in Dashboard！")
	  (magit-status)))

  (defun filezilla ()
	"Open filezilla."
	(interactive)
	(compile "filezilla -s")
	(delete-other-windows)))


(leaf *hydra-browse
  :doc "Selection menu for project work"
  :hydra
  (hydra-browse
   (:hint nil :exit t)
   "
  ^ Shop^        ^ SNS^        ^🔃 Repos^       ^ Blog^       ^ Life^        ^ Social^    ^ Github^      oogle
  ^^^^^^^^───────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  _A_: Amazon      _t_: Twitter    _d_: Dropbox     _g_: ghub.io    _j_: Jorudan     _k_: Keep      _1_: minorugh    _5_: Mail
  _R_: Rakuten     _y_: Youtube    _f_: Flickr      _m_: snap       _n_: News        _p_: Pocket    _2_: gist        _6_: Map
  _Y_: Yodobashi   _I_: Instagram  _G_: Gdrive      _e_: Essay      _w_: Weather     _q_: Qiita     _3_: explore     _7_: Earth
  _K_: Kakaku      _T_: Tumblr     _x_: Xserver     _b_: Blog       _S_: SanyoBas    _s_: Slack     _4_: Centaur     _8_: Photo
"
   ("A" (browse-url "https://www.amazon.co.jp/"))
   ("R" (browse-url "https://www.rakuten.co.jp/"))
   ("Y" (browse-url "https://www.yodobashi.com/"))
   ("K" (browse-url "http://kakaku.com/"))
   ("y" (browse-url "https://www.youtube.com/channel/UCnwoipb9aTyORVKHeTw159A/videos"))
   ("f" (browse-url "https://www.flickr.com/photos/minorugh/"))
   ("G" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("0" (browse-url "https://gist.github.com/minorugh"))
   ("1" (browse-url "https://github.com/minorugh"))
   ("2" (browse-url "https://gist.github.com/minorugh"))
   ("3" (browse-url "https://github.com/explore"))
   ("4" (browse-url "https://github.com/seagle0128/.emacs.d"))
   ("5" (browse-url "https://mail.google.com/mail/"))
   ("6" (browse-url "https://www.google.co.jp/maps"))
   ("7" (browse-url "https://earth.google.com/web/"))
   ("b" (browse-url "http://blog.gospel-haiku.com/"))
   ("e" (browse-url "https://es.gospel-haiku.com/post/"))
   ("m" (browse-url "https://snap.minorugh.com/"))
   ("S" (browse-url "https://www.sanyo-bus.co.jp/highway/maiko.html"))
   ("I" (browse-url "https://www.instagram.com/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("d" (browse-url "https://www.dropbox.com/home"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("8" (browse-url "https://photos.google.com/?pageId=none"))
   ("k" (browse-url "https://keep.google.com/u/0/"))
   ("T" (browse-url "https://minorugh.tumblr.com"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("g" (browse-url "https://minorugh.github.io/"))
   ("p" (browse-url "https://getpocket.com/a/queue/"))
   ("t" (browse-url "https://tweetdeck.twitter.com/"))
   ("s" (browse-url "https://emacs-jp.slack.com/messages/C1B73BWPJ/"))
   ("<muhenkan>" nil)
   ("." nil))
  :config
  (defun chromium-keep-new ()
	"Chromium keep new."
	(interactive)
	(browse-url "https://keep.new/"))

  (defun chromium-dropbox ()
	"Chromium gmail."
	(interactive)
	(browse-url "https://www.dropbox.com/h?role=personal/"))

  (defun chromium-tegaki ()
	"Chromium tegaki."
	(interactive)
	(browse-url "https://mojinavi.com/tegaki")))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 30_hydra-menu.el ends here
