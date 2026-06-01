;;; 40-hydra-broese.el --- hydra browse configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf hydra-browse
  :doc "Selection menu for project work."
  :defun my-github-deploy
  :chord (".."   . hydra-browse/body)
  :bind ("<f15>" . hydra-browse/body)
  :hydra
  (hydra-browse
   (:hint nil :exit t)
   "
  ^Shop^          ^SNS^           ^🔃 Repos^     ^Blog^       ^Life^         ^Social^     ^Github^        ^Google       ^^^Favorites
  ^^^^^^^^^^^^^^^^^────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  _a_: Amazon     _t_: Twitter    _D_: Dropbox   _b_: bible   _j_: Jorudan   _K_: Keep    _1_: minorugh   _c_: calendar^^  _3_: masasam
  _R_: Rakuten    _u_: Youtube    _F_: Flickr    _S_: snap    _n_: News      _p_: Pocket  _2_: gist       _m_: mail/_r_es  _4_: seagle
  _y_: Yodobashi  _i_: Instagram  _G_: Gdrive    _E_: Essay   _w_: Weather   _q_: Qiita   _d_: deploy     _M_: Map
  _k_: Kakaku     _T_: Tumblr     _x_: Xserver   _B_: Blog    _b_: SanyoBas  _s_: Slack   _o_: github.io  _P_: photo
"
   ("a" (browse-url "https://www.amazon.co.jp/"))
   ("R" (browse-url "https://www.rakuten.co.jp/"))
   ("y" (browse-url "https://www.yodobashi.com/"))
   ("k" (browse-url "http://kakaku.com/"))
   ("u" (browse-url "https://www.youtube.com/channel/UCnwoipb9aTyORVKHeTw159A/videos"))
   ("F" (browse-url "https://www.flickr.com/photos/minorugh/"))
   ("G" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("1" (browse-url "https://github.com/minorugh"))
   ("2" (browse-url "https://gist.github.com/minorugh"))
   ("3" (browse-url "https://github.com/masasam"))
   ("4" (browse-url "https://github.com/seagle0128/.emacs.d"))
   ("c" (browse-url "https://calendar.google.com/calendar/r"))
   ("M" (browse-url "https://www.google.co.jp/maps"))
   ("B" (browse-url "http://blog.gospel-haiku.com/"))
   ("E" (browse-url "https://es.gospel-haiku.com/post/"))
   ("S" (browse-url "https://snap.minorugh.com/"))
   ("b" (browse-url "https://bible.minorugh.com/post/"))
   ("i" (browse-url "https://www.instagram.com/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("D" (browse-url "https://www.dropbox.com/home"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("P" (browse-url "https://photos.google.com/?pageId=none"))
   ("K" (browse-url "https://keep.google.com/u/0/"))
   ("T" (browse-url "https://minorugh.tumblr.com"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("g" (browse-url "https://minorugh.github.io/"))
   ("p" (browse-url "https://app.raindrop.io/my/0"))
   ("t" (browse-url "https://twitter.com/gospelhaiku"))
   ("o" (browse-url "https://github.com/minorugh/minorugh.github.io/blob/main/CHANGELOG.md"))
   ("d" #'my-github-deploy)
   ("m" neomutt)
   ("r" neomutt-restart)
   ("M" (browse-url "https://www.google.com/maps/@34.6595995,135.0840072,15z?authuser=0&entry=ttu&g_ep=EgoyMDI2MDUyNy4wIKXMDSoASAFQAw%3D%3D"))
   ("s" (start-process "slack" nil "slack"))
   ("<muhenkan>" nil)
   ("." nil))
  :init
  (defun thunderbird ()
    "Open thunderbird mail-client for Gmail and detach it from Emacs."
    (interactive)
    (call-process "setsid" nil 0 nil "thunderbird"))

  (defun neomutt ()
    "Toggle NeoMutt window."
    (interactive)
    (let ((win (string-trim (shell-command-to-string "wmctrl -l | grep 'NeoMutt Mail'"))))
      (if (string= win "")
          (call-process "setsid" nil 0 nil "neomutt.sh")
	(call-process "wmctrl" nil 0 nil "-a" "NeoMutt Mail"))))

  (defun neomutt-restart ()
    "Kill or restart NeoMutt tmux session."
    (interactive)
    (let ((action (completing-read "NeoMutt: " '("restart" "kill") nil t)))
      (call-process "tmux" nil 0 nil "kill-session" "-t" "mail")
      (when (string= action "restart")
	(call-process "setsid" nil 0 nil "neomutt.sh"))))
  ;; (defun neomutt-restart ()
  ;;     "Kill and restart NeoMutt tmux session."
  ;;     (interactive)
  ;;     (call-process "tmux" nil 0 nil "kill-session" "-t" "mail")
  ;;     (call-process "setsid" nil 0 nil "neomutt.sh"))

;;; github-deploy
;;; changelog-YYYYMMDD.md を ivy で選択して CHANGELOG.md の先頭に追記する。
;;; 処理本体は ~/Dropbox/Changelog/github-deploy.pl に委譲。
;;; push 後のブラウザ確認は Makefile の make git の中で実行。
  (defun my-github-deploy ()
    "Select a changelog-YYYYMMDD.md via ivy and deploy it to CHANGELOG.md."
    (interactive)
    (let* ((base (expand-file-name "~/Dropbox/Changelog/"))
           ;; 直下と archive 配下の changelog-*.md を日付降順で列挙
           (files (sort
                   (mapcar (lambda (f) (file-relative-name f base))
                           (directory-files-recursively
                            base "changelog-[0-9]\\{8\\}\\.md"))
                   #'string>))
           (selected (completing-read "Deploy: " files nil t "^"))
           (src (expand-file-name selected base)))
      ;; 事前確認: 対象ファイルをバッファで開く
      (find-file src)
      (when (y-or-n-p (format "%s を CHANGELOG.md にデプロイしますか?" selected))
	;; Step1: CHANGELOG.md に追記
	(shell-command
	 (format "perl %s %s"
		 (expand-file-name "~/Dropbox/Changelog/github-deploy.pl")
		 src))
	;; Step2: git commit & push → ブラウザ確認は make git の中で実行
	(let ((default-directory
               (expand-file-name "~/src/github.com/minorugh/minorugh.github.io/")))
          (compile "make git"))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 40-hydra-browse.el ends here
