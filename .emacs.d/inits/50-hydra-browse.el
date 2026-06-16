;;; 50-hydra-browse.el --- Hydra browse configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; ============================================================
;;;  Hydra Browse  (ブックマークランチャー)
;;; ============================================================

(leaf hydra-browse
  :doc "Browser bookmark launcher via hydra."
  :defun my-github-deploy
  :chord (".."   . hydra-browse/body)
  :hydra
  (hydra-browse
   (:hint nil :exit t)
   "
  ^Shop^          ^SNS^           ^🔃 Repos^     ^Blog^       ^Life^         ^Social^     ^Github^        ^Google          ^^^Favorites
  ^^^^^^^^^^^^^^^^^────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
  _a_: Amazon     _t_: Twitter    _D_: Dropbox   _B_: bible   _j_: Jorudan   _K_: Keep    _1_: minorugh   _c_: calendar^^    _3_: masasam
  _R_: Rakuten    _u_: Youtube    _F_: Flickr    _S_: snap    _n_: News      _p_: Pocket  _2_: gist       _m_: mutt/_b_ird   _4_: seagle
  _y_: Yodobashi  _i_: Instagram  _G_: Gdrive    _E_: Essay   _w_: Weather   _q_: Qiita   _d_: deploy     _M_: Map^^         _5_: Docker GH
  _k_: Kakaku     _T_: Tumblr     _x_: Xserver   _l_: Blog    _b_: SanyoBas  _s_: Slack   _o_: github.io  _P_: photo^^       _6_: Docker minoruGH
"
   ("a" (browse-url "https://www.amazon.co.jp/"))
   ("R" (browse-url "https://www.rakuten.co.jp/"))
   ("y" (browse-url "https://www.yodobashi.com/"))
   ("k" (browse-url "http://kakaku.com/"))
   ("t" (browse-url "https://twitter.com/gospelhaiku"))
   ("u" (browse-url "https://www.youtube.com/channel/UCnwoipb9aTyORVKHeTw159A/videos"))
   ("i" (browse-url "https://www.instagram.com/"))
   ("T" (browse-url "https://minorugh.tumblr.com"))
   ("D" (browse-url "https://www.dropbox.com/home"))
   ("F" (browse-url "https://www.flickr.com/photos/minorugh/"))
   ("G" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("B" (browse-url "https://bible.minorugh.com/post/"))
   ("S" (browse-url "https://snap.minorugh.com/"))
   ("E" (browse-url "https://es.gospel-haiku.com/post/"))
   ("l" (browse-url "http://blog.gospel-haiku.com/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("b" thunderbird)
   ("K" (browse-url "https://keep.google.com/u/0/"))
   ("p" (browse-url "https://app.raindrop.io/my/0"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("s" (start-process "slack" nil "slack"))
   ("1" (browse-url "https://github.com/minorugh"))
   ("2" (browse-url "https://gist.github.com/minorugh"))
   ("d" #'my-github-deploy)
   ("o" (browse-url "https://github.com/minorugh/minorugh.github.io/blob/main/CHANGELOG.md"))
   ("g" (browse-url "https://minorugh.github.io/"))
   ("c" (browse-url "https://calendar.google.com/calendar/r"))
   ("M" (browse-url "https://www.google.com/maps/@34.6595995,135.0840072,15z?authuser=0&entry=ttu&g_ep=EgoyMDI2MDUyNy4wIKXMDSoASAFQAw%3D%3D"))
   ("P" (browse-url "https://photos.google.com/?pageId=none"))
   ("3" (browse-url "https://github.com/masasam"))
   ("4" (browse-url "https://github.com/seagle0128/.emacs.d"))
   ("5" (browse-url "http://gh.local:8080/"))
   ("6" (browse-url "http://minorugh.local:8080/"))
   ("m" neomutt)
   ("<muhenkan>" nil)
   ("." nil))


;;; ============================================================
;;;  Thunderbird
;;; ============================================================

  :init
  (defun thunderbird ()
    "Open Thunderbird mail client, detached from Emacs."
    (interactive)
    (call-process "setsid" nil 0 nil "thunderbird"))


;;; ============================================================
;;;  GitHub Deploy
;;;
;;;  changelog-YYYYMMDD.md を ivy で選択して CHANGELOG.md の先頭に追記する。
;;;  処理本体は ~/Dropbox/Changelog/github-deploy.pl に委譲。
;;;  push 後のブラウザ確認は Makefile の make git の中で実行。
;;; ============================================================

  (defun my-github-deploy ()
    "Select a changelog-YYYYMMDD.md via ivy and deploy it to CHANGELOG.md."
    (interactive)
    (let* ((base  (expand-file-name "~/Dropbox/Changelog/"))
           (files (sort
                   (mapcar (lambda (f) (file-relative-name f base))
                           (directory-files-recursively
                            base "changelog-[0-9]\\{8\\}\\.md"))
                   #'string>))
           (selected (completing-read "Deploy: " files nil t "^"))
           (src (expand-file-name selected base)))
      ;; Step 0: 対象ファイルをバッファで確認
      (find-file src)
      (when (y-or-n-p (format "%s を CHANGELOG.md にデプロイしますか?" selected))
        ;; Step 1: CHANGELOG.md に追記
        (shell-command
         (format "perl %s %s"
                 (expand-file-name "~/Dropbox/Changelog/github-deploy.pl")
                 src))
        ;; Step 2: git commit & push（ブラウザ確認は make git の中で実行）
        (let ((default-directory
               (expand-file-name "~/src/github.com/minorugh/minorugh.github.io/")))
          (compile "make git"))))))


;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; 50-hydra-browse.el ends here
