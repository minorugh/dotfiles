;;; 30_hydra-browse.el --- hydra browse configurations.
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-browse
  :doc "Selection menu for project work"
  :chord (".." . hydra-browse/body)
  :hydra
  (hydra-browse
   (:hint nil :exit t)
   "
  ^ Shop^       ^ SNS^        ^🔃 Repos^    ^ Blog^     ^ Life^      ^ Social^  ^ Github^    oogle
  ^^^^^^^^^^^^^^^^^──────────────────────────────────────────────────────────────────────────────────────────────────
  _a_: Amazon     _x_: Twitter    _D_: Dropbox  _g_: ghub.io  _j_: Jorudan   _K_: Keep    _1_: minorugh  _c_: Calendar
  _r_: Rakuten    _u_: Youtube    _F_: Flickr   _S_: snap     _n_: News      _p_: Pocket  _2_: gist      _m_: Mail
  _y_: Yodobashi  _i_: Instagram  _G_: Gdrive   _E_: Essay    _w_: Weather   _q_: Qiita   _3_: masasam   _M_: Maps
  _k_: Kakaku     _T_: Tumblr     _X_: Xserver  _B_: Blog     _b_: SanyoBas  _s_: Slack   _4_: Centaur   _P_: Photo
"
   ("a" (browse-url "https://www.amazon.co.jp/"))
   ("r" (browse-url "https://www.rakuten.co.jp/"))
   ("y" (browse-url "https://www.yodobashi.com/"))
   ("k" (browse-url "http://kakaku.com/"))
   ("u" (browse-url "https://www.youtube.com/channel/UCnwoipb9aTyORVKHeTw159A/videos"))
   ("F" (browse-url "https://www.flickr.com/photos/minorugh/"))
   ("G" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("0" (browse-url "https://gist.github.com/minorugh"))
   ("1" (browse-url "https://github.com/minorugh"))
   ("2" (browse-url "https://gist.github.com/minorugh"))
   ("3" (browse-url "https://github.com/masasam"))
   ("4" (browse-url "https://github.com/seagle0128/.emacs.d"))
   ("c" (browse-url "https://calendar.google.com/calendar/r"))
   ("m" (compile "neomutt.sh"))
   ("M" (browse-url "https://www.google.co.jp/maps"))
   ("B" (browse-url "http://blog.gospel-haiku.com/"))
   ("E" (browse-url "https://es.gospel-haiku.com/post/"))
   ("S" (browse-url "https://snap.minorugh.com/"))
   ("b" (browse-url "https://www.sanyo-bus.co.jp/highway/maiko.html"))
   ("i" (browse-url "https://www.instagram.com/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("X" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("D" (browse-url "https://www.dropbox.com/home"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("P" (browse-url "https://photos.google.com/?pageId=none"))
   ("K" (browse-url "https://keep.google.com/u/0/"))
   ("T" (browse-url "https://minorugh.tumblr.com"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("g" (browse-url "https://minorugh.github.io/"))
   ("p" (browse-url "https://getpocket.com/a/queue/"))
   ("x" (browse-url "https://twitter.com/gospelhaiku"))
   ("s" (shell-command "slack"))
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
;; no-byte-compile: t
;; End:
;;; 30_hydra-browse.el ends here
