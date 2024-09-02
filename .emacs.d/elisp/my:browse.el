;;; my:browse.el --- hydra browse configurations.
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
  _A_: Amazon     _t_: Twitter    _d_: Dropbox  _g_: ghub.io  _j_: Jorudan   _k_: Keep    _1_: minorugh  _5_: Mail
  _R_: Rakuten    _y_: Youtube    _f_: Flickr   _m_: snap     _n_: News      _p_: Pocket  _2_: gist      _6_: Map
  _Y_: Yodobashi  _I_: Instagram  _G_: Gdrive   _e_: Essay    _w_: Weather   _q_: Qiita   _3_: masasam   _7_: Earth
  _K_: Kakaku     _T_: Tumblr     _x_: Xserver  _b_: Blog     _S_: SanyoBas  _s_: Slack   _4_: Centaur   _8_: Photo
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
   ("3" (browse-url "https://github.com/masasam"))
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


(provide 'my:browse)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; my:browse.el ends here
