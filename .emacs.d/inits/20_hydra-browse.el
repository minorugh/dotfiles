;;; 20_hydra-browse.el --- Hydra browse utilis configuration for misc utils. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf *hydra-browse
  :hydra
  (hydra-browse
   (:hint nil :exit t)
   "
  ^ Shop^        ^ SNS^        ^🔃 Repos^       ^ Blog^       ^ Life^        ^ Social^    ^ Github^      oogle
  ^^^^^^^^------------------------------------------------------------------------------------------------------------------
  _A_: Amazon      _t_: Twitter    _d_: Dropbox     _g_: ghub.io    _j_: Jorudan     _k_: Keep      _1_: minorugh    _5_: Translate
  _R_: Rakuten     _y_: Youtube    _f_: Flickr      _x_: xsrv.jp    _n_: News        _p_: Pocket    _2_: gist        _6_: Map
  _Y_: Yodobashi   _I_: Instagram  _G_: Gdrive      _e_: Essay      _w_: Weather     _q_: Qiita     _3_: explore     _7_: Earth
  _K_: Kakaku      _T_: Tumblr     _X_: Xserver     _b_: Blog       _S_: SanyoBas    _s_: Slack     _4_: Centaur     _8_: Photo
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
   ("5" (browse-url "https://translate.google.co.jp/?hl=ja"))
   ("6" (browse-url "https://www.google.co.jp/maps"))
   ("7" (browse-url "https://earth.google.com/web/"))
   ("b" (browse-url "http://blog.wegh.net/"))
   ("e" (browse-url "http://essay.wegh.net/"))
   ("x" (browse-url "https://minorugh.xsrv.jp/"))
   ("S" (browse-url "http://www.sanyo-bus.co.jp/pdf/20201124tarusan_schedule.pdf"))
   ("I" (browse-url "https://www.instagram.com/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("X" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("d" (browse-url "https://www.dropbox.com/home"))
   ("q" (browse-url "https://qiita.com/minoruGH"))
   ("8" (browse-url "https://photos.google.com/?pageId=none"))
   ("k" (browse-url "https://keep.google.com/u/0/"))
   ("T" (browse-url "https://minorugh.tumblr.com"))
   ("w" (browse-url "https://tenki.jp/week/6/31/"))
   ("g" (browse-url "https://minorugh.github.io/docs/config.html"))
   ("p" (browse-url "https://getpocket.com/a/queue/"))
   ("t" (browse-url "https://tweetdeck.twitter.com/"))
   ("s" (browse-url "https://emacs-jp.slack.com/messages/C1B73BWPJ/"))
   ("<muhenkan>" nil)
   ("." nil)))


;; Local Variables:
;; no-byte-compile: t
;; End:
;;; 20_hydra-browse.el ends here
