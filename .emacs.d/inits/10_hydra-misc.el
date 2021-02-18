;;; 10_hydra-misc.el --- Hydra for misc  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

(leaf package-utils
  :ensure t
  :chord ("p@" . hydra-package/body)
  :hydra
  (hydra-package
   (:color red :hint nil)
   "
 📦 Package: _l_ist   _i_nstall   _u_pgrade-list   _a_ll-upgrade   _r_emove   _e_l-get"
   ("i" package-install)
   ("u" package-utils-list-upgrades)
   ("r" package-utils-remove-by-name)
   ("a" package-utils-upgrade-all-and-restart)
   ("l" package-list-packages)
   ("e" select-elget-command)
   ("<muhenkan>" nil)))


(leaf open-favorite-on-browse
  :hydra
  (hydra-browse
   (:hint nil :exit t)
   "
  💰 Shop^        ^💭 SNS^        ^🔃 Repos^       ^🏠 Blog^      ^🙌 Favorite^    ^📝 Others^   ^💣 Github^^       Google
  ^^^^^^^^^^----------------------------------------------------------------------------------------------------------------
  _a_: Amazon      _t_: Twitter    _g_: Gdrive      _h_: GH        _j_: Jorudan     _k_: Keep      _1_: minorugh    _5_: Calendar
  _r_: Rakuten     _u_: Youtube    _f_: Flickr      _b_: xsrv.jp   _n_: News        _p_: Pocket    _2_: gist        _6_: Map
  _y_: Yodobashi   _i_: Instagram  _d_: Dropbox     _e_: Essay     _w_: Weather     _q_: Qiita     _3_: explore     _7_: Gmail
  _K_: Kakaku      _l_: Tumblr     _x_: Xserver     _:_: Blog      _s_: SanyoBas    _,_: Slack     _4_: repo.new    _8_: Photo"
   ("a" (browse-url "https://www.amazon.co.jp/"))
   ("r" (browse-url "https://www.rakuten.co.jp/"))
   ("y" (browse-url "https://www.yodobashi.com/"))
   ("K" (browse-url "http://kakaku.com/"))
   ("u" (browse-url "https://www.youtube.com/channel/UCnwoipb9aTyORVKHeTw159A/videos"))
   ("f" (browse-url "https://www.flickr.com/photos/minorugh/"))
   ("g" (browse-url "https://drive.google.com/drive/u/0/my-drive"))
   ("0" (browse-url "https://gist.github.com/minorugh"))
   ("1" (browse-url "https://github.com/minorugh"))
   ("2" (browse-url "https://gist.github.com/minorugh"))
   ("3" (browse-url "https://github.com/explore"))
   ("4" (browse-url "https://repo.new/"))
   ("5" (browse-url "https://calendar.google.com/calendar/u/0/r?tab=wc"))
   ("6" (browse-url "https://www.google.co.jp/maps"))
   ("7" (browse-url "https://mail.google.com/mail/u/0/?tab=rm&ogbl#inbox"))
   (":" (browse-url "http://blog.wegh.net/"))
   ("e" (browse-url "http://essay.wegh.net/"))
   ("b" (browse-url "https://minorugh.xsrv.jp/"))
   ("s" (browse-url "http://www.sanyo-bus.co.jp/pdf/20201124tarusan_schedule.pdf"))
   ("i" (browse-url "https://www.instagram.com/"))
   ("j" (browse-url "https://www.jorudan.co.jp/"))
   ("n" (browse-url "https://news.yahoo.co.jp/"))
   ("x" (browse-url "https://www.xserver.ne.jp/login_server.php"))
   ("d" (browse-url "https://www.dropbox.com/home"))
   ("q" (browse-url "https://qiita.com/tags/emacs"))
   ("8" (browse-url "https://photos.google.com/?pageId=none"))
   ("k" (browse-url "https://keep.google.com/u/0/"))
   ("l" (browse-url "https://minorugh.tumblr.com"))
   ("w" chromium-weather)
   ("h" chromium-homepage)
   ("p" chromium-pocket)
   ("t" chromium-tweetdeck)
   ("," chromium-slack)
   ("<muhenkan>" nil)
   ("." nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 10_hydra-misc.el ends here
