;;; 05-corfu.el --- Corfu configurations.    -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (setq debug-on-error t)

;;; completion (corfu + cape + yasnippet + mozc)

(leaf corfu
  :ensure t
  :doc "Completion in Region Function."
  :hook (after-init-hook . global-corfu-mode)
  :bind (("<backtab>" . completion-at-point)
         (:corfu-map
          ("<tab>"      . corfu-next)
          ("<backtab>"  . corfu-previous)
          ("<muhenkan>" . corfu-quit)))
  :config
  (setq corfu-auto          t)
  (setq corfu-auto-delay    0.5)
  (setq corfu-auto-prefix   2)
  (setq corfu-cycle         t)
  (setq corfu-quit-no-match t)
  (setq completion-ignore-case t))

(leaf yasnippet
  :ensure t
  :doc "Template system: Operate only by your own work."
  :hook (after-init-hook . yas-global-mode)
  :config
  (setq yas-verbosity 0)
  (setq yas-indent-line 'fixed)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(leaf cape
  :ensure t
  :doc "Completion At Point Extensions."
  :hook
  ;; グローバルな CAPF リストに cape バックエンドを追加する。
  ;; add-hook + lambda で after-init 後に実行することで、
  ;; 各メジャーモードが設定する CAPF を上書きせずに末尾へ追記できる。
  (after-init-hook . (lambda ()
                       (add-to-list 'completion-at-point-functions
                                    #'cape-file t)
                       (add-to-list 'completion-at-point-functions
                                    #'cape-dabbrev t))))

(leaf yasnippet-capf
  :ensure t
  :after (yasnippet cape)
  :config
  ;; yasnippet-capf は yasnippet / cape のロード後に登録する。
  ;; cape-file / cape-dabbrev より前に置いてスニペット候補を優先させる。
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(leaf mozc
  :ensure t
  :custom
  ;; mozc は独自ポップアップで候補表示するため corfu とは独立して動作する。
  ;; mozc-completion-at-point は標準提供されないので CAPF ブリッジは設けない。
  (default-input-method . "japanese-mozc"))


;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; 05-corfu.el ends here
