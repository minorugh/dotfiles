;;; my-xsrv.el --- Personal xsrv (xsrv-GH / xsrv-minorugh) extras. -*- lexical-binding: t -*-
;;; Commentary:
;;; xsrv-2pane（my-open-xsrv-2pane 本体・見た目調整）をまとめた個人用設定。
;;; 通常の dired には一切影響しない。
;;; 70-hydra-dired.el の hydra-dired から :require my-xsrv で読み込む想定。
;;; キーバインド自体は 70-hydra-dired.el 側に定義されている。
;;; Code:

;;; --------------------------------------------------------
;;;  xsrv-2pane UI  (ヘッダー強調・アイコン)
;;;  my-open-xsrv-2pane 専用。通常の dired には影響しない。
;;; --------------------------------------------------------

(defface my-xsrv-2pane-header-face
  '((t (:inherit dired-header :background "#1A2640" :extend t)))
  "xsrv-2pane の dired バッファでヘッダー2行に使う face."
  :group 'dired)

(defun my-xsrv-2pane--fontify-header ()
  "現在の dired バッファの先頭2行（パス行・合計行）に
my-xsrv-2pane-header-face を上書きする。
`face' ではなく `font-lock-face' を使うことで、dired の
font-lock 再描画によって上書き消去されないようにしている。"
  (save-excursion
    (goto-char (point-min))
    (let ((end (progn (forward-line 2) (point))))
      (with-silent-modifications
        (put-text-property (point-min) end 'font-lock-face 'my-xsrv-2pane-header-face)))))

(defvar my-xsrv-2pane-remote-icon-name "nf-md-server"
  "xsrv-GH（rsync ミラー）側のパス行に表示する nerd-icons アイコン名.")

(defvar my-xsrv-2pane-local-icon-name "nf-md-home"
  "Dropbox（ローカル）側のパス行に表示する nerd-icons アイコン名.")

(defun my-xsrv-2pane--icon-for (dir)
  "DIR が xsrv-GH/xsrv-minorugh 配下なら remote アイコン、
それ以外なら local アイコンの文字列を返す。"
  (let ((xsrv-gh-root (expand-file-name "~/src/github.com/minorugh/xsrv-GH/"))
        (xsrv-mn-root (expand-file-name "~/src/github.com/minorugh/xsrv-minorugh/"))
        (dir (expand-file-name dir)))
    (if (or (string-prefix-p xsrv-gh-root dir)
            (string-prefix-p xsrv-mn-root dir))
        (nerd-icons-mdicon my-xsrv-2pane-remote-icon-name)
      (nerd-icons-mdicon my-xsrv-2pane-local-icon-name))))

(defun my-xsrv-2pane--prepend-icon ()
  "dired バッファ1行目（パス行）の先頭にアイコンを付記する。
既に付記済みなら二重に追加しない。"
  (save-excursion
    (goto-char (point-min))
    (unless (get-text-property (point) 'my-xsrv-2pane-icon)
      (let ((icon (my-xsrv-2pane--icon-for default-directory))
            (inhibit-read-only t))
        (with-silent-modifications
          (insert (propertize (concat icon " ")
                              'my-xsrv-2pane-icon t)))))))

(defun my-xsrv-2pane-refresh-ui ()
  "xsrv-2pane バッファの見た目（ヘッダー強調・アイコン）を
まとめて適用する。revert-buffer 後にも呼べるよう冪等にしてある。"
  (when (derived-mode-p 'dired-mode)
    (my-xsrv-2pane--fontify-header)
    (my-xsrv-2pane--prepend-icon)))

(defun my-xsrv-2pane-enable-ui ()
  "現在のバッファを xsrv-2pane 対象として UI 調整を適用する。
my-open-xsrv-2pane から呼ぶこと。"
  (add-hook 'dired-after-readin-hook #'my-xsrv-2pane-refresh-ui nil t)
  (my-xsrv-2pane-refresh-ui))


;;; --------------------------------------------------------
;;;  xsrv-2pane を開く  (my-open-xsrv-2pane 本体)
;;;  70-hydra-dired.el の hydra キーバインドから呼ばれる想定。
;;;  ペイン終了処理（my-2pane-quit / my-dired-quit）は
;;;  70-hydra-dired.el 側に残る汎用機能。
;;; --------------------------------------------------------

(defvar my-2pane-divider-active nil
  "Non-nil while the xsrv-2pane window-divider highlight is active.")

(defun my-2pane-divider-on ()
  "Enable a prominent window-divider, scoped to xsrv-2pane usage."
  (window-divider-mode -1)
  (setq window-divider-default-right-width 6)
  (setq window-divider-default-bottom-width 0)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)
  (set-face-foreground 'window-divider "#ff9900")
  (set-face-foreground 'window-divider-first-pixel "#ff9900")
  (set-face-foreground 'window-divider-last-pixel "#ff9900")
  (setq my-2pane-divider-active t))

(defun my-2pane-divider-off ()
  "Restore window-divider to its default (disabled) state."
  (when my-2pane-divider-active
    (window-divider-mode -1)
    (setq my-2pane-divider-active nil)))

(defun my-open-xsrv-2pane (src-dir pair-dir)
  "Open SRC-DIR and PAIR-DIR side by side."
  (setq my-2pane-origin-buffer (current-buffer))
  (shell-command "~/.emacs.d/elisp/bin/xsrv-backup-smart.sh &")
  (delete-other-windows)
  (dired src-dir)
  (my-xsrv-2pane-enable-ui)
  (split-window-right)
  (other-window 1)
  (dired pair-dir)
  (my-xsrv-2pane-enable-ui)
  (other-window 1)
  (my-2pane-divider-on))


;;; --------------------------------------------------------
;;;  my-2pane-quit との連携
;;;  70-hydra-dired.el の my-2pane-quit-hook に登録するだけで、
;;;  my-2pane-quit 本体（公開版）には一切手を加えない。
;;; --------------------------------------------------------

(add-hook 'my-2pane-quit-hook #'my-2pane-divider-off)

(provide 'my-xsrv)

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; my-xsrv.el ends here
