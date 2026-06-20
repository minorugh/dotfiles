;;; my-xsrv.el --- Personal xsrv (xsrv-GH / xsrv-minorugh) UI extras. -*- lexical-binding: t -*-
;;; Commentary:
;;; xsrv-2pane（my-open-xsrv-2pane, 70-hydra-dired.el 定義）専用の見た目調整。
;;; 通常の dired には一切影響しない。
;;; 40-remote.el から (require 'my-xsrv) で読み込む想定。
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

(provide 'my-xsrv)

;; Local Variables:
;; byte-compile-warnings: (not free-vars docstrings unresolved)
;; End:
;;; my-xsrv.el ends here
