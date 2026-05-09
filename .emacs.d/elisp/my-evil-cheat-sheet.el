;;; my-evil-cheat-sheet.el --- Evil keybinding cheat sheet -*- lexical-binding: t -*-
;;; Commentary:
;; Claude-recommended evil keybindings, tailored for this config.
;; Bound to ? in normal-state.
;;
;; Keys in cheat buffer:
;;   i        ivy jump (all lines)
;;   m        jump → 移動
;;   e        jump → 編集
;;   o        jump → operator
;;   v        jump → visual-state
;;   n        jump → normal-stateに留まるコツ
;;   q        quit
;;; Code:

(defvar evil-cheat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") (lambda () (interactive) (my-evil-cheat-sheet--ivy-jump)))
    (define-key map (kbd "m") (lambda () (interactive) (my-evil-cheat-sheet--jump-to "【移動】")))
    (define-key map (kbd "e") (lambda () (interactive) (my-evil-cheat-sheet--jump-to "【編集")))
    (define-key map (kbd "o") (lambda () (interactive) (my-evil-cheat-sheet--jump-to "【operator")))
    (define-key map (kbd "v") (lambda () (interactive) (my-evil-cheat-sheet--jump-to "【visual")))
    (define-key map (kbd "n") (lambda () (interactive) (my-evil-cheat-sheet--jump-to "【normal")))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for evil-cheat buffer.")

(defun my-evil-cheat-sheet--ivy-jump ()
  "Jump to a line in *evil-cheat* via ivy."
  (let ((cands nil))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (unless (string-blank-p line)
            (push (cons line (line-beginning-position)) cands)))
        (forward-line 1)))
    (ivy-read "Jump: " (reverse cands)
              :caller 'my-evil-cheat-sheet--ivy-jump
              :action (lambda (x) (goto-char (cdr x))))))

(defun my-evil-cheat-sheet--jump-to (keyword)
  "Jump directly to section matching KEYWORD."
  (goto-char (point-min))
  (search-forward keyword nil t)
  (beginning-of-line))

(defun my-evil-cheat-sheet ()
  "Show evil keybindings cheat sheet.
i: ivy jump  m/e/o/v/n: section jump  q: quit"
  (interactive)
  (let ((buf (get-buffer-create "*evil-cheat*")))
    (with-current-buffer buf
      (fundamental-mode)
      (evil-emacs-state)
      (use-local-map evil-cheat-mode-map)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "\
【移動】
  w b        単語単位 前進/後退
  e          次の単語末尾へ
  0 $        行頭/行末
  fx Fx      行内の文字xへ（F は逆方向）
  tx         文字xの直前へ
  { }        段落単位で移動
  %          対応する括弧へ

【編集（normal-state のまま）】
  x          カーソル文字を削除
  X          カーソル前の文字を削除
  rx         1文字だけ置換（emacs-state に入らない）
  dd         行ごと削除（カット）
  D          行末まで削除
  yy         行全体をヤンク
  p P        カーソルの後/前にペースト
  J          次行を現在行に連結
  ~          大文字/小文字を切り替え
  u  C-r     undo / redo
  .          直前の編集を繰り返す ← 強力！

【operator + motion】
  dw diw daw 単語削除（各種）
  yw yiw     単語ヤンク
  2dd 3w     数字+コマンドで繰り返し
  ※ cw 等 c系は無効化済み

【visual-state（@ で開始）】
  @          文字選択モード開始
  _          行選択モード開始
  viw vi\"   単語/クォート内を選択
  va(        () ごと選択
  d y        削除/ヤンク
  u U        小文字化/大文字化
  PgUp/Dn    選択範囲 拡大/縮小

【normal-state に留まるコツ】
  r          1文字修正はこれで完結
  x          不要な1文字の削除
  dd→移動→p  行の移動
  diw→i→無変換  単語の置換
  J          改行を消して行を繋げる
  .          直前の編集を繰り返す
")
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (switch-to-buffer buf)))

(provide 'my-evil-cheat-sheet)
;;; my-evil-cheat-sheet.el ends here
