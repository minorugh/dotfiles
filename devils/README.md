# dotfiles/devils/

Emacs 常駐化のための設定群。

---

## 概要

Emacs を「起動しっぱなし」にして高速に使うための仕組み。3つの要素で構成している。

| 要素 | 役割 |
|---|---|
| devilspie + devils_startup.sh | ログイン時に Emacs を自動起動＆最小化 |
| Emacs server (init.el) | emacsclient からフレームを開けるようにする |
| handle-delete-frame (elisp) | 最後の1フレームを閉じても Emacs を終了させない |

---

## 1. 自動起動と最小化 — devilspie

GNOME のセッションと起動に `devils_startup.sh` を登録することで、
ログイン時に Emacs を自動起動しつつ最小化（タスクトレイ格納）する。

`autostart/devils_startup.desktop` 経由で呼び出される。

### devils_startup.sh

```bash
#!/bin/bash
devilspie &
emacs &
sleep 5s
killall -9 devilspie
rm /home/minoru/src/github.com/minorugh/dotfiles/.emacs.d/session*
exit
```

- `devilspie` を先に起動してウィンドウルールを有効化
- `emacs` を起動（devilspie が `emacs.ds` のルールを適用して最小化）
- 5秒後に devilspie を終了（常駐させる必要はないので）
- session ファイルを削除（前回の異常終了時のゴミを掃除）

### emacs.ds

```ds
(if
(is (application_name) "emacs")
(begin (minimize)))
```

devilspie のルールファイル。アプリ名が `emacs` のウィンドウを起動時に最小化する。

---

## 2. Emacs server — init.el

```elisp
(leaf server
  :hook (emacs-startup-hook
         . (lambda ()
             (unless (server-running-p)
               (server-start)))))
```

Emacs 起動時にサーバーを自動起動する。これにより `emacsclient -c` で
新しいフレームを素早く開くことができる。

- フレームを閉じても Emacs 本体（サーバー）は残る
- daemon モードと異なり通常の GUI 起動なので、フォント・テーマ・mozc が正常動作する

---

## 3. 最後のフレームを殺させない — handle-delete-frame

```elisp
(defun handle-delete-frame (event)
  "If it's the last frame, minimize it without deleting it."
  (interactive "e")
  (let ((frame  (posn-window (event-start event)))
        (numfrs (length (visible-frame-list))))
    (cond ((> numfrs 1) (delete-frame frame t))
          ((iconify-frame)))))
```

`frame.el` の `handle-delete-frame` を上書きすることでフレーム削除の動作を変更。

- フレームが2つ以上あるときは普通に閉じる
- 最後の1フレームになったときは **終了せず最小化** する

これにより Emacs サーバーが誤って終了することを防ぐ。
`delete-frame-functions` フックを使う方法も試みたが、
✕ボタン経由の削除を止められなかったため、この実装を採用している。

---

## ファイル構成

```
devils/
├── README.md
├── emacs.ds              # devilspie ルール（起動時最小化）
└── devils_startup.sh     # 自動起動スクリプト
```

dotfiles Makefile の `devilspie` ターゲットでシンボリックリンクを展開する：

```makefile
devilspie:
    mkdir -p ${HOME}/.devilspie
    $(APT) $@
    sudo ln -vsfn ${PWD}/devils/emacs.ds  ${HOME}/.devilspie
    sudo ln -vsfn ${PWD}/devils/devils_startup.sh  /usr/local/bin
    sudo chmod +x /usr/local/bin/devils_startup.sh
    ln -vsf {${PWD},${HOME}}/.config/autostart/devils_startup.desktop
```

---

## 補足：daemon モードとの違い

`emacs --daemon` を使う方法もあるが、GUI なしで起動するため
フォント・テーマ・mozc 等の初期化タイミングが変わり調整が必要になる。
現構成は通常の GUI 起動のまま server 機能だけ有効にしており、
設定の互換性を保ちつつ常駐化を実現している。
