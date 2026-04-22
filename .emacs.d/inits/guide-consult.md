# Changelog 2026-04-22

## テーマ: counsel/ivy → consult/vertico への移行実験

---

## 新規作成ファイル

### `04-counsel-TEST-consult.el` → 正式名 `04-consult.el`

counsel/ivy 環境と並走できるテスト用設定ファイル。
init.el は一切触らず、`~/.emacs.d/inits/04-counsel.el` を `.bak` に退避して
このファイルを同名で置くだけで切り替え可能。

---

## 導入パッケージ

| パッケージ | 役割 | 置き換え元 |
|---|---|---|
| vertico | ミニバッファ縦表示 | ivy-mode |
| orderless | スペース区切り AND 検索 | ivy--regex-plus |
| marginalia | 候補アノテーション表示 | ivy-rich |
| consult | 強化補完コマンド群 | counsel / swiper |

---

## キーバインド対応表

| キー | 旧 | 新 |
|---|---|---|
| `M-x` | counsel-M-x | execute-extended-command (vertico+orderless が補完) |
| `C-:` | counsel-switch-buffer | consult-buffer |
| `C-x C-f` | counsel-find-file | find-file (vertico が補完) |
| `C-x g` | counsel-git | consult-fd / consult-find |
| `s-a` | counsel-git-grep | consult-ripgrep / consult-grep |
| `M-y` | counsel-yank-pop | consult-yank-pop |
| `C-,` | counsel-mark-ring | consult-mark |
| `C-s` | swiper | consult-line |
| `s-s` | swiper-thing-at-point | consult-line-thing-at-point |

---

## トラブルシューティング記録

### consult-M-x が存在しない
現行 consult (2026-04-21版) では `consult-M-x` は廃止済み。
`M-x` は標準の `execute-extended-command` のまま運用。vertico+orderless で同等の補完が得られる。

### consult--source-* 変数が void
`consult--source-hidden-buffer` / `consult--source-modified-buffer` / `consult--source-buffer`
はいずれも現行版で非公開化または廃止。`consult-buffer-sources` のカスタマイズは行わず
デフォルトに委ねることで解決。

### minibuffer-keyboard-quit が void
ivy が提供していた関数。vertico 環境では存在しない。
`abort-minibuffers` (Emacs29 built-in) への alias で互換。

### marginalia が起動時に有効にならない
`after-init-hook` のタイミング問題。`:init (marginalia-mode 1)` に変更して解決。

---

## その他の設定メモ

### savehist
`00-base.el` に既存設定あり。テストファイルの savehist ブロックは削除し、
`extended-command-history` の追加のみ `00-base.el` の `:config` 内で完結。

```elisp
(setq savehist-additional-variables '(kill-ring extended-command-history))
```

### consult-buffer フェイス調整
```elisp
(with-eval-after-load 'consult
  (set-face-attribute 'consult-buffer nil
                      :weight 'bold
                      :foreground "#FFAB70"))
```

### consult フェイステンプレート
```elisp
(with-eval-after-load 'consult
  (set-face-attribute 'consult-buffer   nil :weight 'bold   :foreground "#FFAB70")
  (set-face-attribute 'consult-file     nil :weight 'normal :foreground "#79B8FF")
  (set-face-attribute 'consult-bookmark nil :weight 'normal :foreground "#B392F0")
  (set-face-attribute 'consult-line-number-wrapped nil :foreground "#F97583"))
```

### vertico-count
```elisp
(setq vertico-count 10)  ; 実行中に変更: M-x eval-expression RET (setq vertico-count N)
```

### consult-preview 遅延
```elisp
(setq consult-preview-key '(:debounce 0.5 any))
;; nil にすると RET を押したときだけプレビュー
```

### my-muhenkan vertico 版
`minibuffer-keyboard-quit` → `abort-minibuffers` に差し替えた版を
`04-consult.el` 末尾に追記済み。元ファイルの定義をマスクして動作。

---

## 断捨離予定（移行確定後）

```
M-x package-delete RET counsel RET
M-x package-delete RET amx     RET
M-x package-delete RET ivy-rich RET
M-x package-delete RET swiper  RET
```

⚠️ swiper 削除時に ivy が道連れになる場合は `package-install ivy` で再インストール。

---

## ivy を残す理由

- `git-peek.el` — `ivy-read` を2箇所使用
- `my-makefile.el` — `ivy-read` を1箇所使用

将来 `ivy-read` → `completing-read` に書き換えれば ivy も不要になる。

---

## 現状評価

- 機能的には counsel/swiper と同等以上
- doom-dracula の consult フェイス対応は未実装（自前で上書き対応）
- embark は執筆スタイルには不要と判断
- company / iedit / imenu-list はそのまま継続使用
