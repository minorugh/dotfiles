# 自作 Emacs Lisp を autoload 化する手順書

## 概要

自作の便利関数を `require` でロードすると、Emacs 起動時に全ファイルが読み込まれる。
`autoload` に切り替えると、**関数を呼び出すまでロードが発生しない**ため起動時間を短縮できる。

---

## ファイルの書き方ルール（3点セット）

新しい `.el` ファイルを作るときは必ず以下を守る。

### 1. ファイル先頭に `lexical-binding` を宣言

```emacs-lisp
;;; my:foo.el --- 説明 -*- lexical-binding: t -*-
```

### 2. 公開したい関数の直前に `;;;###autoload` を書く

```emacs-lisp
;;;###autoload
(defun my:foo ()
  "Do something."
  (interactive)
  ...)
```

`interactive` な関数は全部つける。内部ヘルパーにはつけない。

### 3. ファイル末尾に `provide` を書く

```emacs-lisp
(provide 'my:foo)
;;; my:foo.el ends here
```

---

## ファイルの置き場所

```
~/.emacs.d/elisp/my_foo.el
~/.emacs.d/elisp/my_bar.el
...
~/.emacs.d/elisp/Makefile          ← make で loaddefs を再生成
~/.emacs.d/elisp/my-loaddefs.el    ← 自動生成（手で触らない）
```

---

## Makefile の使い方

### できること

| コマンド | 動作 |
|---|---|
| `make` | `my-loaddefs.el` を再生成（上書き） |
| `make clean` | `my-loaddefs.el` を削除 |

### 仕組み

`make` を実行すると、バックグラウンドで Emacs が起動して `loaddefs-generate` を自動実行する。
`my_*.el` に変更がなければ「最新」と判断してスキップするため、**毎回叩いても問題ない**。

`*scratch*` で `loaddefs-generate` を手打ちする必要はない。ターミナルから `make` だけでよい。

### 上書きの挙動

`my-loaddefs.el` が既に存在していても**そのまま上書き再生成**する。
手で編集してはいけない（`make` のたびに消える）。

---

## my-loaddefs.el の生成・更新

### 初回 or ファイルを追加・変更したとき

```bash
cd ~/.emacs.d/elisp
make
```

これだけ。`my-loaddefs.el` が自動生成（または更新）される。

### やり直したいとき

```bash
make clean && make
```

---

## init.el の設定

以下が**すでに書かれていれば追加設定は1行だけ**。

```emacs-lisp
;; init-loader の :init ブロック内（既存）
(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
(load "~/.emacs.d/elisp/my-loaddefs.el" t t)  ; ← この1行を追加
```

---

## inits 群の既存 `require` を削除する

autoload に切り替えたファイルの `require` を残すと意味がなくなる。

```emacs-lisp
;; 削除する例
(require 'my:dired)
(require 'my:template)
(require 'my:github)
```

これらは全て削除して OK。関数は呼び出し時に自動ロードされる。

---

## 日常の運用フロー

| やること | コマンド |
|---|---|
| 新しい `.el` を追加した | `cd ~/.emacs.d/elisp && make` |
| 既存の関数を変更した | `cd ~/.emacs.d/elisp && make` |
| loaddefs を消してやり直し | `cd ~/.emacs.d/elisp && make clean && make` |

---

## チェックリスト

- [ ] `lexical-binding: t` がヘッダーにある
- [ ] 全 `interactive` 関数の直前に `;;;###autoload` がある
- [ ] ファイル末尾に `(provide 'my:xxx)` がある
- [ ] `~/.emacs.d/elisp/` に配置した
- [ ] `make` で `my-loaddefs.el` を再生成した
- [ ] inits 群の対応する `require` を削除した
