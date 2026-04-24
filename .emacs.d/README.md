# ~/.emacs.d/elisp/

Emacs の load-path を通しているディレクトリ。init.el で以下を設定することで、
このディレクトリ以下のサブディレクトリも再帰的に load-path に追加される。

```elisp
(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
(load "~/.emacs.d/elisp/my-loaddefs.el" t t)
```

これにより各 leaf ブロックで `:load-path` を個別指定する必要がない。
また `my-loaddefs.el` により自作関数が autoload 登録され、起動時のロードを遅延できる。

---

## ディレクトリ

### emacs-logo/
ダッシュボード用のロゴ画像を複数収録。気分転換に差し替えて使う。

### markdown-css/
`markdown-preview` 用の CSS を複数収録。設定変更で切り替えられるようにしてある。

nsult-
### deepl-translate/
### key-chord/
### mozc-cursor-color/
### sequential-command/
### tempbuf/

上記はいずれも元々 VC（package-vc-install）で作者リポジトリから直接読み込んでいたもの。
VC 管理を廃止し、このディレクトリに配置して load-path で読み込む方式に統一した。
必要に応じて一部カスタマイズして使用している。

`deepl-translate` については DeepL の API 仕様変更により本家のコードが動作しなくなったため、
API 呼び出し部分を修正して使用している。

---

## スクリプト

### gen_toc.pl
Markdown ファイルの目次（TOC）を自動生成する Perl スクリプト。
`markdown-mode` の設定内から呼び出している。

### howm-fix-code-comments.pl
Markdown コードブロック内のコメント記号を変換する Perl スクリプト。
`markdown-mode` の設定内から呼び出している。

---

## 自作 elisp（autoload 管理）

このディレクトリの `my:*.el` は全て autoload で遅延ロードする方式に統一している。
`require` は使わない。関数を呼び出したタイミングで初めてファイルがロードされる。

### Makefile
`my:*.el` から `my-loaddefs.el` を生成・更新するための Makefile。

```bash
make          # my-loaddefs.el を再生成（上書き）
make clean    # my-loaddefs.el を削除
```

新しい `my:*.el` を追加したり既存の関数を変更したりしたら `make` を実行する。

### my-loaddefs.el
`make` によって自動生成される autoload 定義ファイル。**手で編集しない**。

### my:dired.el
hydra-dired メニューから呼び出すディレクトリ・ファイルオープン関数群。

### my:github.el
GitHub 関連のユーティリティ関数群（CHANGELOG への自動挿入など）。

### my:git-show-file.el
過去のコミットからファイルを取り出して `~/Dropbox/backup/tmp/` に保存する関数。
ivy で対話的にファイルとコミットを選択できる。

### my:markdown.el
howm/Markdown ファイル用のユーティリティ関数群（コードブロック内コメント変換、TOC 生成など）。

### my:template.el
俳句関係の作業ファイルをヘッダー自動生成付きで開くテンプレート関数群。

---

## 自作 elisp の書き方ルール

新しい `my:*.el` を作るときは必ず以下の3点セットを守る。

1. ファイル先頭に `lexical-binding: t` を宣言する
2. 公開する関数の直前に `;;;###autoload` を書く
3. ファイル末尾に `(provide 'my:xxx)` を書く

詳細は `autoload-howto.md` を参照。


## inits ファイルへの Commentary 記載

関連する自作関数がある場合、inits ファイルの `;;; Commentary:` に以下の形式で記載しておくことを推奨する。
```emacs-lisp
;;; Commentary:
;;
;; Related custom functions (~/.emacs.d/elisp/my_xxx.el):
;;   `関数名' - 説明
;;
;; These are autoload-registered via my-loaddefs.el.
;;; Code:
```
