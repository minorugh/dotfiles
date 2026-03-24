# ~/.emacs.d/elisp/

Emacs の load-path を通しているディレクトリ。init.el で以下を設定することで、
このディレクトリ以下のサブディレクトリも再帰的に load-path に追加される。

```elisp
(let ((default-directory "~/.emacs.d/elisp/"))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))
```

これにより各 leaf ブロックで `:load-path` を個別指定する必要がない。

---

## ディレクトリ

### emacs-logo/
ダッシュボード用のロゴ画像を複数収録。気分転換に差し替えて使う。

### markdown-css/
`markdown-preview` 用の CSS を複数収録。設定変更で切り替えられるようにしてある。

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

## プライベート設定 elisp

### my:dired.el
hydra-dired メニューから呼び出すディレクトリ・ファイルオープン関数群。
`40-hydra-dired.el` から `(require 'my:dired)` で読み込む。

### my:template.el
俳句関係の作業ファイルをヘッダー自動生成付きで開くテンプレート関数群。
`40-hydra-menu.el` から `(require 'my:template)` で読み込む。
