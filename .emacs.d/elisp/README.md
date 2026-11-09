# ~/.emacs.d/elisp/
---

## ディレクトリ


### bin/
elisp や外部から呼び出すスクリプト群を収録。

- **gen_toc.pl** --- Markdown ファイルの目次（TOC）を自動生成。`my-markdown.el` から呼び出している。
- **howm-fix-code-comments.pl** --- Markdown コードブロック内のコメント記号を変換。`my-markdown.el` から呼び出している。
- **sen_cleanup.pl** --- 選句データの没句削除。`my-sen-cleanup.el` から呼び出している。
- **tile-focus-toggle.sh** --- Simplenote と Emacs のフォーカスをトグル切り替えする。
- **xsrv-backup-smart.sh** --- xserver からローカルへ rsync バックアップ。変更がなければスキップする。

### css/
`markdown-preview` 用の CSS を複数収録。設定変更で切り替えられるようにしてある。


### img/
ダッシュボード用のロゴ画像を複数収録。気分転換に差し替えて使う。

---

## 自作 elisp

Emacs の load-path を通しているディレクトリ。

### evil-cheat-sheet.el
Evil キーバインドのチートシートを表示するバッファー。
ノーマルステートの `?` にバインドし、ivy でセクションジャンプできる。

### evil-leader-tools.el
選句作業用コマンド。`sen_cleanup.pl` を非同期実行し、結果をストリーミング表示する。
実行前に `.tmp` バックアップを作成し、`;b` で復元できる。
キーバインドは `my-normal-leader-map` の `;c` / `;b`。

### markdown-utils.el
Markdown・howm ファイル編集用ユーティリティ関数群。
コードブロック内コメント変換（`my-howm-fix-code-comments`）や
目次生成（`gen_toc.pl` 呼び出し）などを提供する。
`(require 'my-markdown)` で読み込む。

### insert-template.el
俳句関係の作業ファイルをヘッダー自動生成付きで開くテンプレート関数群。
`80-hydra-navication.el` から `(require 'my-template)` で読み込む。


