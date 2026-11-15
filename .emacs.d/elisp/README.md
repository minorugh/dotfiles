# ~/.emacs.d/elisp/
---

## ディレクトリ


### bin/
elisp や外部から呼び出すスクリプト群を収録。

- **gen_toc.pl** --- Markdown ファイルの目次（TOC）を自動生成。`markdown-utils.el` から呼び出している。
- **howm-fix-code-comments.pl** --- Markdown コードブロック内のコメント記号を変換。`markdown-utils.el` から呼び出している。
- **sen_cleanup.pl** --- 選句データの没句削除。`evil-leader-tools.el` から呼び出している。
- **tile-focus-toggle.sh** --- Simplenote と Emacs のフォーカスをトグル切り替えする。
- **xsrv-backup-smart.sh** --- xserver からローカルへ rsync バックアップ。変更がなければスキップする。

### css/
`markdown-preview` 用の CSS を複数収録。設定変更で切り替えられるようにしてある。

---

## 自作 elisp

Emacs の load-path を通しているディレクトリ。

### commit-msg.el
tig からの commit・reword 用のメッセージバッファ（`*commit-msg*`）。
dotfiles はサブ機から commit できないようにするガードと、`make prep` の実行を含む。
`C-c C-g` で Claude 用プロンプトをクリップボードにコピーし、返事を自動で挿入する。
`02-git.el` の leaf で、tig からの入口が最初に呼ばれたときに遅延ロードする。

### deepl-translate.el
DeepL API で翻訳し、結果をミニバッファに表示してクリップボードにコピーする。
`deepl-translate`（日英を自動判定）、`deepl-ej`、`deepl-je` を提供する。
`C-c d`、および選択範囲での `d` にバインドしている。API キーは外部の非公開ファイルから読み込む。

### dired-permission-help.el
Dired の `P` で、パーミッションの早見表（`r`=4、`w`=2、`x`=1 など）を右サイドバーの `*Permission Help*` に表示する。

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
`60-markdown.el` の leaf で読み込む。

### post-template.el
俳句関係の作業ファイルをヘッダー自動生成付きで開くテンプレート関数群。
`80-hydra-navication.el` の hydra で、`:body-pre` の `(require 'post-template)` により読み込む。
