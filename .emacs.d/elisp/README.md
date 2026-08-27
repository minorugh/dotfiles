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

### elpa-time-machine.el
elpa バックアップ（rsync + git 管理）の過去スナップショットをサイドバーで閲覧するツール。
コミット一覧から選択してプレビューし、必要なものを `~/tmp/` に保存できる。
Claude との共同開発。

### git-peek.el
git 管理下のファイルの過去バージョンを ivy で選択し、左右分割のサイドバーUIで
プレビューしながら保存するツール。以前は GitHub リポジトリを `package-vc-install`
で直接読み込んでいたが、メンテナンス負担を考慮して elisp 直置き + autoload に移行。
GitHub リポジトリは公開用として維持し、`make publish` で同期している。
`my-tig-bridge.el` と連携し、tig から探したコミットをそのまま開ける。
Claude との共同開発。

### deepl-translate.el
DeepL API を使ったリージョン翻訳。ミニバッファに結果を表示し、kill-ring にコピーする。
`deepl-translate` は日本語⇔英語を自動判定、`deepl-ej`/`deepl-je` は方向を明示指定。
API キーは `80-translate.el` から外部の untracked ファイル経由で設定する。

### gcal-dashboard-widget.el
Google Calendar(複数可)を org ファイルへ一方向同期し、dashboard.el の Agenda ウィジェットに
`gcal-agenda` として表示する。同期は洗い替え方式(一時ファイルへ書き出し、全カレンダー成功後に
本番ファイルへ反映)なので、途中でエラーやタイムアウトが起きても本番ファイルは壊れない。
`kill-emacs-hook` で終了時に自動同期されるほか、`M-x gcal-dashboard-sync` で手動実行もできる。

### evil-cheat-sheet.el
Evil キーバインドのチートシートを表示するバッファー。
ノーマルステートの `?` にバインドし、ivy でセクションジャンプできる。

### my-markdown.el
Markdown・howm ファイル編集用ユーティリティ関数群。
コードブロック内コメント変換（`my-howm-fix-code-comments`）や
目次生成（`gen_toc.pl` 呼び出し）などを提供する。
`(require 'my-markdown)` で読み込む。

### my-sen-cleanup.el
選句作業用コマンド。`sen_cleanup.pl` を非同期実行し、結果をストリーミング表示する。
実行前に `.tmp` バックアップを作成し、`;b` で復元できる。
キーバインドは `my-normal-leader-map` の `;c` / `;b`。

### my-template.el
俳句関係の作業ファイルをヘッダー自動生成付きで開くテンプレート関数群。
`40-hydra-dired.el` および `40-hydra-menu.el` から `(require 'my-template)` で読み込む。

### tig-git-peek-bridge.el
tig と git-peek を連携させるブリッジ。tig 起動時にファイルパスをコンテキストファイルに書き出し、
tig 側から `emacsclient` 経由で `git-peek-from-hash` を呼べるようにする。
`~/.tigrc` に `bind generic E` の設定が必要。

### haiku-dashboard-wiget.el
青畝俳句データ（366 日分）を定数として保持する elisp ファイル。
`seihohaiku.cgi` から自動変換生成したもの。

### tempbuf.el
未使用バッファをバックグラウンドで自動 kill するマイナーモード。
EmacsWiki 限定配布で `package-vc-install` できないため、自分の GitHub に
fork した上でこのディレクトリに直接配置（原作: Michele Bini, 2001年〜）。
無シャットダウン運用でバッファが溜まり続けるため必須。

---

## 公開用パッケージの同期

git-peek / gcal-dashboard-widget / deepl-translate / tempbuf の4ファイルは
elisp/ が正のソース。編集後 `make publish` で各自の GitHub リポジトリ
(`~/src/github.com/minorugh/<pkg>/`) へコピー・push する。elc は対象外
（各リポジトリ側は `.gitignore` で `*.elc` を除外）。
