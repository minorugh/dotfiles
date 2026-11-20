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

### dashboard-gcal-widget.el
`gcal-sync.el` が生成する gcal.org を、dashboard の Agenda ウィジェットに表示する設定。
表示日数・曜日表示・ソート順・当日分のフィルタ補正を持つ。
`01-dashboard.el` の dashboard の leaf から require する。

### dashboard-haiku-widget.el
青畝の句 366 日分のデータを持ち、今日の日付の句を dashboard に表示する。
`01-dashboard.el` の `dashboard-insert-haiku` から、最初に呼ばれたときに require する。

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
実行前に `.tmp` バックアップを作成し、`;r` で復元できる。
キーバインドは `my-normal-leader-map` の `;c` / `;b`。

### gcal-sync.el
Google カレンダーの秘密 iCal URL（複数可）から予定を取得し、`~/.emacs.d/tmp/gcal.org` に一方向同期する。
終了時（`kill-emacs-hook`）と `M-x gcal-sync` で同期する。URL は外部の非公開ファイルから読み込む。
`dashboard-gcal-widget.el` と `99-calendar.el` から使う共通部品で、gcal.org の形式は冒頭の Commentary に書いてある。
注意：`99-calendar.el` では `my-calendar-gcal-dates` の中で require する。ファイル先頭で require すると calendar が init の途中で読み込まれ、RET の割り当てが効かなくなる。

### markdown-utils.el
Markdown・howm ファイル編集用ユーティリティ関数群。
コードブロック内コメント変換（`my-howm-fix-code-comments`）や
目次生成（`gen_toc.pl` 呼び出し）などを提供する。
`60-markdown.el` の leaf で読み込む。

### my-csv-mode.el
CSV / TSV の列揃え専用の最小メジャーモード（`my-csv-mode`、`my-tsv-mode`）。
モード起動時と保存時に、オーバーレイで列幅を揃えて表示する。バッファの内容は変更しない。
`#` で始まる行はコメントとして扱う。

### my-php-mode.el
PHP のシンタックスハイライト専用の最小メジャーモード。
キーワード・型・定数・`$変数`・関数名・クラス名を色付けする。`.php` に自動で割り当てる。

### post-template.el
俳句関係の作業ファイルをヘッダー自動生成付きで開くテンプレート関数群。
`80-hydra-navication.el` の hydra で、`:body-pre` の `(require 'post-template)` により読み込む。
