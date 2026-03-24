# .emacs.d

![Screenshot](https://live.staticflickr.com/65535/53032684552_3f0767459c_b.jpg) 

Emacs設定ファイル群。[leaf](https://github.com/conao3/leaf.el) + [init-loader](https://github.com/emacs-jp/init-loader) による分割管理。

## 環境

| 項目 | 内容 |
|---|---|
| Emacs | 30.1（自前ビルド） |
| OS | Debian 12 (Bookworm) |
| Machine | ThinkPad P1（メイン）/ ThinkPad X250（サブ） |
| 日本語入力 | mozc |
| フォント | Cica-18 |
| テーマ | doom-dracula |

## ファイル構成

```
.emacs.d/
├── early-init.el          # フレーム・GC・パッケージ初期化前の設定
├── init.el                # エントリポイント。leaf・init-loader を起動
├── init-mini.el           # ミニマム起動用（トラブルシューティング等）
├── makefile               # バイトコンパイル等のタスク
├── inits/                 # init-loader が番号順に読み込む設定ファイル群
│   ├── 00-base.el         # 基本設定・キーバインド・ユーティリティ関数
│   ├── 01-dashboard.el    # 起動画面（dashboard）
│   ├── 02-git.el          # magit・diff-hl・git-timemachine
│   ├── 03-evil.el         # evil-mode（view-mode 代替として使用）
│   ├── 04-counsel.el      # ivy / counsel / swiper / migemo
│   ├── 05-company.el      # 補完（company + prescient + yasnippet）
│   ├── 06-mozc.el         # 日本語入力（mozc）
│   ├── 07-highlight.el    # 表示強調（goggles・paren・web-mode 等）
│   ├── 08-dimmer.el       # 非アクティブウィンドウの減光
│   ├── 09-funcs.el        # compile・ps-print・gist などの関数定義
│   ├── 10-selected.el     # リージョン選択時のキーマップ
│   ├── 20-check.el        # flycheck・textlint・ispell
│   ├── 20-edit.el         # 編集補助（super-save・undo-fu・ediff 等）
│   ├── 30-ui.el           # 外観（doom-themes・modeline・行番号 等）
│   ├── 30-utils.el        # 汎用ツール（which-key・projectile・popwin 等）
│   ├── 40-hydra-dired.el  # hydra: クイックディレクトリナビゲーション
│   ├── 40-hydra-menu.el   # hydra: 作業メニュー（俳句・ブログ・印刷 等）
│   ├── 40-hydra-misc.el   # hydra: ブラウザ・パッケージ管理・Markdown
│   ├── 50-dired.el        # dired 設定（ls-lisp・sxiv 連携 等）
│   ├── 50-neotree.el      # neotree（ファイルツリー）
│   ├── 60-howm.el         # howm（Markdown メモ管理）
│   ├── 60-markdown.el     # markdown-mode・md2pdf・md2docx
│   ├── 60-org.el          # org-mode・org-capture・カレンダー
│   ├── 70-translate.el    # DeepL / Google Translate
│   ├── 70-yatex.el        # YaTeX（LaTeX 編集）
│   ├── 80-darkroom.el     # darkroom（執筆集中モード）
│   └── 90-easy-hugo.el    # easy-hugo（Hugo ブログ管理）
├── elisp/                 # 自作・非パッケージの elisp（README.md 参照）
├── snippets/              # yasnippet 用スニペット
└── tmp/                   # 履歴・キャッシュ類（Git 管理外）
    ├── custom.el
    ├── recentf
    ├── history
    ├── places
    ├── bookmarks
    ├── prescient-save
    ├── amx-items
    ├── scratch
    ├── tramp
    ├── undohist/
    ├── transient/
    └── projects
```

## 主要パッケージ

| カテゴリ | パッケージ |
|---|---|
| パッケージ管理 | leaf, leaf-keywords, hydra |
| 補完 | ivy, counsel, swiper, company, prescient, yasnippet |
| 検索 | migemo (cmigemo), avy, swiper-migemo |
| Git | magit, diff-hl, git-timemachine, browse-at-remote |
| 編集 | evil, evil-leader, undo-fu, undohist, expand-region, iedit |
| UI | doom-themes, doom-modeline, nerd-icons, dashboard, dimmer |
| 日本語 | mozc, mozc-cursor-color, mozc-popup |
| ファイル管理 | dired (ls-lisp), neotree, projectile, counsel-tramp |
| メモ・執筆 | howm, org, darkroom, easy-hugo, markdown-mode, YaTeX |
| 翻訳 | deepl-translate, google-translate |
| その他 | flycheck, which-key, selected, popwin, super-save, atomic-chrome |

## 設計方針

**GUIアプリ起動は `start-process` を使用**
`compile` や `shell-command` は Emacs をブロックするため、外部GUIアプリ（FileZilla・mozc_tool・thunderbird 等）の起動にはすべて `start-process` を使用する。

**tmpファイルは `tmp/` に集約**
履歴・キャッシュ・状態ファイルはすべて `(locate-user-emacs-file "tmp/...")` で `tmp/` に統一。`~/.emacs.d/` 直下を汚さない。

**`lexical-binding: t` を全ファイルで使用**
関数参照には `#'` を使用。`add-hook` の第2引数も `#'` で統一。

**evil は view-mode 代替として使用**
テキスト閲覧・カーソル移動を normal state、編集は emacs state（insert state を emacs state に alias）。

**hydra で作業メニューを集約**
`M-.` → `hydra-dired`（ディレクトリ操作）、`<henkan>` → `hydra-work`（俳句・ブログ作業）、`..` → `hydra-browse`（ブラウザ）で作業文脈ごとにまとめる。

## インストール

```bash
# dotfiles リポジトリから
git clone https://github.com/minorugh/dotfiles ~/src/github.com/minorugh/dotfiles
cd ~/src/github.com/minorugh/dotfiles
make emacs   # symlink を展開

# tmp/ ディレクトリを作成
mkdir -p ~/.emacs.d/tmp/undohist ~/.emacs.d/tmp/transient
```

Emacs 起動時に leaf が未インストールパッケージを自動インストールする。

## 関連リポジトリ

- [dotfiles](https://github.com/minorugh/dotfiles) — この設定を含む dotfiles 全体
- [mozc-cursor-color](https://github.com/minorugh/mozc-cursor-color) — mozc 入力状態に応じたカーソル色変更
- [deepl-translate](https://github.com/minorugh/deepl-translate) — DeepL API クライアント
- [Emacs Configuration](https://minorugh.github.io/emacs.d/) — 詳しい情報をまとめたウエブページ
