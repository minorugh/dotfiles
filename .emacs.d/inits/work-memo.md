# Emacs設定ファイル修正作業メモ
2026-03-13

## 修正完了ファイル

| ファイル | 主な変更点 |
|---|---|
| early-init.el | `native-comp-jit-compilation nil` 削除、typo修正 |
| init.el | GC二重設定削除、`project-list-file`追加、`custom-file`パス修正、`kill-emacs-hook`から early-init/init 自身のコンパイル除外 |
| 00-base.el | `defalias 'exit` → `my:exit`、tmpパスを `locate-user-emacs-file` に統一 |
| 01-dashboard.el | `system-name` 使用、`shell-command-to-string` 1回化、`show-week-agenda-p` → `dashboard-week-agenda`、`:after dashboard` 削除 |
| 02-git.el | `gitk-open` を `start-process` に、`transient-history-file` 統一、`:custom-face` 修正 |
| 03-evil.el | `:hook` 明示、`if` → `when`、`#'` 統一、`thunderbird`/`neomutt`/`mattermost` を `start-process` に、`:init`/`:config` 順序整理 |
| 04-counsel.el | `ivy-rich-mode` 明示、`amx-save-file` 統一、`swiper-region` 整理、`swiper-migemo` に `:ensure nil` 追加 |
| 05-company.el | `prescient-save-file` 統一、`ivy-prescient`/`company-prescient` を `:after` で独立ブロックに |
| 06-mozc.el | `evil-mode` 確認を `(and (boundp ...) evil-mode)` に、GUIツール3関数を `start-process` に |
| 07-highlight.el | `:custom-face` 二重クォート修正、`rainbow-mode` の `after-init-hook` 削除、`web-mode` 正規表現修正 |
| 08-dimmer.el | 正規表現をリスト形式に分割、`my:dimmer-mode` → `my:dimmer-enabled` に改名 |
| 09-selected.el | `:init`/`:config` 順序整理、`add-hook` の `#'` 統一 |
| 10-check.el | typo修正、`:hook` 明示、`eval-and-compile` 削除、`ispell` の `with-eval-after-load` 削除 |
| 20-edit.el | `super-save` の `:hook` 明示、`undohist-directory` を `locate-user-emacs-file` に統一 |
| 20-funcs.el | **★ `compile-autoclose` ロジックバグ修正**。`leaf` を使わず直接 `setq` で記述（`compilation`・`ps-mule` は組み込みfeature名のためleafブロック名に使うと変数衝突エラー） |
| 30-ui.el | `:custom-face` 二重クォート修正、`:hook` 明示、`display-fill-column-indicator` 名前修正 |
| 30-utils.el | `toggle-scratch-prev-buffer` に `defvar` 追加、tmpパス統一、不要クォート削除 |
| 40-hydra-dired.el | `fzilla-*`/`keepassxc` を `start-process` に、`xmodmap` パスを `~/` に変更 |
| 41-hydra-menu.el | **★ `"e"` キー重複解消**（`easy-hugo` 優先）、`xsrv-gh` を `start-process` に、旧コード削除 |
| 40-hydra-misc.el | `slack` を `start-process` に、`"0"`/`"2"` の URL 重複を整理 |
| 50-dired.el | **★ `ls-lisp` と `--group-directories-first` 不整合修正**（`ls-lisp-dirs-first t` に変更）、正規表現修正 |
| 50-neotree.el | `call-interactively` を削除 |
| 60-howm.el | `:hook` 明示、`my:howm-create-*` を共通関数 `my:howm-create-note` に整理 |
| 60-markdown.el | **★ `md2pdf` 修正**（スコープ外変数参照バグ解消）、`:custom-face` 修正 |
| 60-org.el | `:bind` 二重括弧修正、`interactive` 削除、`:hook` のクォート削除 |
| 70-translate.el | 空 `:doc` 補完、`google-translate-auto` のネスト整理、`#'` 統一、`:ensure nil` 追加 |
| 70-yatex.el | `yatexprc` に `:ensure nil` 追加、`M-c`/`M-v` を `yatex-mode-map` に限定 |
| 80-darkroom.el | **★ `my:darkroom-out` の `diff-hl-mode 0` → `1` に修正** |
| 90-easy-hugo.el | `request` を独立ブロックに分離、`request-storage-directory` 重複解消、`locate-user-emacs-file` 統一 |

★ = 高優先度バグ修正

## 未作成

- Makefile（`rm -f` 変更、`inits/`/`elisp/` 対象追加、`compile` ターゲット追加）

## 横断的に適用した改善方針

- `:custom-face` の二重クォート → `((t (...)))` に統一
- `if` → `when`/`unless`（else節なし）
- tmpファイルパス → `locate-user-emacs-file` に統一
- GUIアプリ起動 → `compile`/`shell-command` → `start-process` に統一
- `add-hook` の `'` → `#'`（lexical-binding環境）
- `:hook` の省略形 → `(hook . command)` 形式に明示

## 教訓：leafブロック名に使えない名前

Emacs組み込みのfeature名（`compilation`・`ps-mule` 等）を `(leaf NAME ...)` に使うと
`:ensure nil` の有無に関わらず「Symbol's value as variable is void」エラーになる。
該当する設定はleafを使わず直接 `setq`/`defun` で書く。
