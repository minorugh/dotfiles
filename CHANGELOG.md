# CHANGELOG

README.md の更新履歴から移した過去の履歴です（新しい順）。

| 日付 | 内容 |
|---|---|
| 2026.07.31 | git/env-sync/git-fix を git/Makefile に分離、トップレベルはラッパー化（リストア用と日常運用用の関心事を分離、git/README.md 新設） |
| 2026.07.31 | env-sync を導入・本番運用確定（サブ機の git pull連動で ~/.env_source・abook の差分を検知し確認のうえ同期。git ターゲットを ##! 化し、Emacs経由の実行でも対話プロンプトが機能するよう修正）。baseinstall/nextinstall の記載漏れを解消（make-run・tig・hugo を追加）。neomutt-bin を廃止し neomutt ターゲットに統合。「対話実行系」セクションの5ターゲットを ##! 化 |
| 2026.07.30 | Emacsからのmake実行を安全化する bin/make-run.sh を導入（##!付きターゲットのみgnome-terminalへ委譲、結果は*compilation-log*バッファへ自動反映）。bin/スクリプトのMakefileターゲットを整理し tile-toggle.sh を新規登録（従来未登録だった）、keepass.sh を keepassxc.sh にリネーム |
| 2026.07.29 | 秘密ファイル管理（dotfiles/env/ → ~/.env_source）をシンボリックリンクから bindfs バインドマウントに移行、新規ファイルの自動反映に対応。env_local を gpg_bundle_key にリネーム |
| 2026.07.10 | Thunderbird 廃止（neomutt+Gmail Webへ移行）、make thunderbird ターゲット・autostart自動起動・thunderbird-backup 一式を削除 |
| 2026.07.08 | keyd を廃止し xmodmap に一本化（CapsLock/PrtSc等のキー変換を keymap ターゲットに集約）、.xprofile の xmodmap 再適用ウォッチャーを廃止（cron毎分実行＋Emacs手動リロードに統一） |
| 2026.06.16 | xsrv-backup を cron に移行、xsrv-systemd ターゲット廃止、cron-stop/cron-start 追加 |
| 2026.06.07 | melpa-backup.sh 追加・autobackup ターゲットに登録、ELPAパッケージ変更ログ蓄積対応 |
| 2026.05.25 | power-menu.sh の Sleep を xset dpms force off に変更（cron スリープ対策）、anacron-backup.sh 追加 |
| 2026.04.29 | git-crypt 廃止・秘密ファイルを ~/.env_source で管理、env-setup ターゲット追加 |
| 2026.04.11 | devilspie 廃止・Emacs/Thunderbird 自動起動を .autostart.sh に統合、xdotool で最小化 |
| 2026.04.10 | emacs-restore を emacs-toggle に変更（F12キーによるEmacs最小化・復元トグル） |
| 2026.04.07 | xsrv-backup を systemd-user timer に移行、xsrv-systemd ターゲット追加、cron/Makefile 緊急操作パネル整備 |
| 2026.03.26 | keyring ターゲットを全機共通コピー方式に統一、autostart.sh の条件分岐を削除 |
| 2026.03.21 | cron ターゲット追加（automerge/autobackup/crontab 管理）、README 全体見直し |
| 2026.03.19 | polkit ターゲット追加（Docker認証ダイアログ抑制、docker-setup に統合） |
| 2026.03.11 | リストア手順を HTTPS clone 対応に修正、SSH 切り替え手順を追加 |
| 2026.03.10 | SSH/keychain 環境を X250 サブ機に対応、autostart.sh・keyring 周りを整理 |
| 2025.03.09 | Debian12 対応クリーンアップ、sxiv→nsxiv 移行メモ追加 |
| 2024.10.01 | Debian12 対応 |
| 2022.09.22 | Debian11 対応 |
| 2021.11.01 | xserver へのリモートリポジトリ追加（同時 Push 対応） |
| 2021.10.11 | 内容整理 |
| 2021.08.26 | Debian11 / Emacs 27.2 対応 |
| 2021.02.20 | Emacs 27.1 対応 |
| 2021.01.29 | mozc 修正 |
| 2021.01.28 | ThinkPad 2台共有対応 |
| 2020.11.10 | 再構築 |
| 2020.10.27 | 初回コミット |
