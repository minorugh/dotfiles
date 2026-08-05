# etc/systemd/

## user/dropbox-sleep-watch.service

サスペンド復帰時にDropbox同期が止まったままになる問題への対策。

`org.freedesktop.login1.Manager` の `PrepareForSleep` D-Busシグナルを
ユーザーセッション内で監視し、復帰（`false`）を検知したら
`dropbox stop; dropbox start -i` を実行して同期を再開させる。

- 監視スクリプト本体: `bin/dropbox-sleep-watch.sh`
- 導入: `make dropbox-resume-watch`（トップレベルMakefile）
- ログ: `~/.cache/dropbox-resume.log`
- root権限・`su`・新規ログインセッション作成を一切使わない
  （`systemctl --user` のみで完結する常駐サービス）

## logind.conf

メイン機（P1）のみ `make grub` で `/etc/systemd/logind.conf` に配置。
サスペンドしない運用のための設定。

## 廃止したもの（2026.08.05）

以下は同日中に試して問題が出たため撤去済み。経緯は
`changelog-20260805.md` を参照。

- `system-sleep/dropbox-resume`（root権限のsystem-sleepフック。
  復帰処理と競合してフリーズを引き起こした）
- `user/dropbox.service`（Dropboxデーモン自体をsystemd管理する試み。
  GUI autostartとの二重起動やアイコン非表示等の副作用が出たため撤去）
