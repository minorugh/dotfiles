# etc（システム設定の原本置き場）

> 作成日: 2026.10.17

システムの `/etc` に置く設定ファイルの原本を、dotfiles リポジトリ内で管理するためのディレクトリです。
`/etc` は root 権限が必要で `$HOME` の外にあるため、原本をここに置き、
トップレベル `Makefile` がリストア時に `/etc` 側へ反映します。

## 反映の仕組み

- 原本を編集すれば、symlink 方式のファイルはシステム設定にそのまま反映されます
- 同時に Git で履歴管理されます（`make git`）
- `etc/` の中身は Makefile が自動生成するものではなく、手で置いてコミットしたものです

## ファイル一覧

| ファイル | 反映先 | 方法 | Makefile 行 |
|---|---|---|---|
| `fuse.conf` | `/etc/fuse.conf` | symlink | 59 |
| `lightdm/lightdm.conf` | `/etc/lightdm/lightdm.conf` | symlink | 106 |
| `systemd/logind.conf` | `/etc/systemd/logind.conf` | symlink | 107 |
| `default/grub` | `/etc/default/grub` | symlink | 108 |
| `tlp.conf` | `/etc/tlp.conf` | symlink | 201 |
| `udev/rules.d/99-keychron-hub-wakeup.rules` | `/etc/udev/rules.d/` | symlink | 185 |
| `gnome-terminal/dracula.dconf` | dconf に読み込み | `dconf load` | 380 |
| `systemd/README.md` | なし | 経緯メモ | - |

行番号は 2026.10.17 時点のものです。Makefile を編集した場合はずれることがあります。

`dracula.dconf` は `/etc` には置かれず、`etc/` を置き場所として使っているだけです。

## etc/ に原本を持たないもの

- `/etc/udev/rules.d/50-keychron.rules`: Keychron Launcher が書き換える実体のため、原本は
  Dropbox（`~/Dropbox/backup/keychron/`）に置き、コピー方式で反映（Makefile 76行目）。
  夜間バックアップ（`keychron-rules-backup`）で Dropbox 側を更新
- `/etc/default/keyboard`: `sed` で `XKBOPTIONS` を書き換え（Makefile 100行目）
- `/etc/cups/cups-browsed.conf`: 1行を追記（Makefile 297行目）
- Docker 用の apt リポジトリと polkit ルール: 実行時に生成（Makefile 420〜441行目）

## 廃止したもの

| 日付 | 対象 | 理由 |
|---|---|---|
| 2026.07.08 | `keyd/default.conf` | CapsLock→Ctrl を XKB 方式（`make keymap`）に統一 |
| 2026.08.05 | `systemd/` の Dropbox 関連サービス一式 | スリープ復帰対策を試行したが、同日中に全て取りやめ |
| 2026.10.17 | `default/tlp` | TLP 1.4 以降は `/etc/tlp.conf` が正式。旧ファイルは読まれない |
| 2026.10.17 | `NetworkManager/NetworkManager.conf` | `/etc` 側が dotfiles へのリンクではなく、未使用だった |
