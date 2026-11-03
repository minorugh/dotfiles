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
| `lightdm/lightdm.conf` | `/etc/lightdm/lightdm.conf` | symlink | 120 |
| `systemd/logind.conf` | `/etc/systemd/logind.conf` | symlink | 121 |
| `default/grub` | `/etc/default/grub` | symlink | 122 |
| `tlp.conf` | `/etc/tlp.conf` | symlink | 195 |
| `gnome-terminal/dracula.dconf` | dconf に読み込み | `dconf load` | 377 |
| `systemd/README.md` | なし | 経緯メモ | - |

行番号は 2026.11.03 時点のものです。Makefile を編集した場合はずれることがあります。

`dracula.dconf` は `/etc` には置かれず、`etc/` を置き場所として使っているだけです。

### default/grub

- `GRUB_DISABLE_OS_PROBER=false`：Debian 13 へのアップグレード後、別 SSD の Windows が起動メニューから消えたため、os-prober を有効にしている
- `usbcore.autosuspend=-1`：USB の自動サスペンドを切る。Keychron の試行錯誤で入れたもの。今はキーボードの電源を外部モニタから取っているため、不要かもしれない
- 変更は etc/default/grub を編集し、`sudo update-grub` で反映する。`sed -i` は symlink を普通のファイルに置き換えるので使わない

## etc/ に原本を持たないもの

- `/etc/udev/rules.d/50-keychron.rules`: Keychron Launcher が書き換える実体のため、原本は
  Dropbox（`~/Dropbox/backup/keychron/`）に置き、コピー方式で反映（Makefile 75行目）。
  夜間バックアップ（`keychron-rules-backup`）で Dropbox 側を更新
- `/etc/default/keyboard`: `sed` で `XKBOPTIONS` を書き換え（Makefile 114行目）
- `/etc/cups/cups-browsed.conf`: 1行を追記（Makefile 294〜295行目）
- Docker 用の apt リポジトリと polkit ルール: 実行時に生成（Makefile 418〜439行目）

## 廃止したもの

| 日付 | 対象 | 理由 |
|---|---|---|
| 2026.07.08 | `keyd/default.conf` | CapsLock→Ctrl を XKB 方式（`make keymap`）に統一 |
| 2026.08.05 | `systemd/` の Dropbox 関連サービス一式 | スリープ復帰対策を試行したが、同日中に全て取りやめ |
| 2026.10.17 | `default/tlp` | TLP 1.4 以降は `/etc/tlp.conf` が正式。旧ファイルは読まれない |
| 2026.10.17 | `NetworkManager/NetworkManager.conf` | `/etc` 側が dotfiles へのリンクではなく、未使用だった |
