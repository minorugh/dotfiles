# git（世代管理）

> 作成日: 2026.07.31 / 最終更新: 2026.11.10

dotfiles リポジトリ自身の commit 前処理 / push / pull を扱う Makefile です。
リストア用のトップレベル `Makefile` とは関心事が異なるため、ここに分離しています。

## 運用方針

このリポジトリは複数マシン（P1: 親機 / X250・Let's note: サブ機）で共有しており、
commit と push を分けて運用しています。

- **commit は Emacs 側で行う**。tig の status（`Ctrl_r` で開く）から `C`（ファイル単位）・`A`（全変更）で
  `*commit-msg*` を開き、メッセージを書いて確定する。空のまま確定すると `auto: 日時` になる
- **push は `make git`**（tig の status から `P`）。メイン機は未コミットの変更があると警告して止まり、
  自動では commit しない
- **夜は NIGHT SUSPEND**（power-menu の `4`）が `make nightly` を呼び、全変更を auto commit して
  push してから suspend する。押さずに寝た夜は push されないが、変更は P1 に残り、
  次回の NIGHT SUSPEND か tig の `P` で出す
- **サブ機で pull する前は、メイン機で push する**
- 朝のフォールバック（`autobackup.sh --check`）では dotfiles を push しない。
  作業中の編集を auto commit しないため
- 日々の作業経緯は別途 changelog に記録し、過去の差分は
  [git-peek](https://github.com/minorugh/git-peek) や `tig` で追う

## ターゲット一覧

トップレベルの `Makefile` から `make git` 等でこのディレクトリのターゲットが
呼び出されます（`$(MAKE) -C git <target>` 形式のラッパー経由）。単体で
`cd git && make git` としても同じ動作をします。

| ターゲット | 内容 |
|---|---|
| `make git` | P1: 未コミットがなければ push（あれば警告して止まる） / サブ機: pull --ff-only |
| `make prep` | P1 の commit 前処理。`env-backup` と `changelog-rotate.sh`。Emacs の「すべて commit」（tig の `A`）から呼ばれる |
| `make env-backup` | `~/.env_source` の auto commit と bundle 更新、`git/env.hash` の更新（P1） |
| `make nightly` | P1: `prep` のあと全変更を `auto: 日時` で commit して push。NIGHT SUSPEND から呼ばれる |
| `make git-fix` | サブ機で pull（ff-only）に失敗した際の自動修復（rebase 中なら abort → `reset --hard origin/main` → pull）。dotfiles 専用。P1 では実行を止める |
| `make env-sync` | サブ機の `git pull` 後に呼ばれ、`~/.env_source` の更新を検知して確認のうえ同期（abookも統合済みのため自動的に含まれる） |

### `make git` の分岐（ホスト名で判定）

- **P1（メイン機）**:
  1. `env-backup` を実行する（`~/.env_source` 側の `make git` があれば呼び出し、bundle のハッシュを `git/env.hash` に書き出す）
  2. `git/env.hash` が変わっていれば、そのファイルだけを `auto: env.hash 日時` で commit する
  3. 未コミットの変更があれば、`##> ✗ 未コミットの変更があります。先に commit してください` と
     `git status --short` を出して終わる。終了コードは 0（Emacs の compile バッファにエラー表示を出さないため。
     push されたかどうかは、`##> ✗` の行と `##> ✓ push 完了` の行で見分ける）
  4. なければ未 push の一覧を出して `push` し、`##> ✓ push 完了` を出す
- **サブ機（X250・Let's note）**: push は行わず pull のみ。未commit の変更（未追跡を含む）は
  `git stash` で退避し、`git pull --ff-only` のあと、今回 stash を作ったときだけ
  `stash pop` する。成功後に `make env-sync` を自動実行
  - 履歴が分岐している、または独自コミットがある場合、`--ff-only` はエラーで止まる
    （書き換えない）。`git log origin/main..HEAD` で確認し、必要なら `make git-fix`
  - `stash pop` が競合した場合は警告を出す（stash は残る）

サブ機は「親機と同じ環境を維持する持ち出し専用機」という位置づけのため、
誤って push してしまう事故を避ける目的で明示的に push を封鎖しています
（`init-sub` ターゲットで remote の push URL も無効化済み）。

（2026.10.01訂正: 以前は、ホスト名が `archlinux`（Let's note）の場合に
`arch-install` リポジトリの commit/push を連鎖して実行していましたが、
`arch-install` リポジトリの廃止（内容は `arch-debian-restore` のサブディレクトリへ
移行）に伴い、この分岐を削除しました。Let's note も他のサブ機と同じく pull のみで、
push しません。）

### `make prep` と `make nightly`

`prep` は P1 の commit 前処理です。`env-backup` で `~/.env_source` の bundle と
`git/env.hash` を更新し、`bin/changelog-rotate.sh` で更新履歴を `CHANGELOG.md`（全履歴）に
反映します（トップ `README.md` は直近10件に保つ）。`env.hash` は commit に含めるため、
`prep` は commit の前に実行します。Emacs の「すべて commit」（tig の `A`）が先にこれを呼びます。

`nightly` は P1 だけの夜間用ターゲットです。`prep` のあと `git add -A` して、
差分があれば `auto: 日時` で commit し、push します。サブ機では何もしません。

呼び出し元は power-menu の NIGHT SUSPEND（`4`）です。成功したら suspend し、
失敗したら suspend せずに Enter 待ちで止まります。以前は夜間の `autobackup` の
`git-push` から呼んでいましたが、2026-11-10 に外しました。端末で
`make -C ~/src/github.com/minorugh/dotfiles/git nightly` と手動で実行することもできます。

### `env-sync` の挙動

`~/Dropbox/backup/env/env.bundle.gpg`（秘密ファイル一式。abookもここに
統合済み）の更新をハッシュ値で検知し、変更があった場合のみ確認プロンプトを
出したうえで同期します。変更がなければ何もせず終了します。

判定の基準は、git 管理下の `git/env.hash` です（P1 が bundle の生成直後に書き出し、
dotfiles の push で配布）。サブ機の Dropbox 側の同期が追いついていない場合は、
「同期待ち」と表示して終了します（Dropbox の同期遅延による誤検知を避けるため）。

（2026.09.15追記: 以前はabookを`addressbook_*.gpg`という個別ファイルで
別経路バックアップ・同期していましたが、abook自体が`~/.env_source`に
git統合されたため廃止しました。abookの更新は`env_source`全体の同期に
自動的に含まれます。）

`~/.env_source` は `rm -rf` せず `git fetch` + `git reset --hard` で更新するため、
ディレクトリ自体が消えて作り直されることはありません。

GPG のパスフレーズ入力が発生しうるため、Emacs 経由での実行時も
`bin/make-run.sh` 経由で `gnome-terminal` に委譲され、対話可能な状態で
実行されます（詳細は `bin/README.md` を参照）。

## 使い方

P1（メイン機）の普段の流れ:

1. `Ctrl_r` で tig の status を開く
2. `C`（ファイル単位）または `A`（全変更）で commit する
3. `P` で push する（端末から `make git` でも同じ）
4. 寝る前に power-menu の `4`（NIGHT SUSPEND）で、取りこぼしを auto commit して push し、suspend する

tig の `P` は、未コミットの変更があるとメッセージを出して止まります。`q` で status に戻ります。

サブ機は、メイン機で push したあとに `make git`（pull）を実行します。

```bash
cd ~/src/github.com/minorugh/dotfiles
make git
```

pull（`--ff-only`）に失敗した場合（サブ機のみ想定）：

```bash
make git-fix
```

`git-fix` は `reset --hard origin/main` を使うため、サブ機の独自コミットと未commit の
変更は破棄されます（stash に退避済みのものは残る）。Dropbox で共有しているリポジトリ
（GH・minorugh.com・arch-debian-restore）には使わないでください。作業ファイルが
Dropbox 経由で P1 にも波及します。

## なぜトップレベルから分離したか

トップレベルの `Makefile` は「新しい環境にリストアする」ための、
公開しても他の人の参考になりうる汎用的なターゲット群です。一方で
このディレクトリの内容は「このリポジトリを日々どう運用しているか」という
個人の作業スタイルに強く依存する部分のため、関心事として切り離しています。
（`docker/Makefile` を分離した際と同じ考え方です）
