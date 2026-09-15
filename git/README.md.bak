# git（世代管理）

> 作成日: 2026.07.31

dotfiles リポジトリ自身の commit / push / pull を自動化するための Makefile です。
リストア用のトップレベル `Makefile` とは関心事が異なるため、ここに分離しています。

## 運用方針

このリポジトリは複数マシン（P1: 親機 / X250: サブ機）で共有しており、
commit の粒度や書き方に手間をかけるより、変更を都度自動で記録して
世代管理そのものに専念する方針にしています。

- commit メッセージは `auto: 日時` の機械的な形式に統一
- 日々の作業経緯そのものは別途 changelog 等で記録し、git はあくまで
  「その時点のスナップショットに戻れること」を担保する道具として割り切る
- 過去の差分は [git-peek](https://github.com/minorugh/git-peek) や `tig` で
  自在に追えるため、commit メッセージに意味を持たせる必要性は薄いと判断
- magit での丁寧な commit 運用も試したが、こまめな commit を書く手間や
  操作を誤ったときの事故リスクを踏まえ、自動コミット一本に断捨離した

## ターゲット一覧

トップレベルの `Makefile` から `make git` 等でこのディレクトリのターゲットが
呼び出されます（`$(MAKE) -C git <target>` 形式のラッパー経由）。単体で
`cd git && make git` としても同じ動作をします。

| ターゲット | 内容 |
|---|---|
| `make git` | 変更を `add` → 差分があれば `auto: 日時` で commit → P1: push / サブ機: pull --rebase |
| `make git-fix` | サブ機で rebase に失敗した際の自動修復（abort → reset --hard → pull） |
| `make env-sync` | サブ機の `git pull` 後に呼ばれ、`~/.env_source`・abook の更新を検知して確認のうえ同期 |
| `make env-remount` | `dotfiles/env/` の bindfs マウントをやり直す（sudo不要）。`git`（P1）・`env-sync`（サブ機）から共通で呼ばれる |

### `make git` の分岐（ホスト名で判定）

- **P1（メイン機）**: `add` → `commit`（差分があれば） → `~/.env_source` 側の
  `make git` があれば呼び出し → `push`
- **サブ機（X250 等）**: push は行わず `pull --rebase` のみ。成功後に
  `make env-sync` を自動実行

サブ機は「親機と同じ環境を維持する持ち出し専用機」という位置づけのため、
誤って push してしまう事故を避ける目的で明示的に push を封鎖しています
（`init-sub` ターゲットで remote の push URL も無効化済み）。

### `env-sync` の挙動

`~/Dropbox/backup/env/env_repo.bundle.gpg`（秘密ファイル一式）と
`~/Dropbox/backup/abook/addressbook_*.gpg`（アドレス帳）の更新をハッシュ値・
ファイル名で検知し、変更があった場合のみ確認プロンプトを出したうえで同期します。
変更がなければ何もせず終了します。

`~/.env_source` は `rm -rf` せず `git fetch` + `git reset --hard` で更新するため、
ディレクトリ自体が消えて作り直されることはありません。ただし、内部の
ファイル構成が変化した際に `dotfiles/env/` の bindfs マウントが古い状態の
まま取り残され、中身が空に見える事象が実際に発生しています
（2026.08.03、詳細は `changelog-20260802.md` 参照）。そのため `env-sync`
は同期完了後に必ず `env-remount` を呼び、マウントを張り直します。

GPG のパスフレーズ入力が発生しうるため、Emacs 経由での実行時も
`bin/make-run.sh` 経由で `gnome-terminal` に委譲され、対話可能な状態で
実行されます（詳細は `bin/README.md` を参照）。

## 使い方

普段は何かを試したり修正を加えたタイミングで手動実行します。

```bash
cd ~/src/github.com/minorugh/dotfiles
make git
```

rebase に失敗した場合（サブ機のみ想定）：

```bash
make git-fix
```

## なぜトップレベルから分離したか

トップレベルの `Makefile` は「新しい環境にリストアする」ための、
公開しても他の人の参考になりうる汎用的なターゲット群です。一方で
このディレクトリの内容は「このリポジトリを日々どう運用しているか」という
個人の作業スタイルに強く依存する部分のため、関心事として切り離しています。
（`docker/Makefile` を分離した際と同じ考え方です）
