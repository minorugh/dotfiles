# dotfiles git運用改善 作業計画書

## 目標

- サブ機（X250）からのgit pushを封鎖する
- メイン機・サブ機ともにMakefile一発でgit操作を完結させる
- magitと永遠におさらばする

---

## 前提確認

- メイン機ホスト名：`p1g`（要確認：`hostname` コマンドで確認）
- サブ機ホスト名：`x250`（要確認）
- dotfiles Makefileの場所：`~/src/github.com/minorugh/dotfiles/Makefile`

---

## Step 1：ホスト名を確認する（両機で実施）

```bash
hostname
```

メモしておく。これがMakefile分岐のキーになる。

---

## Step 2：サブ機のgit push URLを封鎖する（X250で実施）

```bash
cd ~/src/github.com/minorugh/dotfiles
git remote set-url --push origin no-push
```

確認：

```bash
git remote -v
```

こう表示されればOK：

```
origin  git@github.com:minorugh/dotfiles.git (fetch)
origin  no-push (push)
```

これでサブ機から誤って `git push` しても物理的に止まる。**保険として設定するだけで、Makefileとは独立した話。**

---

## Step 3：dotfiles MakefileのgitターゲットをMakefile全体で確認（メイン機で実施）

現在の `git:` ターゲットがどう書かれているかを確認する。

```bash
grep -n "git" ~/src/github.com/minorugh/dotfiles/Makefile
```

---

## Step 4：dotfiles Makefileのgitターゲットを書き換える（メイン機で実施）

現在の `git:` ターゲットをhostname分岐版に書き換える。

```makefile
HOSTNAME := $(shell hostname)

git:
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),p1g)
	git push
else
	@echo "$(HOSTNAME): サブ機からはpushしません（pullのみ）"
	git pull --rebase
endif
```

`p1g` の部分はStep 1で確認した実際のホスト名に変える。

---

## Step 5：各作業ディレクトリのMakefileも同様に整備する

dotfiles傘下の各作業ディレクトリのMakefileに同じパターンを展開する。

```makefile
HOSTNAME := $(shell hostname)

git:
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),p1g)
	git push
else
	@echo "$(HOSTNAME): サブ機からはpushしません"
	git pull --rebase
endif
```

---

## Step 6：動作確認

**メイン機（P1）で：**

```bash
make git
```

→ commit + push されること

**サブ機（X250）で：**

```bash
make git
```

→ "サブ機からはpushしません" と表示されてpullだけ実行されること

さらに念押しで：

```bash
git push
```

→ `fatal: 'no-push' does not appear to be a git repository` で止まること

---

## まとめ

| やること | どこで | 目的 |
|---|---|---|
| hostname確認 | 両機 | 分岐キーの確認 |
| push URL封鎖 | X250のみ | 物理的な保険 |
| Makefile書き換え | P1（dotfiles） | hostname分岐追加 |
| 各作業Makefile整備 | P1 | 同じパターンを展開 |
| 動作確認 | 両機 | 意図通りか検証 |

Step 1〜2 は今すぐできる準備作業です。Step 3以降はStep 1のホスト名確認後に進めましょう。


## Step 2 補足：`.git/config` はなぜサブ機だけに効くのか

### dotfilesで共有されるものとされないもの

dotfilesのgit pushで共有されるのは `.zshrc` や `Makefile` などの
ファイル本体だけです。`.git/` ディレクトリの中身はGitHubに存在せず、
git pushもgit pullの対象にもなりません。

### `.git/config` はいつ誰が作るのか

各マシンで最初に `git clone` したときに、そのマシン上に自動で作られます。

- P1でcloneしたとき → P1の `.git/config` が作られる
- X250でcloneしたとき → X250の `.git/config` が作られる

つまり両機の `.git/config` は最初からずっと別物です。
```
GitHub上          P1上                     X250上
（リモート）       （ローカル）               （ローカル）

dotfiles.git  →  dotfiles/.git/config     dotfiles/.git/config
（.gitなし）      （P1専用・独立）           （X250専用・独立）
```

### だからStep 2はX250だけで一度やれば永続する

`git remote set-url --push origin no-push` はX250の `.git/config` に
`pushurl = no-push` を書き込むだけです。P1には何も影響しません。
再起動しても消えません。新規セットアップ時だけ必要です。


### 新規セットアップ時の注意

`.git/config` はdotfilesで共有されないため、X250を新規セットアップしたときは
手動で設定が必要です。リストア用Makefileに以下を書いておくと漏れを防げます。
```makefile
init-sub:
	git -C ~/src/github.com/minorugh/dotfiles remote set-url --push origin no-push
	@echo "サブ機push封鎖完了"
```

## Step 7：リストア用Makefileに init-sub ターゲットを追加する（メイン機で実施）

リストア用Makefileの `all:` ターゲットに `init-sub` を追加し、
内容を以下のように書く。
```makefile
HOSTNAME := $(shell hostname)

init-sub:
ifeq ($(HOSTNAME),x250)
	git -C ~/src/github.com/minorugh/dotfiles remote set-url --push origin no-push
	@echo "サブ機push封鎖完了"
else
	@echo "これはサブ機専用の処理です"
endif
```

追加したらdotfilesをgit pushして両機に反映する。
