# バックアップ再構築レジュメ
作成: 2026-03-14

---

## 背景・動機

- 従来の `~/Dropbox/makefile` による Xserver rsync バックアップが
  keychain 環境変数展開の問題でエラーを起こし続けている
- GH（3GB超）の丸ごと圧縮転送は処理が重く非効率
- Xserver に 300GB 中 50GB 未満しか使っておらず、容量は十分
- 設計を根本から見直し、**git bare リポジトリ方式**に移行する

---

## 対象リポジトリと現状

| 対象 | GitHub | 備考 |
|------|--------|------|
| dotfiles | Public | `~/src/github.com/minorugh/dotfiles` |
| gpgimport | Private | GPG パスワードキー |
| GH | Private | 俳句サイト管理ファイル群（3GB近い） |
| minorugh.com | Private | ブログサイト管理ファイル群 |
| melpa | Dropbox のみ | 8世代ローテーション ← **現行維持** |

---

## 新方式の方針

### コアアイデア: git push を Xserver bare リポジトリへ同時送信

`git push` 一回で GitHub と Xserver の両方に push する。
`~/.git/config` の `pushurl` を2行書くだけで実現できる。

```ini
[remote "origin"]
    url = git@github.com:minorugh/dotfiles.git
    pushurl = git@github.com:minorugh/dotfiles.git
    pushurl = minorugh@sv*.xserver.jp:~/git/dotfiles.git
```

### メリット

- rsync / tar / keychain 不要 → エラーの温床がなくなる
- 差分転送なので GH（3GB）も初回以外は軽い
- Xserver 側でも `git log`、`git checkout` が使える
- makefile が大幅にシンプルになる

---

## 移行手順（チェックリスト）

### Phase 1: Xserver に bare リポジトリを作成

対象: dotfiles, GH, minorugh.com（gpgimport は後回しでも可）

```bash
# Xserver 上で実行（ssh xsrv で入って作業）
mkdir -p ~/git
git init --bare ~/git/dotfiles.git
git init --bare ~/git/GH.git
git init --bare ~/git/minorugh.com.git
```

- [ ] dotfiles.git 作成
- [ ] GH.git 作成
- [ ] minorugh.com.git 作成

### Phase 2: 各ローカルリポジトリの .git/config に pushurl 追記

```bash
# dotfiles の例
cd ~/src/github.com/minorugh/dotfiles
git remote set-url --add --push origin git@github.com:minorugh/dotfiles.git
git remote set-url --add --push origin minorugh@sv*.xserver.jp:~/git/dotfiles.git

# 確認
git remote -v
```

同様に GH、minorugh.com にも適用する。

- [ ] dotfiles に pushurl 追記・確認
- [ ] GH に pushurl 追記・確認
- [ ] minorugh.com に pushurl 追記・確認

> **注意**: `git remote set-url --add --push` は既存の fetch url を
> 自動的に push の1本目には含めないので、GitHub 分も明示的に追加すること。

### Phase 3: 動作確認

```bash
# dotfiles でテスト push
cd ~/src/github.com/minorugh/dotfiles
touch .pushtest && git add .pushtest && git commit -m "test: pushurl dual config"
git push

# Xserver 側に届いているか確認
ssh xsrv "git -C ~/git/dotfiles.git log --oneline -3"

# テストコミットは削除
git revert HEAD --no-edit && git push
```

- [ ] dotfiles テスト push 成功
- [ ] GH テスト push 成功
- [ ] minorugh.com テスト push 成功

### Phase 4: makefile の整理

不要になるターゲット: `dotfiles`、`gh`

新しい `all` ターゲット:

```makefile
all: melpa git-push

## melpa は現行維持（8世代ローテーション）
melpa:
	rm -rf ${HOME}/Dropbox/backup/emacs/elpa/`ls -rt ${HOME}/Dropbox/backup/emacs/elpa | head -n 1`; \
	tar cfz ${HOME}/Dropbox/backup/emacs/elpa/`date '+%Y%m%d%H%M%S'`.tar.gz -C ${HOME}/.emacs.d elpa

## 各リポジトリの日次 auto-commit + push（pushurl で Xserver にも同時送信）
git-push:
	$(MAKE) -C ${HOME}/src/github.com/minorugh/dotfiles git
	$(MAKE) -C ${HOME}/Dropbox/GH git
	$(MAKE) -C ${HOME}/Dropbox/minorugh.com git
```

各リポジトリの Makefile `git` ターゲット（現行と同じ構造でよい）:

```makefile
git:
	git add -A
	git diff --cached --quiet || git commit -m "auto: `date '+%Y-%m-%d'`"
	git push
```

- [ ] makefile から `dotfiles`/`gh` ターゲット削除
- [ ] `git-push` ターゲット追加・動作確認
- [ ] cron 実行テスト（`make -f ~/Dropbox/makefile` を手動で一度通す）

### Phase 5: Xserver 旧バックアップの後始末

しばらく新方式が安定稼働したら旧ディレクトリを削除。

```bash
ssh xsrv "ls ~/backup/"   # 現状確認
ssh xsrv "rm -rf ~/backup/dotfiles ~/backup/GH"
```

- [ ] 新方式安定確認（1週間程度）
- [ ] 旧 rsync バックアップディレクトリ削除

---

## Xserver side の git リポジトリ参照方法

Xserver は GUI がないが、ローカルから操作できる。

```bash
# ログ確認
ssh xsrv "git -C ~/git/GH.git log --oneline -10"

# 特定ファイルを取り出す（bare なので直接）
ssh xsrv "git -C ~/git/GH.git show HEAD:path/to/file"

# 必要なら一時的に clone して中身を見る
ssh xsrv "git clone ~/git/GH.git /tmp/GH-check"
```

---

## Phase 6（任意）: Gitea でローカル GUI 閲覧環境を構築

### 動機・位置づけ

Xserver bare リポジトリはコマンドラインで十分参照できるが、
以下のような用途ではブラウザ GUI があると格段に楽になる。

- しばらく作業しなかった後にファイル構成を素早く把握したい
- 特定コミットのファイルをダウンロードしたい
- コミット履歴・差分をビジュアルに閲覧したい
- 「いざというとき」に CLI に頼らず操作したい

Mattermost と同じ docker-compose 運用で立てられる。

### Gitea を選ぶ理由

| ツール | 重さ | 備考 |
|--------|------|------|
| **Gitea** | 軽量（〜50MB RAM） | Go製・シングルバイナリ。**推奨** |
| Forgejo | 軽量（同等） | Gitea の community fork。どちらでも可 |
| GitLab CE | 重量級（2〜4GB RAM） | 個人用途には過剰 |
| GitBucket | 中程度（JVM） | メモリ多め。機能は十分だが Gitea より重い |

### docker-compose.yml（最小構成）

`~/docker/gitea/docker-compose.yml` として配置する。

```yaml
services:
  gitea:
    image: gitea/gitea:latest
    container_name: gitea
    restart: unless-stopped
    ports:
      - "3000:3000"
      - "2222:22"
    volumes:
      - ./gitea-data:/data
    environment:
      - USER_UID=1000
      - USER_GID=1000
```

```bash
cd ~/docker/gitea
docker compose up -d
```

初回は `http://localhost:3000` にアクセスして初期設定ウィザードを完了させる。
（DB は SQLite でよい。個人用途なら十分。）

### Gitea へのリポジトリ登録

Phase 2 で各リポジトリに pushurl を追加した要領で、Gitea にも追加する。

```bash
# Gitea に空リポジトリを作成（Web UI または API）した後
git remote set-url --add --push origin http://localhost:3000/minoru/dotfiles.git

# 確認：3つの push 先が並ぶ
git remote -v
# origin  git@github.com:minorugh/dotfiles.git (fetch)
# origin  git@github.com:minorugh/dotfiles.git (push)
# origin  minorugh@sv*.xserver.jp:~/git/dotfiles.git (push)
# origin  http://localhost:3000/minoru/dotfiles.git (push)
```

以後の `git push` で GitHub・Xserver・Gitea の3箇所に同時送信される。

> **注意**: Gitea は P1 がシャットダウン中は参照できない。
> 「常時参照」が必要なら Xserver bare git + SSH が引き続きメイン。
> Gitea はあくまで「手元で快適に見るための補助 GUI」と位置づける。

### Mattermost との共存

既存の Mattermost と同一の docker-compose ファイルにまとめることもできる。
あるいは別ディレクトリで独立して管理する方が起動・停止の制御がしやすい。

```bash
# 独立管理の場合（推奨）
~/docker/mattermost/docker-compose.yml   # 既存
~/docker/gitea/docker-compose.yml        # 新規追加
```

### チェックリスト

- [ ] `~/docker/gitea/` ディレクトリ作成・docker-compose.yml 配置
- [ ] `docker compose up -d` で起動確認
- [ ] `http://localhost:3000` 初期設定完了（SQLite・admin ユーザー作成）
- [ ] dotfiles リポジトリを Gitea に登録・push 確認
- [ ] GH リポジトリを Gitea に登録・push 確認
- [ ] Web UI でコミット履歴・ファイル閲覧・ZIP ダウンロード動作確認

---

## 懸案事項

### GitHub Private の容量制限

- 現状は無制限に近いが将来のリスクとして認識しておく
- Xserver bare git が「保険の副リポジトリ」として機能するので
  GitHub が有料化・制限されてもローカル + Xserver で継続可能

### gpgimport の扱い

- GPG 秘密鍵を含む Private リポジトリ
- Xserver に bare リポジトリを置くことへのセキュリティ上の懸念があれば
  GitHub Private のみ継続でもよい
- Xserver は自分のサーバなので問題ないと判断するなら他と同様に追加可能

---

## 次セッションの開始点

Phase 1 の Xserver bare リポジトリ作成から着手する。
Xserver のホスト名（`sv*.xserver.jp` の実際の値）を確認してから作業に入ること。

```bash
# ホスト名確認
ssh xsrv "hostname"
cat ~/.ssh/config | grep -A3 xsrv
```

---

## git log --oneline --graph 学習メモ

Xserver bare リポジトリ追加後、両 remote への push 確認や履歴閲覧に使う。

### 参考リソース

- **公式日本語ドキュメント（Pro Git Book）**
  https://git-scm.com/book/ja/v2/Git-の基本-コミット履歴の閲覧
  `--pretty=format` との組み合わせまで体系的に解説

- **グラフ記号の読み方（note.com）**
  https://note.com/yukikkoaimanabi/n/nf468008fb157
  `*` `|` `/` `\` の意味、`HEAD ->` `origin/x` タグの意味を図解

- **alias 設定まで含む解説（rfs.jp）**
  https://rfs.jp/server/git/02git/git-log.html
  カラー付きフォーマットを `~/.gitconfig` の `[alias]` に `lg` として登録する手順

### 今回の構成で使うコマンド

```bash
# ローカルから：両 remote に push できているか確認
git log --oneline --graph --decorate --all

# Xserver bare リポジトリのログをローカルから確認
ssh xsrv "git -C ~/git/GH.git log --oneline --graph -10"
```

### .gitconfig alias 候補

```ini
[alias]
    lg = log --graph --oneline --decorate --all
    lga = log --graph --oneline --decorate --all --date=short \
          --format="%C(yellow)%h%C(reset) %C(green)%ad%C(reset) %s%C(red)%d%C(reset)"
```

`git lg` 一発でブランチ・リモート・HEAD の全体像が見える。

---

## Magit 中級メモ

現状: commit / push / pull は使えるが、merge conflict や rebase で躓くことがある。

### merge conflict が起きたとき

**`e` → `a` か `b` → `q` → stage → continue** の流れが基本。

1. magit-status の unmerged item の上で `e` → ediff が3バッファで開く
2. `n`/`p` で conflict 箇所を移動、`a`/`b` でどちらを採用するか選ぶ
3. `q` で ediff を抜ける
4. 解決済みファイルをステージして merge を続行

### rebase 中に止まったときのキー

| 状況 | キー |
|------|------|
| conflict 解決後に続行 | `r r` |
| この commit だけスキップ | `r s` |
| rebase を全部やめて元に戻す | `r a` |

pushurl が2つになると、どちらかで conflict が起きる可能性がある。
その場合は `r a` で一旦中断して状況を確認する。

### 参考リソース

- **Magit 公式マニュアル — Rebasing**
  https://docs.magit.vc/magit/Rebasing.html

- **Magit rebase チュートリアル（Guowei Lv）**
  https://www.lvguowei.me/post/magit-rebase-2/
  commit 修正・squash から conflict 解消まで、手元に置いて参照するメモとして使える

- **System Crafters — Interactive Rebase（動画付き）**
  https://systemcrafters.net/mastering-git-with-magit/using-interactive-rebase/
  rebase 前に `b n` でバックアップブランチを作る習慣と、
  失敗時に `X h` でハードリセットして戻す安全網の作り方

---

*このレジュメは再構築完了後に CHANGELOG として記録する*
