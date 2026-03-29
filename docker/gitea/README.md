# git-peek リポジトリ dual push 設定手順

## 前提
- GitHub にリポジトリ作成済み
- Gitea（Docker）にリポジトリ作成済み（Web UI: http://localhost:3000）

## 1. Xserver に bare リポジトリを作成
```bash
ssh minorugh@sv13268.xserver.jp -p 10022 \
  "mkdir -p ~/git/git-peek.git && \
   cd ~/git/git-peek.git && \
   git init --bare"
```

## 2. ローカルの .git/config に pushurl を追記
```ini
[remote "origin"]
    url = git@github.com:minorugh/git-peek.git
    pushurl = git@github.com:minorugh/git-peek.git
    pushurl = ssh://minorugh@sv13268.xserver.jp:10022/home/minorugh/git/git-peek.git
    pushurl = http://localhost:3000/minoru/git-peek.git
```

## 3. git push で3箇所に同時送信
```bash
git push
```

GitHub・Xserver・Gitea の3箇所に同時プッシュされる。

## 注意
- Xserver は bare リポジトリを手動作成しないと push 時にエラーになる
- Gitea は Web UI でリポジトリを先に作成しておくこと
- GitHub は pushurl に含めないと fetch 元と push 先がずれるので必ず記載すること

