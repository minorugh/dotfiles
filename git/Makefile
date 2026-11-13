########################################################
## git 世代管理（メイン機: push のみ、commit は Emacs 側 / サブ機: pull only）
########################################################
# 個人の日々の作業運用のためのターゲット群。
# リストア用のトップレベル Makefile とは切り離して管理する。
# Main: push only (commit is done in Emacs: 85-git.el) / others (sub): pull --ff-only only
#
# 2026.08.03
# .env_source 側の構造変化で dotfiles/env が空になることがあるのでメイン機側の実行コードに remount処理を追加した。
# 経緯の詳細は changelog-20260802.md 参照。
#
# 2026.08.04
# env-sync の変更判定が「サブ機ローカルのDropbox同期状態」に依存しており、Dropboxの同期遅延で
# 誤って「変更なし」と表示される問題があったため、判定基準を git 管理下の git/env.hash（メイン機が
# bundle生成直後に書き出し、dotfiles push経由で即時配布）に変更。Dropbox側がまだ追いついていない
# 場合は「同期待ち」と明示し、誤検知を無くした。
#
# 2026.09.08
# サブ機側のpull前処理を「git add -A + auto commit」から「git stash」方式に変更。
# 前者だと、サブ機で試しに書いただけの編集（コミットする意図のない変更）まで
# 次のmake git実行時に問答無用でコミットされてしまい、pushしないためローカルに
# 気づかれずコミットが蓄積、後日のpullで正規の変更とマージされて重複定義等の
# 原因不明な不具合につながっていた。stash方式なら作業ツリーに戻るだけでコミット
# 履歴を汚さない。
#
# 2026.09.15
# abookが~/.env_sourceにgit統合されたため、env-syncの abook_diff（addressbook_*.gpg
# を個別に探して同期する処理）を廃止。abookの更新はenv_source全体の同期（env_diff）
# に自動的に含まれるため、別経路での同期は不要かつ二重管理の原因になっていた。
# env.bundle.gpg のパス表記ゆれ（arch-restore / arch-debian-restore 等）も
# ~/Dropbox/backup/env/env.bundle.gpg に統一。
#
# 2026.09.30
# サブ機の pull 処理を2点修正。
# (1) git stash pop を「今回 stash を作った時だけ」実行する。従来は無条件だったため、作業ツリーが
#     きれいで stash を作らなかった場合に、過去の失敗で残った古い stash が pop されて復活していた。
# (2) pull --rebase を pull --ff-only に変更。サブ機は push しないので独自コミットは持たない前提。
#     --rebase だと古い自動コミットが再適用されて混ざる（2026.09.08 の不具合と同じ型）。
#     --ff-only なら、履歴が分岐していれば書き換えずにエラーで止まる。
#     あわせて stash pop の競合を警告する（従来は競合しても || true で成功扱いのまま env-sync へ進んでいた）。
#
# 2026.10.01
# archlinux 用の分岐（arch-install の commit/push を呼ぶ処理）を削除。arch-install リポジトリは
# 廃止され、現在は arch-debian-restore のサブディレクトリにある（そちらはサブ機では何もしない）。
# 残すと、存在しないリポジトリへの push が失敗（Repository not found）して make git が異常終了する。
#
# 2026.10.15
# env/ の bindfs 失効対策。再起動やプロセス死亡でマウントが外れると env/ が空に見える。
# env-check を新設し、make git の冒頭（メイン/サブ共通）で自動検知・復旧する。正常時は無音。
# 判定: ~/.env_source が非空なのに、env/ が非マウントまたは空なら失効とみなして再マウント。
# env-remount は、mountpoint で拾えない失効マウント（/proc/mounts に残る状態）も掃除する。
# 従来メイン機の push 前に毎回実行していた env-remount 呼び出しは、env-check に一本化して削除。

# 2026.11.06
# env/ の bindfs 窓を廃止したため、env-remount と env-check を削除した。~/.env_source を直接参照する。

# 2026.11.07
# commit と push を分離した。commit は Emacs 側（85-git.el のメッセージバッファ）で行い、
# auto commit と commit ターゲットは廃止した。
# prep: メイン機の commit 前処理（env_source の bundle 更新、env.hash 更新、changelog-rotate）。
#       Emacs の「すべて commit」から呼ばれる。env.hash は commit に含めるため push 側では更新しない。
# git:  メイン機は未コミットの変更があれば警告して止め、なければ未 push の一覧を表示して push する。
#       サブ機は従来どおり pull。Emacs の ] はこのターゲットを直接呼ぶ。
# git-fix: 未 push のコミットが消えるため、メイン機では実行を止めるようにした（サブ機専用）。

# 2026.11.09
# commit と push の分離で、env_source の auto commit と bundle が「すべて commit」（prep）のときだけ
# 実行されるようになり、1ファイルだけの commit のあとに push しても Dropbox の bundle が更新されなかった。
# prep の env_source 処理を env-backup に切り出し、git（メイン機の push）の冒頭でも実行するようにした。
# env.hash が変わったときは、そのファイルだけを auto commit してから push する。
# nightly を追加した。夜間の autobackup（backup/Makefile の git-push）から呼ばれ、メイン機で prep のあと
# 全変更を auto commit して push する。日中の git の動き（未 commit は警告して止まる）は変えていない。

# 2026.11.10
# nightly の呼び出し元を autobackup（backup/Makefile の git-push）から power-menu の NIGHT SUSPEND に移した。
# dotfiles の auto commit と push は、寝る前に NIGHT SUSPEND を押したときだけ走る。
# 押さずに寝た夜は push されないが、変更は P1 に残り、次回の NIGHT SUSPEND か tig の P で出る。
# サブ機で pull する前は、メイン機で push する運用で補う。
# 朝のフォールバックでは dotfiles を push しない（作業中の編集を auto commit しないため）。

# 2026.11.13
# nightly を git-auto に改名した。呼び出し元が power-menu の NIGHT SUSPEND だけでなく、
# Emacs の my-make-git（dotfiles のメイン機のとき）にも増え、時間帯を表す名前が実態に合わなくなったため。
# 動作は変えていない。git は push のみ、git-auto は prep のあと全変更を auto commit して push する。

# NOTE: 将来メイン機を変更した時はここを書き換えること。
MAIN_HOSTNAME  := P1
HOSTNAME       := $(shell hostname)

DOTFILES_DIR   := $(HOME)/src/github.com/minorugh/dotfiles
ENV_SOURCE_DIR := $(HOME)/.env_source
ENV_SYNC_CACHE := $(HOME)/.cache/env-sync
ENV_BUNDLE     := $(HOME)/Dropbox/backup/env/env.bundle.gpg

.PHONY: git prep git-fix env-sync env-backup git-auto

git: ## Push (main only, sub: pull only)
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	@$(MAKE) -s env-backup
	@cd $(DOTFILES_DIR) && \
		if ! git diff --quiet git/env.hash; then \
			git commit -q -m "auto: env.hash $$(date '+%Y-%m-%d %H:%M:%S')" -- git/env.hash; \
		fi
	@cd $(DOTFILES_DIR) && \
		if [ -n "$$(git status --porcelain)" ]; then \
			echo "##> ✗ 未コミットの変更があります。先に commit してください"; \
			git status --short; \
		else \
			git log --oneline origin/main..HEAD && \
			git push && \
			echo "##> ✓ push 完了"; \
		fi
else
	@echo "$(HOSTNAME): サブ機からはpushしません（pullのみ）"
	@cd $(DOTFILES_DIR) && \
		before=$$(git stash list | wc -l); \
		git stash --include-untracked -q; \
		after=$$(git stash list | wc -l); \
		git pull --ff-only || { echo "エラー: pull(ff-only)に失敗しました。独自コミットがあるか履歴が分岐しています。'git log origin/main..HEAD' で確認し、必要なら 'make git-fix' を実行してください（未commitの変更は stash に退避されたままです: git stash list）"; exit 1; }; \
		if [ "$$after" -gt "$$before" ]; then \
			git stash pop -q || echo "警告: stash pop で競合しました。'git status' と 'git stash list' を確認してください（stash は残っています）"; \
		fi
	@$(MAKE) -s -C $(DOTFILES_DIR)/git env-sync
endif

env-backup: ## env_source の auto commit と bundle、env.hash の更新（メイン機）
	if [ -f $(ENV_SOURCE_DIR)/Makefile ]; then \
		$(MAKE) -s -C $(ENV_SOURCE_DIR) git; \
	fi
	sha256sum $(ENV_BUNDLE) 2>/dev/null | awk '{print $$1}' > $(DOTFILES_DIR)/git/env.hash

prep: ## Prepare for commit (main only)
	@$(MAKE) -s env-backup
	-@$(DOTFILES_DIR)/bin/changelog-rotate.sh

git-auto: ## Auto commit all changes and push (main only)
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	@$(MAKE) -s prep
	@cd $(DOTFILES_DIR) && \
		git add -A && \
		{ git diff --cached --quiet || git commit -q -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"; } && \
		git push
	@echo "##> ✓ git-auto 完了"
else
	@echo "$(HOSTNAME): サブ機では実行しません"
endif

env-sync: ## env_source の更新を検知し、あれば確認のうえ同期（サブ機のgit pull連動用）
	@mkdir -p $(ENV_SYNC_CACHE); \
	env_hash_flag=$$(cat $(DOTFILES_DIR)/git/env.hash 2>/dev/null); \
	env_hash_old=$$(cat $(ENV_SYNC_CACHE)/env.hash 2>/dev/null); \
	env_hash_local=$$(sha256sum $(ENV_BUNDLE) 2>/dev/null | awk '{print $$1}'); \
	env_diff=false; env_pending=false; \
	if [ -n "$$env_hash_flag" ] && [ "$$env_hash_flag" != "$$env_hash_old" ]; then \
		if [ "$$env_hash_flag" = "$$env_hash_local" ]; then \
			env_diff=true; \
		else \
			env_pending=true; \
		fi; \
	fi; \
	if [ "$$env_diff" = false ] && [ "$$env_pending" = false ]; then \
		echo "##> ✓ pull 完了 env_source に変更なし"; \
		exit 0; \
	fi; \
	if [ "$$env_diff" = false ]; then \
		echo "##> ⏳ env_source の更新を検知しましたが、Dropbox側の同期待ちです。しばらくしてから 'make git' を再実行してください"; \
		exit 0; \
	fi; \
	echo ">>> env_source の更新を検知しました (abookも含む)"; \
	read -p "同期しますか? [y/N]: " ans; \
	if [ "$$ans" != "y" ] && [ "$$ans" != "Y" ]; then \
		echo "##> スキップしました"; \
		exit 0; \
	fi; \
	gpg -d $(ENV_BUNDLE) > /tmp/env_repo.bundle; \
	if [ -d $(ENV_SOURCE_DIR)/.git ]; then \
		head_ref=$$(git bundle list-heads /tmp/env_repo.bundle | head -1 | awk '{print $$2}'); \
		cd $(ENV_SOURCE_DIR) && \
		git fetch /tmp/env_repo.bundle "$$head_ref:refs/heads/__incoming__" && \
		git reset --hard __incoming__ && \
		git branch -D __incoming__; \
	else \
		git clone /tmp/env_repo.bundle $(ENV_SOURCE_DIR); \
	fi; \
	rm /tmp/env_repo.bundle; \
	echo "$$env_hash_flag" > $(ENV_SYNC_CACHE)/env.hash; \
	echo "##> ✓ env_source 同期完了 (abook含む)"

git-fix: ## rebase失敗時の自動修復（サブ機のみ）
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	@echo "$(HOSTNAME): メイン機では実行しません（未pushのコミットが消えます）"
	@exit 1
else
	cd $(DOTFILES_DIR) && git rebase --abort 2>/dev/null || true
	cd $(DOTFILES_DIR) && git reset --hard origin/main
	cd $(DOTFILES_DIR) && git pull
endif
