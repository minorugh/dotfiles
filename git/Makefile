########################################################
## git 世代管理（メイン機: push のみ、commit は Emacs 側 / サブ機: pull only）
########################################################
# 個人の日々の作業運用のためのターゲット群。
# リストア用のトップレベル Makefile とは切り離して管理する。
# 変更履歴は CHANGELOG.md、運用方針は README.md を参照。
#
# メイン機: commit は Emacs / tig で行い、push は git で行う。
#   auto-commit: prep のあと全変更を auto commit する（push しない。hydra-dired の [）
#   git:         未コミットがなければ push する（my-make-git、tig の P）
#   git-auto:    auto-commit のあと push する（power-menu の NIGHT SUSPEND）
# サブ機: git は pull --ff-only のみ。push は封鎖している。

# NOTE: 将来メイン機を変更した時はここを書き換えること。
MAIN_HOSTNAME  := P1
HOSTNAME       := $(shell hostname)

DOTFILES_DIR   := $(HOME)/src/github.com/minorugh/dotfiles
ENV_SOURCE_DIR := $(HOME)/.env_source
ENV_SYNC_CACHE := $(HOME)/.cache/env-sync
ENV_BUNDLE     := $(HOME)/Dropbox/backup/env/env.bundle.gpg

.PHONY: git prep git-fix env-sync env-backup auto-commit git-auto

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

auto-commit: ## Auto commit all changes without push (main only)
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	@$(MAKE) -s prep
	@cd $(DOTFILES_DIR) && \
		git add -A && \
		{ git diff --cached --quiet || git commit -q -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"; }
	@echo "##> ✓ auto-commit 完了"
else
	@echo "$(HOSTNAME): サブ機では実行しません"
endif

git-auto: ## Auto commit all changes and push (main only)
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	@$(MAKE) -s auto-commit
	@cd $(DOTFILES_DIR) && git push
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
