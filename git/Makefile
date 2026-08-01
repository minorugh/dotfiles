########################################################
## git 世代管理（メイン機: commit+push / サブ機: pull only）
########################################################
# 個人の日々の作業運用のためのターゲット群。
# リストア用のトップレベル Makefile とは切り離して管理する。
# Main: commit + push / others (sub): pull --rebase only
# Note: Also synchronizes the secret '~/.env_source' repository on the maine machine.

# NOTE: 将来メイン機を変更した時は忘れずに修正すること
MAIN_HOSTNAME  := P1
HOSTNAME       := $(shell hostname)
DOTFILES_DIR   := $(HOME)/src/github.com/minorugh/dotfiles
ENV_SOURCE_DIR := $(HOME)/.env_source
ENV_SYNC_CACHE := $(HOME)/.cache/env-sync

.PHONY: git git-fix env-sync

git: ##! Auto commit+push (main only, sub: pull only)
	cd $(DOTFILES_DIR) && git add -A
	cd $(DOTFILES_DIR) && git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),$(MAIN_HOSTNAME))
	if [ -f $(ENV_SOURCE_DIR)/Makefile ]; then \
		$(MAKE) -s -C $(ENV_SOURCE_DIR) git; \
	fi
	cd $(DOTFILES_DIR) && git push
else
	@echo "$(HOSTNAME): サブ機からはpushしません（pullのみ）"
	@cd $(DOTFILES_DIR) && git pull --rebase || (echo "エラー: rebaseに失敗しました。'make git-fix' を実行してください" && exit 1)
	@$(MAKE) -s -C $(DOTFILES_DIR)/git env-sync
endif

env-sync: ## env_source / abook の更新を検知し、あれば確認のうえ同期（サブ機のgit pull連動用）
	@mkdir -p $(ENV_SYNC_CACHE); \
	env_hash_new=$$(sha256sum ~/Dropbox/backup/env/env_repo.bundle.gpg 2>/dev/null | awk '{print $$1}'); \
	env_hash_old=$$(cat $(ENV_SYNC_CACHE)/env.hash 2>/dev/null); \
	abook_name_new=$$(ls -t ${HOME}/Dropbox/backup/abook/addressbook_*.gpg 2>/dev/null | head -1); \
	abook_name_old=$$(cat $(ENV_SYNC_CACHE)/abook.name 2>/dev/null); \
	env_diff=false; abook_diff=false; \
	if [ -n "$$env_hash_new" ] && [ "$$env_hash_new" != "$$env_hash_old" ]; then env_diff=true; fi; \
	if [ -n "$$abook_name_new" ] && [ "$$abook_name_new" != "$$abook_name_old" ]; then abook_diff=true; fi; \
	if [ "$$env_diff" = false ] && [ "$$abook_diff" = false ]; then \
		echo "✓ env_source / abook ともに変更なし"; \
		exit 0; \
	fi; \
	echo ">>> 更新を検知しました:"; \
	if [ "$$env_diff" = true ]; then echo "  - env_source (secretファイル群)"; fi; \
	if [ "$$abook_diff" = true ]; then echo "  - abook (addressbook)"; fi; \
	read -p "同期しますか? [y/N]: " ans; \
	if [ "$$ans" != "y" ] && [ "$$ans" != "Y" ]; then \
		echo "スキップしました"; \
		exit 0; \
	fi; \
	if [ "$$env_diff" = true ]; then \
		gpg -d ~/Dropbox/backup/env/env_repo.bundle.gpg > /tmp/env_repo.bundle; \
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
		echo "$$env_hash_new" > $(ENV_SYNC_CACHE)/env.hash; \
		echo "✓ env_source 同期完了"; \
	fi; \
	if [ "$$abook_diff" = true ]; then \
		mkdir -p ${HOME}/.abook; \
		gpg --yes -d -o ${HOME}/.abook/addressbook "$$abook_name_new"; \
		echo "$$abook_name_new" > $(ENV_SYNC_CACHE)/abook.name; \
		echo "✓ abook 同期完了"; \
	fi

git-fix: ## rebase失敗時の自動修復
	cd $(DOTFILES_DIR) && git rebase --abort 2>/dev/null || true
	cd $(DOTFILES_DIR) && git reset --hard origin/main
	cd $(DOTFILES_DIR) && git pull
