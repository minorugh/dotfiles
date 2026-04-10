# Docker 開発環境管理 Makefile
# dotfiles/docker/Makefile
# author: Minoru Yamada
#
# 使い方:
#   dotfiles/ 直下から: make -f docker/Makefile gitea
#   docker/ 直下から:   make gitea

.DEFAULT_GOAL := help
.ONESHELL:
SHELL = /bin/bash

DOCKER_DIR := $(PWD)

.PHONY: help
help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| sort \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

## ----------------------------------------
## Gitea（ポート 3000）
## ----------------------------------------
.PHONY: gitea gitea-down gitea-log
gitea: ## Gitea の起動
	cd $(DOCKER_DIR)/gitea && docker compose up -d

gitea-down: ## Gitea の停止
	cd $(DOCKER_DIR)/gitea && docker compose down

gitea-log: ## Gitea のログ表示
	cd $(DOCKER_DIR)/gitea && docker compose logs -f

## ----------------------------------------
## Mattermost（ポート 8065）
## ----------------------------------------
.PHONY: mattermost mattermost-down mattermost-log mattermost-backup
mattermost: ## Mattermost の起動
	sudo chown -R 2000:2000 /home/minoru/Docker/mattermost
	cd $(DOCKER_DIR)/mattermost && docker compose up -d

mattermost-down: ## Mattermost の停止
	cd $(DOCKER_DIR)/mattermost && docker compose down

mattermost-log: ## Mattermost のログ表示
	cd $(DOCKER_DIR)/mattermost && docker compose logs -f

mattermost-backup: ## Mattermost の手動バックアップ
	/usr/local/bin/mattermost-backup.sh

## ----------------------------------------
## Apache httpd（ポート 8080）
## ----------------------------------------
.PHONY: httpd httpd-down httpd-log
httpd: ## Apache httpd の起動
	cd $(DOCKER_DIR)/httpd && docker compose up -d

httpd-down: ## Apache httpd の停止
	cd $(DOCKER_DIR)/httpd && docker compose down

httpd-log: ## Apache httpd のログ表示
	cd $(DOCKER_DIR)/httpd && docker compose logs -f

## ----------------------------------------
## 一括操作
## ----------------------------------------
.PHONY: docker-start docker-stop docker-ps
docker-start: gitea mattermost httpd ## 全Dockerサービス起動

docker-stop: gitea-down mattermost-down httpd-down ## 全Dockerサービス停止

docker-ps: ## 起動中コンテナ一覧
	docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"


# Hostname-based git push control:
# P1 (main): commit + push / others (sub): pull --rebase only
git: ## Auto commit+push (main only, sub: pull only)
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),P1)
	git push
else
	@echo "$(HOSTNAME): サブ機からはpushしません（pullのみ）"
	git pull --rebase
endif
