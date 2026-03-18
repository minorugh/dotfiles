########################################################
## Docker 開発環境管理
########################################################
gitea: ## Gitea の起動（ポート3000）
	cd docker/gitea && docker compose up -d

gitea-down: ## Gitea の停止
	cd docker/gitea && docker compose down

gitea-log: ## Gitea のログ表示
	cd docker/gitea && docker compose logs -f

mattermost: ## Mattermost の起動（ポート8065）
	cd docker/mattermost && docker compose up -d

mattermost-down: ## Mattermost の停止
	cd docker/mattermost && docker compose down

mattermost-log: ## Mattermost のログ表示
	cd docker/mattermost && docker compose logs -f

httpd: ## Apache httpd の起動（ポート8080）
	cd docker/httpd && docker compose up -d

httpd-down: ## Apache httpd の停止
	cd docker/httpd && docker compose down

httpd-log: ## Apache httpd のログ表示
	cd docker/httpd && docker compose logs -f

docker-start: gitea mattermost httpd ## 全Dockerサービス起動

docker-stop: gitea-down mattermost-down httpd-down ## 全Dockerサービス停止

docker-ps: ## 起動中コンテナ一覧
	docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}"
