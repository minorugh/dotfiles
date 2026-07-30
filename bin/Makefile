test-guard: ##! テスト用（実処理なし、echo だけ）
	@echo "実行されました: $$(date)"
	@echo "INSIDE_EMACS=$$INSIDE_EMACS"

test-guard-fail: ##! テスト用（失敗を模擬する）
	@echo "実行されました: $$(date)"
	@echo "INSIDE_EMACS=$$INSIDE_EMACS"
	@echo "わざとここで失敗させます" >&2
	@exit 1
