# Makefile for ~/.emacs.d/elisp/
all: clean

MAKEFILE_DIR := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

clean:
	@n=$$(ls -1 *.elc 2>/dev/null | wc -l); \
	rm -f *.elc; \
	echo "##> Removed $$n .elc file(s)"

HOSTNAME := $(shell hostname)
# Hostname-based git push control:
# P1 (main): commit + push / others (sub): pull --rebase only
git: ## Auto commit+push (main only, sub: pull only)
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),P1)
	git push
	$(MAKE) publish
else
	@echo "$(HOSTNAME): サブ機からはpushしません"
	git pull --rebase
endif

# ------------------------------------------------------------
# Publish: self-published packages を各ローカルリポジトリへコピーしてgit push
# P1 のみ実行可能
# ------------------------------------------------------------
PUBLISH_PKGS := git-peek gcal-dashboard deepl-translate tempbuf
PUBLISH_DEST := $(HOME)/src/github.com/minorugh
publish: ## Copy changed managed files to each local repo and push
ifeq ($(HOSTNAME),P1)
	@for pkg in $(PUBLISH_PKGS); do \
		src=$$pkg.el; \
		dest=$(PUBLISH_DEST)/$$pkg/$$pkg.el; \
		if [ ! -f $$src ]; then \
			echo "##> SKIP $$pkg: $$src not found"; \
			continue; \
		fi; \
		if [ ! -d $(PUBLISH_DEST)/$$pkg ]; then \
			echo "##> SKIP $$pkg: repo $(PUBLISH_DEST)/$$pkg not found"; \
			continue; \
		fi; \
		if [ -f $$dest ] && cmp -s $$src $$dest; then \
			echo "##> SKIP $$pkg: no changes"; \
			continue; \
		fi; \
		cp $$src $$dest; \
		echo "##> Copied $$pkg.el -> $$dest"; \
		$(MAKE) -C $(PUBLISH_DEST)/$$pkg git; \
	done
else
	@echo "##> SKIP publish: $(HOSTNAME) is not P1"
endif


# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
