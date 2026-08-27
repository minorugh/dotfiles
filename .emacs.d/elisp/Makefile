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
else
	@echo "$(HOSTNAME): サブ機からはpushしません"
	git pull --rebase
endif

# ------------------------------------------------------------
# Publish: self-published packages を各ローカルリポジトリへコピーしてgit push
# ------------------------------------------------------------
PUBLISH_PKGS := git-peek gcal-dashboard deepl-translate tempbuf
PUBLISH_DEST := $(HOME)/src/github.com/minorugh

publish: ## Copy managed files to each local repo and push
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
		cp $$src $$dest; \
		echo "##> Copied $$pkg.el -> $$dest"; \
		$(MAKE) -C $(PUBLISH_DEST)/$$pkg git; \
	done

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
