# Makefile for ~/.emacs.d/elisp/
# Usage: make   → toggle *.elc (remove if exists, compile if not)
#        make git → auto commit+push (main only, sub: pull only)
ELISP_DIR  := $(shell cd "$(dir $(abspath $(lastword $(MAKEFILE_LIST))))" && pwd)
EMACS      := emacs

.PHONY: all toggle-elc git

all: toggle-elc

toggle-elc: ## Toggle *.elc: remove if exists, byte-compile if not
	if find $(ELISP_DIR) -maxdepth 1 -name "*.elc" | grep -q .; then \
	    find $(ELISP_DIR) -maxdepth 1 -name "*.elc" -delete; \
	    echo "Removed *.elc"; \
	else \
	    $(EMACS) --batch -Q \
	      --eval "(byte-recompile-directory \"$(ELISP_DIR)\" 0 nil)"; \
	    echo "Compiled *.elc"; \
	fi

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
