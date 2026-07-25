# Makefile for ~/.emacs.d/elisp/
# Usage: make         → toggle *.elc (remove if exists, compile if not)
#        make clean   → remove *.elc only
ELISP_DIR := $(shell cd "$(dir $(abspath $(lastword $(MAKEFILE_LIST))))" && pwd)
EMACS     := emacs
IVY_DIR   := $(shell ls -d $(HOME)/.emacs.d/elpa/ivy-*/ 2>/dev/null | head -1)

.PHONY: all toggle-elc clean

all: toggle-elc

toggle-elc:
	@n=$$(find $(ELISP_DIR) -maxdepth 1 -name "*.elc" | wc -l); \
	if [ $$n -gt 0 ]; then \
	find $(ELISP_DIR) -maxdepth 1 -name "*.elc" -delete; \
	echo "##> Removed $$n .elc file(s)"; \
	else \
	$(EMACS) --batch -Q \
	--eval "(add-to-list 'load-path \"$(ELISP_DIR)\")" \
	--eval "(add-to-list 'load-path \"$(IVY_DIR)\")" \
	--eval "(byte-recompile-directory \"$(ELISP_DIR)\" 0 nil)"; \
	n=$$(find $(ELISP_DIR) -maxdepth 1 -name "*.elc" | wc -l); \
	echo "##> Compiled $$n .elc file(s)"; \
	fi

clean:
	@n=$$(find $(ELISP_DIR) -maxdepth 1 -name "*.elc" | wc -l); \
	find $(ELISP_DIR) -maxdepth 1 -name "*.elc" -delete; \
	echo "##> Removed $$n .elc file(s)"

git: toggle-elc

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
