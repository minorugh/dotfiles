# Makefile for ~/.emacs.d/elisp/
# Usage: make         → toggle *.elc (remove if exists, compile if not)
#        make clean   → remove *.elc only
ELISP_DIR := $(shell cd "$(dir $(abspath $(lastword $(MAKEFILE_LIST))))" && pwd)
EMACS     := emacs

.PHONY: all toggle-elc clean

all: toggle-elc

toggle-elc:
	if find $(ELISP_DIR) -maxdepth 1 -name "*.elc" | grep -q .; then \
	    find $(ELISP_DIR) -maxdepth 1 -name "*.elc" -delete; \
	    echo "Removed *.elc"; \
	else \
	    $(EMACS) --batch -Q \
	      --eval "(byte-recompile-directory \"$(ELISP_DIR)\" 0 nil)"; \
	    echo "Compiled *.elc"; \
	fi

clean:
	find $(ELISP_DIR) -maxdepth 1 -name "*.elc" -delete
	echo "Removed *.elc"

git: toggle-elc

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
