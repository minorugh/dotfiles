# Makefile for ~/.emacs.d/elisp/
# Usage: make           → toggle *.elc (compile if none, remove if exists)
#        make compile   → regenerate my-loaddefs.el
#        make clean     → same as toggle-elc
ELISP_DIR  := $(shell cd "$(dir $(abspath $(lastword $(MAKEFILE_LIST))))" && pwd)
LOADDEFS   := $(ELISP_DIR)/my-loaddefs.el
EMACS      := emacs

.PHONY: all clean compile toggle-elc

a.out: toggle-elc

toggle-elc: ## Toggle *.elc: remove if exists, byte-compile if not
	@if ls *.elc 2>/dev/null | grep -q .; then \
	    rm -f $(LOADDEFS) *.elc; \
	    echo "Removed $(LOADDEFS) and *.elc"; \
	else \
	    $(EMACS) --batch -Q \
	      --eval "(byte-recompile-directory \"$(ELISP_DIR)\" 0 t)"; \
	    echo "Compiled *.elc"; \
	fi

clean: ## Same as toggle-elc
	@$(MAKE) toggle-elc

compile: ## Regenerate my-loaddefs.el using loaddefs-generate
	@echo "Generating $(LOADDEFS) ..."
	$(EMACS) --batch -Q \
	  --eval "(loaddefs-generate \"$(ELISP_DIR)\" \"$(LOADDEFS)\")" \
	  --eval "(message \"Done: %s\" \"$(LOADDEFS)\")"
	@echo "Done."
