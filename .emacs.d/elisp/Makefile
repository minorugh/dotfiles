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
	@if find $(ELISP_DIR) -name "*.elc" | grep -q .; then \
	    find $(ELISP_DIR) -name "*.elc" -delete; \
	    echo "Removed *.elc"; \
	else \
	    $(EMACS) --batch -Q \
	      --eval "(byte-recompile-directory \"$(ELISP_DIR)\" 0 t)"; \
	    echo "Compiled *.elc"; \
	    $(MAKE) compile; \
	fi

clean: ## Same as toggle-elc
	@$(MAKE) toggle-elc

compile: ## Regenerate my-loaddefs.el using update-directory-autoloads
	@echo "Generating $(LOADDEFS) ..."
	$(EMACS) --batch -Q \
	  --eval "(require 'autoload)" \
	  --eval "(let ((generated-autoload-file \"$(LOADDEFS)\")) \
	    (apply #'update-directory-autoloads \
	      (delete-dups (cons \"$(ELISP_DIR)\" \
	        (mapcar #'file-name-directory \
	          (file-expand-wildcards \"$(ELISP_DIR)/*/\"))))))" \
	  --eval "(message \"Done: %s\" \"$(LOADDEFS)\")"
	@echo "Done."

git:
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
	git push

