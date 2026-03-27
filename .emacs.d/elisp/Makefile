# Makefile for ~/.emacs.d/elisp/
# Usage: make         → regenerate my-loaddefs.el
#        make clean   → delete my-loaddefs.el

# Makefile for ~/.emacs.d/elisp/
# Usage: make         → regenerate my-loaddefs.el
#        make clean   → delete my-loaddefs.el
ELISP_DIR  := $(shell cd "$(dir $(abspath $(lastword $(MAKEFILE_LIST))))" && pwd)
LOADDEFS   := $(ELISP_DIR)/my-loaddefs.el
EMACS      := emacs

.PHONY: all clean

all:
	@echo "Generating $(LOADDEFS) ..."
	$(EMACS) --batch -Q \
	  --eval "(loaddefs-generate \"$(ELISP_DIR)\" \"$(LOADDEFS)\")" \
	  --eval "(message \"Done: %s\" \"$(LOADDEFS)\")"
	@echo "Done."

clean:
	@rm -f $(LOADDEFS)
	@echo "Removed $(LOADDEFS)"
