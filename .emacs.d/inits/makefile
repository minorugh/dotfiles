EMACS       := emacs
PACKAGE_DIR := ~/.emacs.d/elpa
ELISP_DIR   := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))

a.out: toggle-elc

toggle-elc:
	@if ls *.elc 2>/dev/null | grep -q .; then \
	    rm -f $(LOADDEFS) *.elc; \
	    echo "Removed $(LOADDEFS) and *.elc"; \
	else \
	    $(EMACS) --batch -Q \
	      --eval "(dolist (d (file-expand-wildcards \"$(PACKAGE_DIR)/*\")) (add-to-list 'load-path d))" \
	      --eval "(require 'leaf)" \
	      --eval "(require 'leaf-keywords)" \
	      --eval "(leaf-keywords-init)" \
	      --eval "(dolist (f (directory-files \"$(ELISP_DIR)\" t \"\\\\.el$$\")) (byte-compile-file f))"; \
	    echo "Compiled *.elc"; \
	fi

# clean:
# 	rm -f *.elc
