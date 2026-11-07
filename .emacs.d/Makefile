HOSTNAME := $(shell hostname)
# Hostname-based git push control:
# P1 (main): push only (commit is done in Emacs) / others (sub): pull --ff-only only
git: ## Push (main only, sub: pull only)
	$(MAKE) -s -C ../git git

# ------------------------------------------------------------
# [Read-only] This file opens in read-only mode automatically.
# Toggle editable: C-c C-e  or  qq
# ------------------------------------------------------------
# Local Variables:
# buffer-read-only: t
# End:
