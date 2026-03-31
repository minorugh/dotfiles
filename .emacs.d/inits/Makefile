a.out: clean


clean:
	rm -f *.elc


HOSTNAME := $(shell hostname)

git:
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
ifeq ($(HOSTNAME),P1)
	git push
else
	@echo "$(HOSTNAME): サブ機からはpushしません"
	git pull --rebase
endif

