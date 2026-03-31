a.out: clean


clean:
	rm -f *.elc


git:
	git add -A
	git diff --cached --quiet || git commit -m "auto: $$(date '+%Y-%m-%d %H:%M:%S')"
	git push

