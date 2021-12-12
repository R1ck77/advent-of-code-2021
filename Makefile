.PHONY: test last

test:
	emacs -f package-initialize -batch --eval "(setq load-path (cons \".\" load-path))" -f buttercup-run-discover

last:
	emacs -f package-initialize -batch -l run-last.el  --eval "(run-last-test)"
