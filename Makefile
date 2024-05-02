EMACS ?= emacs

.PHONY: test clean

clean:
	rm -f *.elc *~ \#*\#

test: ob-acl2-test.el
	$(EMACS) -batch -l ert -l ob-acl2-test.el -f ert-run-tests-batch-and-exit
