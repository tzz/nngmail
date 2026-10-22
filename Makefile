EMACS ?= emacs
PACKAGE_EL := nngmail.el nngmail-background.el nngmail-menu.el nngmail-gdocs.el nngmail-efrit.el
LOAD := -L . -L dev -L test

.PHONY: compile lint test clean

compile:
	$(EMACS) -Q --batch $(LOAD) --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile $(PACKAGE_EL)

lint: compile
	$(EMACS) -Q --batch $(LOAD) -l checkdoc \
	  --eval '(dolist (f (list $(foreach f,$(PACKAGE_EL),"$(f)"))) (checkdoc-file f))'

test:
	$(EMACS) -Q --batch $(LOAD) -l ert -l test/nngmail-tests.el \
	  -l test/nngmail-background-tests.el -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc dev/*.elc test/*.elc
