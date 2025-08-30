EMACS ?= emacs
CASK ?= cask

.PHONY: deps lint compile test clean

deps:
	$(CASK) install

lint:
	$(CASK) exec $(EMACS) -Q --batch -l package-lint-batch 		-f package-lint-batch-and-exit verse-mode.el

compile:
	$(CASK) exec $(EMACS) -Q --batch -f batch-byte-compile verse-mode.el

test:
	$(CASK) exec ert-runner

clean:
	rm -f *.elc
	rm -rf .cask
