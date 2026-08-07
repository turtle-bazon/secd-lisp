# Copyright (C) 2026
# License: GPL3

LISP ?= sbcl

.PHONY: build test clean

build:
	$(LISP) --non-interactive --load build.lisp

test:
	$(LISP) --non-interactive \
	  --eval '(asdf:load-system :secd-lisp-test)' \
	  --eval '(unless (secd-lisp/test:run-tests) (uiop:quit 1))'

clean:
	rm -rf build