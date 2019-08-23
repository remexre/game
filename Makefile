all:
	sbcl --non-interactive --load load.lisp
repl:
	sbcl --load load.lisp
run:
	sbcl --load load.lisp --eval '(game:main)' --quit
swank:
	sbcl --load load.lisp --eval '(game:swank)'
.PHONY: all repl run swank
