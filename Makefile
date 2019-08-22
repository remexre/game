all:
	sbcl --load load.lisp
swank:
	sbcl --load load.lisp --eval '(game:swank)'
.PHONY: all swank
