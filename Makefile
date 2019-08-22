all:
	sbcl --load load.lisp
run:
	sbcl --load load.lisp --eval '(game:main)' --eval '(uiop:quit)'
swank:
	sbcl --load load.lisp --eval '(game:swank)'
.PHONY: all run swank
