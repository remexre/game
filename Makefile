all:
	sbcl --non-interactive --load load.lisp --eval '(asdf:make :game)'
repl:
	sbcl --load load.lisp
run:
	sbcl --load load.lisp --eval '(game:main)' --quit
.PHONY: all repl run
