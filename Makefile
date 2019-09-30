all: release
debug:
	sbcl --quit \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)"
release:
	sbcl --non-interactive \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(asdf:make :game)"
run:
	sbcl --non-interactive \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game:main)"
run-release: release
	target/game
.PHONY: all debug release run run-release
