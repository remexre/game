all: release
debug:
	sbcl --quit \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)"
release:
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(asdf:make :game)"
run:
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game:main)"
run-release: release
	target/game
.PHONY: all debug release run run-release
