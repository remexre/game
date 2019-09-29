all: release
debug:
	sbcl --quit \
		--eval "(ql:quickload :game)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)"
release: target/game
run:
	sbcl --non-interactive \
		--eval "(ql:quickload :game)" \
		--eval "(game:main)"
run-release: release
	target/game
.PHONY: all debug release run run-release

target/game:
	sbcl --non-interactive \
		--eval "(ql:quickload :game)" \
		--eval "(asdf:make :game)"
.PHONY: target/game
