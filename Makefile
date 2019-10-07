SCENE := assets/scenes/lod-teapot.json
all: release
debug:
	sbcl --quit \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)" \
		"$(SCENE)"
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
		--eval "(game:main)" \
		"$(SCENE)"
run-release: release
	target/game "$(SCENE)"
.PHONY: all debug release run run-release
