SCENE := assets/scenes/lod-teapot.json

FILES := assets game

all:
	docker build -t remexre/game-builder docker
	docker run -v "$(shell pwd):/code" --rm remexre/game-builder bash /build.sh
clean:
	test ! -f game || rm game
debug:
	sbcl --quit \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)" \
		"$(SCENE)"
preprocess-assets:
	cargo run --manifest-path preassets/Cargo.toml
release: preprocess-assets
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(asdf:make :game)"
	tar czvf game.tgz $(FILES)
repl:
	sbcl \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		"$(SCENE)"
run:
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game:main)" \
		"$(SCENE)"
.PHONY: all debug preprocess-assets release repl
