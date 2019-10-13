SCENE := assets/scenes/lod-teapot.json

all:
	docker build -t remexre/game-builder docker
	mkdir -p tmp/dists
	docker run --rm \
		-v "$(shell pwd)/tmp:/root/.cache" \
		-v "$(shell pwd):/code" \
		remexre/game-builder bash /build.sh
clean:
	test ! -f game || rm game
	test ! -f game.tgz || rm game.tgz
	test ! -d preassets/target || rm -r preassets/target
	test ! -d tmp || rm -r tmp
debug:
	sbcl --quit \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)" \
		"$(SCENE)"
preprocess-assets: preassets/target/debug/preassets
	$<
preassets/target/debug/preassets:
	cargo build --manifest-path preassets/Cargo.toml
release: preprocess-assets
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(cffi:close-foreign-library '%glfw::glfw)" \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(asdf:make :game)"
	tar czvf game.tgz game assets
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
