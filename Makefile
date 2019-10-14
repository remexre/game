SCENE := assets/scenes/lod-teapot.json

all:
	docker build -t remexre/game-builder docker
	mkdir -p tmp/dists
	docker run --rm \
		-v "$(shell pwd)/tmp:/root/.cache" \
		-v "$(shell pwd):/code" \
		remexre/game-builder bash /build.sh
clean:
	test ! -d assets/shaders || rm -r assets/shaders
	test ! -f assets/textures/ter-u32n.png || rm -r assets/textures/ter-u32n.png
	test ! -f game || rm game
	test ! -f game.tgz || rm game.tgz
	test ! -d preassets/target || rm -r preassets/target
	test ! -d renderer/target || rm -r renderer/target
	test ! -d tmp || rm -r tmp
debug:
	sbcl --quit \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)" \
		"$(SCENE)"
preprocess-assets: preassets/target/debug/preassets
	cd preassets && cargo run
preassets/target/debug/preassets:
	cd preassets && cargo build
release: preprocess-assets
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(cffi:close-foreign-library '%glfw::glfw)" \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(asdf:make :game)"
	tar czvf game.tgz game assets
renderer/target/release/librenderer.so:
	cd renderer && cargo build --release
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
