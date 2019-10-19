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
	test ! -d out || rm -r out
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
preprocess-assets: tmp/preassets
	$<
release: preprocess-assets out/librenderer.so
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(cffi:close-foreign-library '%glfw::glfw)" \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(asdf:make :game)"
	tar czvf game.tgz game assets
repl: preprocess-assets
	sbcl \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		"$(SCENE)"
run: preprocess-assets
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game:main)" \
		"$(SCENE)"

.PHONY: all debug preprocess-assets release repl

out/librenderer.so:
	@mkdir -p $(dir $@)
	cd renderer && cargo build --release
	cp renderer/target/release/librenderer.so $@
out/renderer-c-example: out/librenderer.so renderer/example.c
	@mkdir -p $(dir $@)
	gcc -o $@ -Wall -Wextra -Werror -O2 renderer/example.c -lrenderer -Lout -Wl,-rpath=$(abspath out):.
.PHONY: out/librenderer.so

tmp/preassets:
	@mkdir -p $(dir $@)
	cd preassets && cargo build
	cp preassets/target/debug/preassets $@
.PHONY: tmp/preassets
