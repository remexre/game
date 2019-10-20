SCENE := assets/scenes/lod-teapot.json

all:
	docker build -t remexre/game-builder docker
	docker run --rm \
		-v "$(shell pwd)/tmp:/root/.cache" \
		-v "$(shell pwd):/code" \
		remexre/game-builder bash /build.sh
clean:
	test ! -d assets/shaders || rm -r assets/shaders
	test ! -f assets/textures/ter-u32n.png || rm -r assets/textures/ter-u32n.png
	test ! -d out || rm -r out
	test ! -d preassets/target || rm -r preassets/target
	test ! -d nova/target || rm -r nova/target
	test ! -d tmp || rm -r tmp
debug:
	sbcl --quit \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)" \
		"$(SCENE)"
repl: assets
	sbcl \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		"$(SCENE)"
run: assets
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(game:main)" \
		"$(SCENE)"
run-c-example: out/nova-c-example
	$<
run-lye-example: out/nova-lye-example
	$<
run-nova-example: out/nova-example
	$<
.PHONY: all clean debug repl run

assets: tmp/preassets
	$<
out/game:
	sbcl --non-interactive \
		--eval "(push (uiop:getcwd) asdf:*central-registry*)" \
		--eval "(ql:quickload :game :verbose t)" \
		--eval "(cffi:close-foreign-library '%glfw::glfw)" \
		--eval "(trace sb-ext:save-lisp-and-die)" \
		--eval "(asdf:make :game)"
out/game.tgz: assets out/game out/libnova.so
	@mkdir -p $(dir $@)
	tar czvf $@ $^
out/libnova.so:
	@mkdir -p $(dir $@)
	cd nova && cargo build --release
	cp nova/target/release/libnova.so $@
out/nova-example: assets
	@mkdir -p $(dir $@)
	cargo build --example=nova --manifest-path=nova/Cargo.toml
	cp nova/target/debug/examples/nova $@
out/nova-c-example: assets out/libnova.so nova/example.c
	@mkdir -p $(dir $@)
	gcc -o $@ -Wall -Wextra -Werror -O2 nova/example.c -lnova -Lout -Wl,-rpath=$(abspath out):.
out/nova-lye-example: assets
	@mkdir -p $(dir $@)
	cargo build --example=lye --manifest-path=nova/Cargo.toml
	cp nova/target/debug/examples/lye $@
.PHONY: assets out/game out/libnova.so out/nova-example out/nova-lye-example

tmp/preassets:
	@mkdir -p $(dir $@)
	cd preassets && cargo build
	cp preassets/target/debug/preassets $@
.PHONY: tmp/preassets
