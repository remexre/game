CFLAGS := -Wall -Werror
TOOLS := gefsModel

all: target/game target/release/librenderer.so tools
debug: target/debug/librenderer.so
	sbcl --quit \
		--eval "(ql:quickload :game)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)"
release: target/game target/release/librenderer.so
run: target/release/librenderer.so
	sbcl --non-interactive \
		--eval "(ql:quickload :game)" \
		--eval "(game:main)"
run-release: release
	target/game
tools: $(patsubst %,target/tools/%,$(TOOLS))
.PHONY: all debug release run run-release tools

watch: target/debug/librenderer.so
	tmux \
		new-session -s game-debug -n game-debug 'sleep 3; cargo watch -x check -x build -s "pkill -10 sbcl"' \; \
		split-window -t game-debug '$(MAKE) debug'
.PHONY: watch

target/tools/%: target/tools-tmp/%.o
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) -o $@ $^

target/tools-tmp/%.o: tools/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c -o $@ $^

target/game:
	sbcl --non-interactive \
		--eval "(ql:quickload :game)" \
		--eval "(asdf:make :game)"
target/debug/librenderer.so:
	cargo build
target/release/librenderer.so:
	cargo build --release
.PHONY: target/game target/debug/librenderer.so target/release/librenderer.so
