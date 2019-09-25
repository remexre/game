all: target/game target/release/librenderer.so
debug: target/debug/librenderer.so
	sbcl \
		--eval "(ql:quickload :game)" \
		--eval "(game::enable-loop-stage :debug)" \
		--eval "(game:main)"
run: target/release/librenderer.so
	sbcl --non-interactive \
		--eval "(ql:quickload :game)" \
		--eval "(game:main)"
run-release: target/game target/release/librenderer.so
	target/game
.PHONY: all debug run run-release

watch:
	tmux \
		new-session -s game-debug -n game-debug 'sleep 3; cargo watch -x check -x build -s "pkill -10 sbcl"' \; \
		split-window -t game-debug 'sbcl --eval "(ql:quickload :game)" --eval "(game::enable-loop-stage :debug)" --eval "(game:main)"'
.PHONY: watch

target/game:
	sbcl --non-interactive \
		--eval "(ql:quickload :game)" \
		--eval "(asdf:make :game)"
target/debug/librenderer.so:
	cargo build
target/release/librenderer.so:
	cargo build --release
.PHONY: target/game target/debug/librenderer.so target/release/librenderer.so
