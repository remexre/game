#!/bin/bash

# Prereq: ensure game is already in the asdf search path.

tmux \
	new-session -s game-debug -n game-debug 'sleep 3; cargo watch -x check -x build -s "pkill -10 sbcl"' \; \
	split-window -t game-debug 'sbcl --eval "(ql:quickload :game)" --eval "(game::enable-loop-stage :debug)" --eval "(game:main)"'
