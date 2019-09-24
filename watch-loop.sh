#!/bin/bash

# Prereq: ensure game is already in the asdf search path.

sbcl --non-interactive \
	--eval "(ql:quickload :game)" \
	--eval "(game-util:reload-loop)"
