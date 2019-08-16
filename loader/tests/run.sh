#!/bin/bash

set -eu

export PIPENV_VENV_IN_PROJECT=1
if [[ ! -d .venv ]]; then
	pipenv install
fi
exec pipenv run python tests.py
