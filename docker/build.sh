#!/bin/bash
set -eu
trap 'chown $(stat -c "%u:%g" Makefile) -R .' EXIT
source ~/.cargo/env
make release
