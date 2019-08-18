SUBDIRS := bootstrapper

all: compile
clean:
	@$(foreach d,$(SUBDIRS),$(MAKE) -C $(d) $@ || exit 1;)
compile:
	@$(foreach d,$(SUBDIRS),$(MAKE) -C $(d) $@ || exit 1;)
.PHONY: all clean compile

run: compile
	./bootstrapper/out/game src/main.lisp
.PHONY: run
