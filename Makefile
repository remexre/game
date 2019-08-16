SUBDIRS := loader

all: compile check
check:
	@$(foreach d,$(SUBDIRS),$(MAKE) -C $(d) $@ || exit 1;)
clean:
	@$(foreach d,$(SUBDIRS),$(MAKE) -C $(d) $@ || exit 1;)
compile:
	@$(foreach d,$(SUBDIRS),$(MAKE) -C $(d) $@ || exit 1;)
.PHONY: all check clean compile

run: compile
	./loader/out/game
.PHONY: run
