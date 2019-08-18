CFLAGS := -fPIC -Wall -Werror -Wimplicit-fallthrough -Wunused-parameter
CFLAGS_DBG := -g -Og -fsanitize=address,undefined
CFLAGS_REL := -DNDEBUG -O3 -fsanitize=address
LDFLAGS := -flto -fPIC -lgc
LDFLAGS_DBG := -fsanitize=address,undefined
LDFLAGS_REL := -fsanitize=address
UNITS := buffer io lexer lisp/args lisp/context lisp/env lisp/eval lisp/lists lisp/prims \
	lisp/value parser util

all: compile
clean:
	test ! -d out || rm -r out
	test ! -d tmp || rm -r tmp
compile: out/game out/game-debug
watch:
	watchexec -cre c,h,py $(MAKE)
.PHONY: all clean compile watch

out/game: $(patsubst %,tmp/%.rel.o,$(UNITS) main)
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) $(LDFLAGS_REL) -o $@ $^

out/game-debug: $(patsubst %,tmp/%.dbg.o,$(UNITS) main)
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) $(LDFLAGS_DBG) -o $@ $^

tmp/%.dbg.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DBG) -c -o $@ $<

tmp/%.rel.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c -o $@ $<

tmp/%.d: src/%.c
	@set -e; mkdir -p $(dir $@); rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@.$$$$; \
		sed 's,$(notdir $*)\.o[ :]*,tmp/$*.dbg.o tmp/$*.rel.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$
include $(patsubst %,tmp/%.d,$(UNITS) main)
