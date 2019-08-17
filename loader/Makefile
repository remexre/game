CFLAGS := -fPIC -g -O3 -Wall -Werror -Wimplicit-fallthrough
LDFLAGS := -flto -fPIC -lgc
UNITS := buffer io lexer lisp/args lisp/context lisp/env lisp/eval lisp/lists lisp/preds lisp/tags \
	lisp/value parser util

all: compile check
check: out/game.so
	cd tests && ./run.sh
clean:
	test ! -d out || rm -r out
	test ! -d tmp || rm -r tmp
compile: out/game
watch:
	watchexec -cre c,h,py $(MAKE)
.PHONY: all clean compile watch

out/game: $(patsubst %,tmp/%.san.o,$(UNITS) main)
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) -fsanitize=address,undefined -o $@ $^

out/game.so: $(patsubst %,tmp/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) -shared -o $@ $^

tmp/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c -o $@ $<

tmp/%.san.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -fsanitize=address,undefined -c -o $@ $<

tmp/%.d: src/%.c
	@set -e; mkdir -p $(dir $@); rm -f $@; \
		$(CC) -M $(CPPFLAGS) $< > $@.$$$$; \
		sed 's,\($*\)\.o[ :]*,tmp/\1.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$
include $(patsubst %,tmp/%.d,$(UNITS) main)
