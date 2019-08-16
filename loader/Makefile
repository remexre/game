CFLAGS := -fPIC -g -O3 -Wall -Werror -Wimplicit-fallthrough
LDFLAGS := -flto -fPIC -lgc
UNITS := interpreter io lexer lisp parser util

all: compile check
check: out/game.so
	cd tests && pipenv run python tests.py
clean:
	test ! -d out || rm -r out
	test ! -d tmp || rm -r tmp
compile: out/game
watch:
	watchexec -cre c,h,py $(MAKE)
.PHONY: all clean compile watch

out/game: $(patsubst %,tmp/%.o,$(UNITS)) tmp/main.o
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) -o $@ $^

out/game.so: $(patsubst %,tmp/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) -shared -o $@ $^

tmp/%.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c -o $@ $<
