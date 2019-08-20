CFLAGS := -fPIC -Wall -Werror -Wextra -Wdouble-promotion -Wduplicated-branches -Wduplicated-cond \
	-Wformat=2 -Wjump-misses-init -Wlogical-op -Wnull-dereference -Wrestrict -Wshadow
CFLAGS_DBG := -g -Og -fsanitize=address,undefined
CFLAGS_REL := -DNDEBUG -O3 -fsanitize=address
LDFLAGS := -flto -fPIC -lgc -lGL -lglfw -lX11
LDFLAGS_DBG := -fsanitize=address,undefined
LDFLAGS_REL := -fsanitize=address
UNITS := buffer io lisp/args lisp/context lisp/env lisp/eval lisp/gl lisp/prims lisp/value parser \
	util

LISP_UNITS := gl gl-mid lang

CFLAGS += -Isrc/ext/linenoise
UNITS += ext/linenoise/linenoise 

all: compile
check: out/bootstrapper
	./out/bootstrapper ''
clean:
	test ! -d out || rm -r out
	test ! -d tmp || rm -r tmp
compile: out/bootstrapper out/bootstrapper-debug
watch:
	watchexec -cre c,h,lisp $(MAKE) compile check
.PHONY: all check clean compile watch

out/bootstrapper: $(patsubst %,tmp/%.rel.o,$(UNITS) main)
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) $(LDFLAGS_REL) -o $@ $^

out/bootstrapper-debug: $(patsubst %,tmp/%.dbg.o,$(UNITS) main)
	@mkdir -p $(dir $@)
	$(CC) $(LDFLAGS) $(LDFLAGS_DBG) -o $@ $^

tmp/%.dbg.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_DBG) -c -o $@ $<

tmp/%.rel.o: src/%.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) $(CFLAGS_REL) -c -o $@ $<

tmp/%.lisp.h: src/%.lisp
	@mkdir -p $(dir $@)
	cd src && xxd -i $*.lisp > ../$@

tmp/main.d: $(patsubst %,tmp/%.lisp.h,$(LISP_UNITS))
tmp/main.dbg.o: $(patsubst %,tmp/%.lisp.h,$(LISP_UNITS))
tmp/main.rel.o: $(patsubst %,tmp/%.lisp.h,$(LISP_UNITS))

tmp/%.d: src/%.c
	@set -e; mkdir -p $(dir $@); rm -f $@; \
		$(CC) -M $(CFLAGS) $< > $@.$$$$; \
		sed 's,$(notdir $*)\.o[ :]*,tmp/$*.dbg.o tmp/$*.rel.o $@ : ,g' < $@.$$$$ > $@; \
		rm -f $@.$$$$
include $(patsubst %,tmp/%.d,$(UNITS) main)
