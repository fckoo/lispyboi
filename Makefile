CC := clang++
CFLAGS := -std=c++17 -Wall
LDFLAGS := -ldl

.PHONY: all clean _debug debug debug3 debug2 debug1 release

all: debug1

debug: debug1

debug3: CFLAGS += -O0 -g3 -DDEBUG=3
debug3: CFLAGS += -fsanitize=address -fsanitize-recover=all
debug3: _debug

debug2: CFLAGS += -O0 -g3 -DDEBUG=2
debug2: _debug

debug1: CFLAGS += -O1 -g3 -DDEBUG=1
debug1: _debug

_debug: CFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls
_debug: lispyboi

release: CFLAGS += -O3 -DDEBUG=0
release: lispyboi

lispyboi: obj/lispyboi.o obj/platform.o obj/ffi.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.o: src/%.cpp obj
	$(CC) $(CFLAGS) -o $@ -c $<

obj:
	mkdir -p $@

clean:
	rm -rf obj lispyboi
