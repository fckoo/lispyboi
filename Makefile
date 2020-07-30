CC := clang++
CFLAGS := -std=c++17 -fpic -Wall
LDFLAGS := -ldl

.PHONY: all clean debug release

all: debug

debug: CFLAGS += -O0 -g3 -DDEBUG=3
#debug: CFLAGS += -fsanitize=address -fsanitize-recover=all
debug: CFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls
debug: LDFLAGS += -Wl,--export-dynamic
debug: lispyboi

release: CFLAGS += -O3
release: lispyboi

lispyboi: obj/lispyboi.o obj/backtrace.o obj/platform.o obj/ffi.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.o: src/%.cpp obj
	$(CC) $(CFLAGS) -o $@ -c $<

obj:
	mkdir -p $@

clean:
	rm -rf obj lispyboi
