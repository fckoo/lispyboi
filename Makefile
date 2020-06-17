CC := clang++
CFLAGS := -std=c++17 -fpic -Wall
LDFLAGS := -lreadline

.PHONY: all clean debug release

all: debug

debug: CFLAGS += -O1 -g3 -DDEBUG=2
debug: CFLAGS += -fsanitize=address -fsanitize-recover=all
debug: CFLAGS += -fno-omit-frame-pointer -fno-optimize-sibling-calls
debug: LDFLAGS += -Wl,--export-dynamic
debug: clean lispyboi

release: CFLAGS += -O3
release: clean lispyboi

lispyboi: obj/primitives.o obj/lispyboi.o obj/backtrace.o obj/platform.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.o: src/%.cpp obj
	$(CC) $(CFLAGS) -o $@ -c $<

obj:
	mkdir -p $@

clean:
	rm -rf obj lispyboi
