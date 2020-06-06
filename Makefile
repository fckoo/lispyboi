CC := g++
CFLAGS := -std=c++17
LDFLAGS := -lreadline

.PHONY: all clean debug release

all: debug

debug: CFLAGS += -O1 -g3 -fno-omit-frame-pointer -DDEBUG=1
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
