CC := g++
CFLAGS := -std=c++17 -O0 -g3 -rdynamic -fno-omit-frame-pointer
#CFLAGS := -std=c++17 -O3
LDFLAGS := -lreadline


.PHONY: all
all: clean lispyboi

lispyboi: obj/primitives.o obj/lispyboi.o obj/backtrace.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

obj/%.o: src/%.cpp obj
	$(CC) $(CFLAGS) -o $@ -c $<

obj:
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf obj lispyboi
