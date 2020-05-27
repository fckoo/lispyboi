CC := g++
CFLAGS := -std=c++17 -O0 -g
LDFLAGS := -O0 -g -lreadline


lispyboi: obj/primitives.o obj/lispyboi.o
	$(CC) -o $@ $^ $(LDFLAGS)

obj/%.o: src/%.cpp obj
	$(CC) $(CFLAGS) -o $@ -c $<

obj:
	mkdir -p $@

.PHONY: clean
clean:
	rm -rf obj lispyboi
