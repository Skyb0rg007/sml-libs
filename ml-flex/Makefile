
CC = gcc
CFLAGS = -Wall -Wextra -g -std=c99 -fno-omit-frame-pointer -fsanitize=address
LDFLAGS = -fsanitize=address
LDLIBS =
FLEX = flex

json.yy.c: json.l
	$(FLEX) --outfile=$@ --header=json.yy.h $<

json.yy.o: json.yy.c
	$(CC) $(CFLAGS) -c $< -o $@

main.o: main.c json.yy.c
	$(CC) $(CFLAGS) -c $< -o $@

a.out: main.o json.yy.o
	$(CC) $(LDFLAGS) $^ -o $@ $(LDLIBS)
