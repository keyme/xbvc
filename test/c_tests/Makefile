
CC = gcc

C_FILES = test.c foo/C/cobs.c foo/C/xbvc_core.c gqueue.c


all: run_test
.PHONY: clean

CFLAGS = -Wall -Werror -Wextra -ggdb

foo: msgs.yaml
	xbvcgen -i $< -o $@ -t host -t device -l c

test: foo $(C_FILES)
	gcc $(C_FILES) $(CFLAGS) -Ifoo/C -o $@

run_test: test
	./test

clean:
	rm -rf \
	test \
	foo
