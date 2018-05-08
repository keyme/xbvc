
CC = gcc

NIM_FILES = foo/Nim/xbvc.nim


all: test
.PHONY: clean test

CFLAGS = -Wall -Werror -Wextra -ggdb

foo: msgs.yaml
	xbvcgen -i $< -o $@ -t host -t device -l nim

test: foo
	nim c --threads:on -p:$(CURDIR)/foo/Nim --run test.nim

clean:
	rm -rf \
	test \
	foo
