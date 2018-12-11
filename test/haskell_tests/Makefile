
CC = gcc

NIM_FILES = foo/Nim/xbvc.nim


all: test
.PHONY: clean test

CFLAGS = -Wall -Werror -Wextra -ggdb

foo: msgs.yaml
	xbvcgen -i $< -o $@ -t host -t device -l haskell
	cp foo/Haskell/*.hs .
	mkdir XBVC
	mv Data.hs XBVC

test: foo
	cabal sandbox init
	cabal test --show-details=streaming

clean:
	rm -rf \
	foo \
	XBVC \
	XBVC.hs \
	Cobs.hs \
	Bitvec.hs
