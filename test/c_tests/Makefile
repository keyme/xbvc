C_FILES = test.c foo/C/cobs.c foo/C/xbvc_core.c

all: test
.PHONY: clean

foo: msgs.yaml
	xbvcgen -i $< -o $@ -t host -t device -l c

test: foo $(C_FILES)
	gcc $(C_FILES) -Ifoo/C -o $@
	./$@

clean:
	rm -rf \
	test \
	foo
