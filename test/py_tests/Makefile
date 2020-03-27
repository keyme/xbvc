
all: test
.PHONY: clean test foo

foo: msgs.yaml
	@xbvcgen -i $< -o $@ -t host -t device -l python
	@cp foo/Python/* .

test: foo
	@python3 test_xbvc.py

clean:
	rm -rf \
	test \
	foo \
	xbvc_py.py \
	cobs.py
