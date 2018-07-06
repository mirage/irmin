.PHONY: all clean test

all:
	jbuilder build --dev

test:
	jbuilder runtest -j1 --no-buffer --dev

examples:
	jbuilder build @examples

clean:
	rm -rf _build
