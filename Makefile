.PHONY: all clean test

BUILD=jbuilder build --dev
RUNTEST=jbuilder runtest -j1 --no-buffer --dev

all:
	$(BUILD)

test:
	$(RUNTEST)

clean:
	rm -rf _build

REPO=../opam-repository
PACKAGES=$(REPO)/packages

# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	rm -f $(PACKAGES)/$*/$*.opam
	cd $(PACKAGES) && git add $*

PKGS=$(basename $(wildcard *.opam))
opam-pkg:
	$(MAKE) $(PKGS:%=pkg-%)
