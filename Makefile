BUILD=jbuilder build --dev
RUNTEST=jbuilder runtest -j 1 --dev

all:
	$(BUILD)
	$(RUNTEST)

core:
	$(BUILD) -p irmin

mem:
	$(BUILD) -p irmin-mem
	$(RUNTEST) test/irmin-mem

fs:
	$(BUILD) -p irmin-fs
	$(RUNTEST) test/irmin-fs

git:
	$(BUILD) -p irmin-git
	$(RUNTEST) test/main_git

http:
	$(BUILD) -p irmin-http
	$(RUNTEST) test/main_http

mirage:
	$(BUILD) -p irmin-mirage

unix:
	$(BUILD) -p irmin-unix
	$(RUNTEST) test/main_unix

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
