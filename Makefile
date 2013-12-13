VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
SETUP   = ocaml setup.ml
VFILE   = lib/core/irminVersion.ml
TESTS  ?= --enable-tests
PREFIX ?= /usr/local

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all:
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)
	rm -f

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS) $(TESTS) --prefix $(PREFIX)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

$(VFILE): _oasis
	echo "let current = \"$(VERSION)\"" > $@
