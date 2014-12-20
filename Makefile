TESTS  ?= --enable-tests
PREFIX ?= /usr/local

VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
SETUP   = ocaml setup.ml
VFILE   = lib/ir_version.ml
SFILE   = lib/http/irmin_http_static.ml
SFILES  = $(wildcard lib/http/static/*.js) \
	  $(wildcard lib/http/*.html) \
	  $(wildcard lib/http/static/*.css)

build: setup.data $(VFILE) $(SFILE)
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
	rm -f $(VFILE) $(SFILE)
	rm -rf ./test-db

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS) $(TESTS) --prefix $(PREFIX)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

$(VFILE): _oasis
	echo "let current = \"$(VERSION)\"" > $@

$(SFILE): $(SFILES)
	cd lib/http/ && ./make_static.sh


doc/html/.git:
	cd doc/html && git init && git remote add origin git@github.com:samoht/irmin.git

gh-pages: doc/html/.git
	cd doc/html && git checkout gh-pages
	rm -f doc/html/*.html
	cp irmin.docdir/*.html doc/html/
	cd doc/html && git add *.html
	cd doc/html && git commit -a -m "Doc updates"
	cd doc/html && git push origin gh-pages

NAME    = $(shell grep 'Name:' _oasis    | sed 's/Name: *//')
ARCHIVE = https://github.com/mirage/$(NAME)/archive/$(VERSION).tar.gz

release:
	git tag -a $(VERSION) -m "Version $(VERSION)."
	git push upstream $(VERSION)
	$(MAKE) pr

pr:
	opam publish prepare $(NAME).$(VERSION) $(ARCHIVE)
	opam publish submit $(NAME).$(VERSION) && rm -rf $(NAME).$(VERSION)
