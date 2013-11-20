VERSION  = 0.2
PREFIX  ?= /usr/local
MAIN     = irminArg
TESTS    = test
TARGET   = irmin

PACKAGES = -pkgs cryptokit,jsonm,uri,ocamlgraph,cmdliner,lwt,ocplib-endian,cstruct \
	   -pkgs cohttp.lwt
FLAGS    = -use-ocamlfind -cflags "-bin-annot" -no-links
INCLUDES = -Is src/core,src/backend,src/server,src/driver
BUILD    = ocamlbuild $(INCLUDES) $(FLAGS) $(PACKAGES) $(SYNTAXES)

.PHONY: all test
.PHONY: _build/src/irminMain.native

all: $(TARGET)
	@

src/irminVersion.ml:
	echo "let current = \"$(VERSION)\"" > src/core/irminVersion.ml

$(TARGET): _build/src/$(MAIN).native
	ln -s -f _build/src/driver/$(MAIN).native $(TARGET)

_build/src/$(MAIN).native: src/irminVersion.ml
	$(BUILD) $(MAIN).native

test:
	$(BUILD) -pkg ounit tests/$(TESTS).native
	./_build/tests/$(TESTS).native --quick-tests

clean:
	rm -rf $(TARGET) _build test-db test-output src/irminVersion.ml

install:
	cp $(TARGET) $(PREFIX)/bin
