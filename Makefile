VERSION  = 0.1
PREFIX  ?= /usr/local
MAIN     = irmin
TESTS    = test
TARGET   = irmin

PACKAGES = -pkgs cryptokit,jsonm,uri,ocamlgraph,cmdliner,lwt,ocplib-endian,cstruct
FLAGS    = -use-ocamlfind -cflags "-bin-annot" -no-links
INCLUDES = -Is src/core
BUILD    = ocamlbuild $(INCLUDES) $(FLAGS) $(PACKAGES) $(SYNTAXES)

.PHONY: all test
.PHONY: _build/src/irminMain.native

all: $(TARGET)
	@

src/irminVersion.ml:
	echo "let current = \"$(VERSION)\"" > src/irminVersion.ml

$(TARGET): _build/src/$(MAIN).native
	ln -s -f _build/src/$(MAIN).native $(TARGET)

_build/src/$(MAIN).native: src/irminVersion.ml
	$(BUILD) $(MAIN).native

test:
	$(BUILD) -pkg ounit tests/$(TESTS).native
	./_build/tests/$(TESTS).native

clean:
	rm -rf $(TARGET) _build test-db test-output

install:
	cp $(TARGET) $(PREFIX)/bin src/irminVersion.ml
