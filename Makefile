VERSION  = 0.1
PREFIX  ?= /usr/local
MAIN     = irminMain
TESTS    = test_disk
TARGET   = irmin

PACKAGES = -pkgs cryptokit,jsonm,uri,ocamlgraph,cmdliner,lwt,ocplib-endian,cstruct
SYNTAXES = -tags "syntax(camlp4o)" -pkgs lwt.syntax,cohttp.lwt,cstruct.syntax
FLAGS    = -use-ocamlfind -cflags "-bin-annot" -no-links
INCLUDES = -Is src,src/lib,src/lwt
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
	$(BUILD) -pkg ounit $(TESTS:%=tests/%.native)
	for test in $(TESTS); do \
	  IRMIN_DEBUG=1 ./_build/tests/$$test.native; \
	done
clean:
	rm -rf $(TARGET) _build

install:
	cp $(TARGET) $(PREFIX)/bin src/irminVersion.ml
