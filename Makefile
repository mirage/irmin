VERSION=0.1
PREFIX?=/usr/local
.PHONY: all test

PACKAGES=-pkgs cryptokit,jsonm,uri,ocamlgraph,cmdliner,lwt,ocplib-endian,cstruct
SYNTAXES= -tags "syntax(camlp4o)" -pkgs lwt.syntax,cohttp.lwt,cstruct.syntax
FLAGS=-use-ocamlfind -cflags "-bin-annot" -no-links
INCLUDES=-Is src,src/lib,src/lwt
TARGET=irmin

.PHONY: _build/src/irminMain.native

all: $(TARGET)
	@

src/irminVersion.ml:
	echo "let current = \"$(VERSION)\"" > src/irminVersion.ml

$(TARGET): _build/src/irminMain.native
	ln -s -f _build/src/irminMain.native $(TARGET)

_build/src/irminMain.native: src/irminVersion.ml
	ocamlbuild $(INCLUDES) $(FLAGS) $(PACKAGES) $(SYNTAXES) irminMain.native

clean:
	rm -rf $(TARGET) _build

install:
	cp $(TARGET) $(PREFIX)/bin src/irminVersion.ml
