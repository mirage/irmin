VERSION=0.1
PREFIX?=/usr/local
.PHONY: all test

PACKAGES=-pkgs cryptokit,jsonm,uri,ocamlgraph,cmdliner,lwt.syntax,cohttp.lwt
SYNTAX=-tags "syntax(camlp4o)"
FLAGS=-use-ocamlfind -cflags "-bin-annot" -no-links
INCLUDES=-Is src/lib,src/lwt
TARGET=irmin

all: irmin
	@

src/version.ml:
	echo "let current = \"$(VERSION)\"" > src/version.ml

$(TARGET): _build/src/main.native
	ln -f _build/src/main.native $(TARGET)

_build/src/main.native: src/version.ml
	ocamlbuild $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) src/main.native 

clean:
	rm -rf $(TARGET) _build

install:
	cp $(TARGET) $(PREFIX)/bin
