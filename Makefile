VERSION=0.1
PREFIX?=/usr/local
.PHONY: all test

PACKAGES=-pkgs cryptokit,jsonm,uri,ocamlgraph,cmdliner,lwt.syntax,cohttp.lwt
SYNTAX=-tags "syntax(camlp4o)"
FLAGS=-use-ocamlfind -cflags "-bin-annot" -no-links
INCLUDES=-Is src/lib,src/lwt

all: irminsule
	@

src/version.ml:
	echo "let current = \"$(VERSION)\"" > src/version.ml

irminsule: src/main.native
	ln -f _build/src/main.native irminsule

src/main.native: src/version.ml
	ocamlbuild $(INCLUDES) $(FLAGS) $(SYNTAX) $(PACKAGES) src/main.native 

clean:
	rm -rf irminsule _build

install:
	cp irminsule $(PREFIX)/bin
