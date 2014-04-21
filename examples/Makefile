TARGETS = git_store custom_merge views

BUILDS = $(TARGETS:%=_build/%.native)

all: $(BUILDS)
	@

_build/%.native: %.ml
	ocamlbuild -use-ocamlfind -no-hygiene -tag "syntax(camlp4o)" -package irminsule.backend.git,sexplib.syntax,comparelib.syntax,bin_prot.syntax $*.native

clean:
	ocamlbuild -clean
	rm -rf _build
