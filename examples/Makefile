all: git_store
	@

git_store: git_store.ml
	ocamlfind ocamlopt -package irminsule.backend.git -linkpkg git_store.ml -o git_store

clean:
	rm -f *.cmi *.cmx *.o *.cmo git_store
	rm -rf _build
