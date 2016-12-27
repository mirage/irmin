HTTP   ?= $(shell opam config var cohttp:installed)
GIT    ?= $(shell opam config var git:installed)
UNIX   ?= $(shell opam config var git-unix:installed)
MIRAGE ?= $(shell opam config var git-mirage:installed)
TESTS  ?= true

OPTIONS=--with-http ${HTTP} --with-git ${GIT} --with-unix ${UNIX} \
	--with-mirage ${MIRAGE} --tests ${TESTS} -q

.PHONY: test

all:
	ocaml pkg/pkg.ml build ${OPTIONS}

clean:
	rm -rf _build lib_test/_tests lib_test/test-db lib_test/test_db_git
	ocaml pkg/pkg.ml clean

test:
	ocaml pkg/pkg.ml build ${OPTIONS}
