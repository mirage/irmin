# OPAM packages needed to build tests.
OPAM_PACKAGES="ezjsonm ocamlgraph lwt cryptokit re dolog mstruct core_kernel uri \
               cohttp ssl core_kernel cmdliner alcotest"

ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1
export OPAMVERBOSE=1
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam install ${OPAM_PACKAGES}

eval `opam config env`
make
make test
