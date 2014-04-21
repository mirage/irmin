# OPAM packages needed to build tests.
OPAM_PACKAGES="ezjsonm ocamlgraph lwt sha re dolog mstruct core_kernel uri \
               cohttp ssl core_kernel cmdliner alcotest git"

ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1

opam init
opam pin git https://github.com/samoht/ocaml-git.git
opam install ${OPAM_PACKAGES}

eval `opam config env`
ocaml setup.ml -configure --enable-tests
make
make test
