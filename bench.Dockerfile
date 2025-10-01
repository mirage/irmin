FROM ocaml/opam:debian-ocaml-5.3
RUN sudo ln -sf /usr/bin/opam-2.1 /usr/bin/opam
RUN mkdir bench-dir && chown opam:opam bench-dir
WORKDIR bench-dir
RUN sudo chown opam .
COPY *.opam ./
RUN opam remote add origin https://github.com/ocaml/opam-repository.git && opam update
RUN opam pin -yn --with-version=dev .
RUN opam install -y --deps-only --with-test .
COPY . ./
ENTRYPOINT opam exec -- make bench
