## Irminsule

Irminsule is a distributed database with built-in snapshot, branch and
revert mechanisms. It is designed to use a large variety of backends,
although it is optimized for append-only ones.

Irminsule is written in pure OCaml, and can thus be compiled to a
variety of backends (including Javascript, and Mirage
microkernels).

### Compile

```
opam install jsonm uri ocamlgraph cohttp cmdliner obuild lwt \
     ocplib-endian ssl cryptokit
make
```

### Usage

TODO