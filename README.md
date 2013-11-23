## Irminsule

Irminsule is a distributed database with built-in snapshot, branch and
revert mechanisms. It is designed to use a large variety of backends,
although it is optimized for append-only store.

Irminsule is written in pure OCaml, and can thus be compiled to Javascript
(to run in the browsers) and Mirage microkernels (to run directly on top of
Xen).

### Compile

```
opam install jsonm uri ocamlgraph cohttp cmdliner obuild lwt \
     ocplib-endian ssl cryptokit
make
```

### Install

```
make install
```

### Running the tests

To run only the quick tests:
```
make test
```

To run the full test suite:
```
make fulltest
```

### Usage

See `irmin --help`

## Issues

To report any issues please use the [bugtracker on Github](https://github.com/samoht/issues).
