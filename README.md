## Irminsule

Irminsule is a distributed database with built-in snapshot, branch and
revert mechanisms. It is designed to use a large variety of backends,
although it is optimized for append-only store.

Irminsule is written in pure OCaml. It can thus be compiled to Javascript
-- and run in the browsers; or into a Mirage unikernel -- and run directly
on top of Xen.

[![Build Status](https://travis-ci.org/samoht/irminsule.png?branch=master)](https://travis-ci.org/samoht/irminsule)

### Build & Install

You first need to have `libssl` on your system. For instance, on Debian/Ubuntu:
```
apt-get install libssl-dev
```

Then, install the OCaml dependencies using [OPAM](http://opam.ocaml.org):
```
opam install ezjsonm ocamlgraph lwt cryptokit \
             re dolog mstruct core_kernel \
             uri cohttp ssl core_kernel \
             cmdliner alcotest git
```

You can then download the source code of Irminsule, uncompress it, and run
the usual steps:

```
make PREFIX=$(opam config var prefix)
make install
```

### Uninstall

Due to a bug in [oasis], the uninstall target is quite unreliable. Instead, use:

```
ocamlfind remove irminsule
rm $(which irmin)
```

### Usage

Irminsule comes with a command-line tool called `irmin`. See `irmin
 --help` for further reading. Use either `irmin <command> --help` or
 `irmin help <command>` for more information on a specific command.

### Tutorial

A tutorial is available on the [wiki](https://github.com/samoht/irminsule/wiki/Getting-Started).

### Issues

To report any issues please use the [bugtracker on Github](https://github.com/samoht/issues).
