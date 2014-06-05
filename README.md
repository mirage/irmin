## Irmin

Irmin is a distributed database with built-in snapshot, branch and
revert mechanisms. It is designed to use a large variety of backends,
although it is optimized for append-only store.

Irmin is written in pure OCaml. It can thus be compiled to Javascript
-- and run in the browsers; or into a Mirage unikernel -- and run directly
on top of Xen.

[![Build Status](https://travis-ci.org/mirage/irmin.png?branch=master)](https://travis-ci.org/mirage/irmin)

### Install

Irmin is packaged with [opam](https://opam.ocaml.org):

```
opam install irmin
```

### Usage

Irmin comes with a command-line tool called `irmin`. See `irmin
 --help` for further reading. Use either `irmin <command> --help` or
 `irmin help <command>` for more information on a specific command.

To get the full capabilites of Irmin, use the API:

```ocaml
$ ledit ocaml
# #require "irmin.backend";;
# module G = IrminGit.Make(IrminGit.Memory);;
# module S = G.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String);;
# let () =
    S.create () >>= fun t ->
    S.update t ["foo"; "bar"] "hi!"
```

### Tutorial

A tutorial is available on the [wiki](https://github.com/mirage/irmin/wiki/Getting-Started).

### Issues

To report any issues please use the [bugtracker on Github](https://github.com/mirage/irmin/issues).

### Install from source

If you need to install Irmin from source, first you need to have
`libssl` on your system. For instance, on Debian/Ubuntu: ``` apt-get
install libssl-dev```.

Then, install the OCaml dependencies using [OPAM](http://opam.ocaml.org):
```
opam install ezjsonm ocamlgraph lwt sha \
             re dolog mstruct core_kernel \
             uri cohttp ssl core_kernel \
             cmdliner alcotest git crunch
```

You can then download the source code of Irmin, uncompress it, and run
the usual steps:

```
make PREFIX=$(opam config var prefix)
make install
```

Due to a bug in [oasis], the uninstall target from source is quite
unreliable. Instead, use:

```
ocamlfind remove irmin
rm $(which irmin)
```
