## Irmin

Irmin is a library for persistent stores with built-in snapshot,
branching and reverting mechanisms. It is designed to use a large
variety of backends. Irmin is written in pure OCaml and does not
depend on external C stubs; it aims is to run everywhere, from Linux
to Xen unikernels -- and can be be compiled to JavaScipt to run in a
browser.

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
