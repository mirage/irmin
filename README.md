## Irmin

Irmin is a library for persistent stores with built-in snapshot,
branching and reverting mechanisms. It is designed to use a large
variety of backends. Irmin is written in pure OCaml and does not
depend on external C stubs; it aims is to run everywhere, from Linux
to Xen unikernels.

[![Build Status](https://travis-ci.org/mirage/irmin.svg)](https://travis-ci.org/mirage/irmin)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/irmin/)

### Install

Irmin is packaged with [opam](https://opam.ocaml.org):

```
opam install irmin-unix # install all the optional depencies
```

The `git` and `cohttp` packages are needed to compile and install the
`irmin` command-line tool.

### Usage

Irmin comes with a command-line tool called `irmin`. See `irmin
 --help` for further reading. Use either `irmin <command> --help` or
 `irmin help <command>` for more information on a specific command.

To get the full capabilites of Irmin, use the [API](https://mirage.github.io/irmin):

```ocaml
open Lwt
open Irmin_unix
let store = Irmin.basic (module Irmin_git.FS) (module Irmin.Contents.String)
let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
let prog =
  Irmin.create store config task >>= fun t ->
  Irmin.update (t "Updating foo/bar")  ["foo"; "bar"] "hi!" >>= fun () ->
  Irmin.read_exn (t "Reading foo/bar") ["foo"; "bar"] >>= fun x ->
  Printf.printf "Read: %s\n%!" x;
  return_unit
let () = Lwt_main.run prog
```

To compile the example above, save it to a file called
`example.ml`. Install irmin and git with opam (`opam install irmin
git`) and run

```ocaml
$ ocamlfind ocamlopt example.ml -o example -package lwt,irmin.unix,lwt.unix -linkpkg
$ ./example
Read: hi!
```

The `examples` directory contains more examples. To build them, run

```ocaml
$ ./configure --enable-examples
$ make
```

### Tutorial

Tutorials are available on the
[wiki](https://github.com/mirage/irmin/wiki/).

### Issues

To report any issues please use the [bugtracker on
Github](https://github.com/mirage/irmin/issues).
