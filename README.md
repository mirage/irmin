## Irmin - A distributed database built on the same principles as Git
[![Build Status](https://travis-ci.org/mirage/irmin.svg)](https://travis-ci.org/mirage/irmin)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/irmin/)

Irmin is an OCaml library for building mergeable, branchable distributed data stores.

### Features
- **Built-in snapshotting** - backup and restore
- **Storage agnostic** - you can use Irmin on top of your own storage layer
- **Custom datatypes** - (de)serialization for custom data types
- **Highly portable** - runs anywhere from Linux to web browsers and Xen unikernels
- **Git compatibility** - `irmin-git` uses an on-disk format that can be inspected and modified using Git
- **Dynamic behavior** - allows the users to define custom merge functions, use in-memory transactions (to keep track of reads as well as writes) and to define event-driven workflows using a notification mechanism

### Documentation
Documentation can be found online at [https://mirage.github.io/irmin](https://mirage.github.io/irmin)


### Installation
To install Irmin, the command-line tool and all optional dependencies using [opam](https://github.com/ocaml/opam):

    opam install irmin-unix

A minimal installation, with no storage backends can be installed by running:

    opam install irmin

To only install the in-memory storage backend:

    opam install irmin-mem

The following packages have been made available on `opam`:
- `irmin` - the base package, no storage implementations
- `irmin-chunk` - chunked storage
- `irmin-fs` - filesystem-based storage using `bin_prot`
- `irmin-git` - Git compatible storage
- `irmin-http` - a simple REST interface
- `irmin-mem` - in-memory storage implementation
- `irmin-mirage` - mirage compatibility
- `irmin-unix` - unix compatibility

For more information about an individual package consult the [online documentation](https://mirage.github.io/irmin)

### Examples
Below is a simple example of setting a key and getting the value out of a Git based, filesystem-backed store.

```ocaml
open Lwt.Infix

(* Irmin store with string contents *)
module Store = Irmin_unix.Git.FS.KV(Irmin.Contents.String)

(* Database configuration *)
let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit author *)
let author = "Example <example@example.com>"

(* Commit information *)
let info fmt = Irmin_unix.info ~author fmt

let main =
    (* Open the repo *)
    Store.Repo.v config >>=

    (* Load the master branch *)
    Store.master >>= fun t ->

    (* Set key "foo/bar" to "testing 123" *)
    Store.set t ~info:(info "Updating foo/bar") ["foo"; "bar"] "testing 123" >>= fun () ->

    (* Get key "foo/bar" and print it to stdout *)
    Store.get t ["foo"; "bar"] >|= fun x ->
    Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let () = Lwt_main.run main
```

To compile the example above, save it to a file called `example.ml` and run:

```bash
$ ocamlfind ocamlopt example.ml -o example -package irmin-unix,lwt.unix -linkpkg
$ ./example
foo/bar => 'testing 123'
```
The `examples` directory contains some more advanced examples. The build them, run:

```bash
$ jbuilder build examples/trees.exe
$ _build/default/examples/trees.exe
```

### Command-line
The same thing can also be accomplished using `irmin`, the command-line application installed with `irmin-unix`, by running:

```bash
$ echo "root=." > .irminconfig
$ irmin init
$ irmin write foo/bar "testing 123"
$ irmin read foo/bar
```

`.irminconfig` allows for `irmin` flags to be set globally on a per-directory basis. Run `irmin help irminconfig` for further details.

Also see `irmin --help` for list of all commands and either `irmin <command> --help` or `irmin help <command>` for more help with a specific command.

### Issues
Feel free to to report any issues using the [Github bugtracker](https://github.com/mirage/irmin/issues).

### Conditions

See the [LICENSE file](./LICENSE.md).

### Acknowledgements

Development of Irmin was supported in part by the EU FP7 User-Centric Networking project, Grant No. 611001.
