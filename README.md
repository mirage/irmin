<div align="center">
  <a href="https://irmin.org">
    <img src="./logo.svg" alt="Irmin logo"/>
  </a>
  <br />
  <strong>A Distributed Database Built on the Same Principles as Git</strong>
</div>

<div align="center">
<br />

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fmirage%2Firmin%2Fmain&logo=ocaml&style=flat-square)](https://ci.ocamllabs.io/github/mirage/irmin)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/mirage/irmin?style=flat-square&color=09aa89)](https://github.com/mirage/irmin/releases/latest)
[![docs](https://img.shields.io/badge/doc-online-blue.svg?style=flat-square)](https://mirage.github.io/irmin/)

</div>

<hr />

<div align="center">
  <em>
    Irmin is an OCaml library for building mergeable, branchable distributed
    data stores.
  </em>
</div>

## About

- **Built-in Snapshotting** - backup and restore
- **Storage Agnostic** - you can use Irmin on top of your own storage layer
- **Custom Datatypes** - (de)serialization for custom data types, derivable via
  [`ppx_irmin`][ppx_irmin-readme]
- **Highly Portable** - runs anywhere from Linux to web browsers and Xen unikernels
- **Git Compatibility** - `irmin-git` uses an on-disk format that can be
  inspected and modified using Git
- **Dynamic Behavior** - allows the users to define custom merge functions,
  use in-memory transactions (to keep track of reads as well as writes) and
  to define event-driven workflows using a notification mechanism

## Documentation

API documentation can be found online at [https://mirage.github.io/irmin](https://mirage.github.io/irmin)

## Installation

### Prerequisites

Please ensure to install the minimum `opam` and `ocaml` versions. Find the latest
version and install instructions on [ocaml.org](https://ocaml.org/docs/install.html).

To install Irmin with the command-line tool and all optional dependencies using
`opam`:

```bash
  opam install irmin-unix
```

A minimal installation containing the reference in-memory backend can be
installed by running:

```bash
  opam install irmin
```

The following packages have been made available on `opam`:
- `irmin` - the base package, plus an in-memory storage implementation
- `irmin-chunk` - chunked storage
- `irmin-fs` - filesystem-based storage using `bin_prot`
- `irmin-git` - Git compatible storage
- `irmin-graphql` - GraphQL server
- `irmin-http` - a simple REST interface
- `irmin-mirage` - mirage compatibility
- `irmin-mirage-git` - Git compatible storage for mirage
- `irmin-mirage-graphql` - mirage compatible GraphQL server
- `irmin-unix` - unix compatibility
- `irmin-pack` - compressed, on-disk, posix backend
- `ppx_irmin` - PPX deriver for Irmin content types (see [README_PPX.md][ppx_irmin-readme])
- `irmin-containers` - collection of simple, ready-to-use mergeable data structures

To install a specific package, simply run:

```bash
  opam install <package-name>
```

### Development Version

To install the development version of Irmin in your current `opam switch`, clone
this repository and `opam install` the packages inside:

```bash
  git clone https://github.com/mirage/irmin
  cd irmin/
  opam install .
```

## Usage

### Example

Below is a simple example of setting a key and getting the value out of a
Git-based, filesystem-backed store.

<!-- N.B. Any changes to the following example must be mirrored in `examples/readme.ml`. -->
```ocaml
open Lwt.Syntax

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
  let* repo = Store.Repo.v config in

  (* Load the master branch *)
  let* t = Store.master repo in

  (* Set key "foo/bar" to "testing 123" *)
  let* () =
    Store.set_exn t ~info:(info "Updating foo/bar") ["foo"; "bar"]
      "testing 123"
  in

  (* Get key "foo/bar" and print it to stdout *)
  let+ x = Store.get t ["foo"; "bar"] in
  Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let () = Lwt_main.run main
```

The example is contained in [examples/readme.ml](./examples/readme.ml) It can
be compiled and executed with dune:

    $ dune build examples/readme.exe
    $ dune exec examples/readme.exe
    foo/bar => 'testing 123'

The [examples](./examples/) directory also contains more advanced examples,
which can be executed in the same way.

### Command-line

The same thing can also be accomplished using `irmin`, the command-line
application installed with `irmin-unix`, by running:

    $ echo "root: ." > irmin.yml
    $ irmin init
    $ irmin set foo/bar "testing 123"
    $ irmin get foo/bar

`irmin.yml` allows for `irmin` flags to be set on a per-directory basis. You
can also set flags globally using `$HOME/.irmin/config.yml`. Run
`irmin help irmin.yml` for further details.

Also see `irmin --help` for list of all commands and either
`irmin <command> --help` or `irmin help <command>` for more help with a
specific command.

## Issues

Feel free to to report any issues using the [GitHub bugtracker](https://github.com/mirage/irmin/issues).

## License

See the [LICENSE file](./LICENSE.md).

## Acknowledgements

Development of Irmin was supported in part by the EU FP7 User-Centric Networking
project, Grant No. 611001.

[ppx_irmin-readme]: ./README_PPX.md
