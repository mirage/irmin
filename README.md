## Irmin - A distributed database built on the same principles as Git
[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https%3A%2F%2Fci.ocamllabs.io%2Fbadge%2Fmirage%2Firmin%2Fmaster&logo=ocaml)](https://ci.ocamllabs.io/github/mirage/irmin)
[![Travis-CI Build Status](https://travis-ci.org/mirage/irmin.svg)](https://travis-ci.org/mirage/irmin)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/irmin/)

Irmin is an OCaml library for building mergeable, branchable distributed data stores.

### Features
- **Built-in snapshotting** - backup and restore
- **Storage agnostic** - you can use Irmin on top of your own storage layer
- **Custom datatypes** - (de)serialization for custom data types, derivable via [`ppx_irmin`][ppx_irmin-readme]
- **Highly portable** - runs anywhere from Linux to web browsers and Xen unikernels
- **Git compatibility** - `irmin-git` uses an on-disk format that can be inspected and modified using Git
- **Dynamic behavior** - allows the users to define custom merge functions, use in-memory transactions (to keep track of reads as well as writes) and to define event-driven workflows using a notification mechanism

### Documentation
Documentation can be found online at [https://mirage.github.io/irmin](https://mirage.github.io/irmin)

### Installation

#### Prerequisites:

Please ensure to install the minimum Opam and Ocaml versions which are 2 and 4.07 respectively.
          
    opam list    // listing the installed packages

To install Irmin, the command-line tool and all optional dependencies using [opam](https://github.com/ocaml/opam):

    opam install irmin-unix

***Note :***  If you face installation issues due to pinned packages. Run  the below commands to unpin and try to install again,

Unpinning *irmin-unix* package,

    opam info irmin-unix // to get available versions
    opam pin -s | grep irmin | xargs opam unpin

A minimal installation, with no storage backends can be installed by running:

    opam install irmin

To only install the in-memory storage backend:

    opam install irmin-mem

The following packages have been made available on `opam`:
- `irmin` - the base package, no storage implementations
- `irmin-chunk` - chunked storage
- `irmin-fs` - filesystem-based storage using `bin_prot`
- `irmin-git` - Git compatible storage
- `irmin-graphql` - GraphQL server
- `irmin-http` - a simple REST interface
- `irmin-mem` - in-memory storage implementation
- `irmin-mirage` - mirage compatibility
- `irmin-mirage-git` - Git compatible storage for mirage
- `irmin-mirage-graphql` - mirage compatible GraphQL server
- `irmin-unix` - unix compatibility
- `irmin-pack` - compressed, on-disk, posix backend
- `ppx_irmin` - PPX deriver for Irmin content types (see [README_PPX.md][ppx_irmin-readme])

For more information about an individual package consult the [online documentation](https://mirage.github.io/irmin).

#### Development version

To install the development version of Irmin, clone this repository and `opam install` the packages inside:

    git clone https://github.com/mirage/irmin
    cd irmin/
    opam install .

### Examples
Below is a simple example of setting a key and getting the value out of a Git based, filesystem-backed store.

<!-- N.B. Any changes to the following example must be mirrored in `examples/readme.ml`. -->
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
  Store.set_exn t ~info:(info "Updating foo/bar") ["foo"; "bar"] "testing 123" >>= fun () ->

  (* Get key "foo/bar" and print it to stdout *)
  Store.get t ["foo"; "bar"] >|= fun x ->
  Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let () = Lwt_main.run main
```

The example is contained in `examples/readme.ml`. It can be compiled and executed with dune:

```bash
$ dune build examples/readme.exe
$ dune exec examples/readme.exe
foo/bar => 'testing 123'
```
The `examples/` directory also contains more advanced examples, which can be executed in the same way.

### Command-line
The same thing can also be accomplished using `irmin`, the command-line application installed with `irmin-unix`, by running:

```bash
$ echo "root: ." > irmin.yml
$ irmin init
$ irmin set foo/bar "testing 123"
$ irmin get foo/bar
```

`irmin.yml` allows for `irmin` flags to be set on a per-directory basis. You can also set flags globally using `$HOME/.irmin/config.yml`. Run `irmin help irmin.yml` for further details.

Also see `irmin --help` for list of all commands and either `irmin <command> --help` or `irmin help <command>` for more help with a specific command.

### Issues
Feel free to to report any issues using the [Github bugtracker](https://github.com/mirage/irmin/issues).

### Conditions

See the [LICENSE file](./LICENSE.md).

### Acknowledgements

Development of Irmin was supported in part by the EU FP7 User-Centric Networking project, Grant No. 611001.

[ppx_irmin-readme]: ./README_PPX.md
