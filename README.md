## Irmin -- A distributed database that follows the same design principles as Git

Irmin is a library for persistent stores with built-in snapshot,
branching and reverting mechanisms. It is designed to use a large
variety of backends. Irmin is written in pure OCaml and does not
depend on external C stubs; it aims to run everywhere, from Linux,
to browsers and Xen unikernels.

[![Build Status](https://travis-ci.org/mirage/irmin.svg)](https://travis-ci.org/mirage/irmin)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/irmin/)

### Description

Irmin is a library to version-control application data. It has the following
features:

- **on-disk format** various formats are supported, including the Git format:
  Irmin on-disk repositories can be inspected and modified using the classic
  Git command-line tools.

- **wire format** various formats are supported, including the Git protocol
  (only in client mode) or a simple JSON-based REST API (client and server).

- **dynamic behaviour** Irmin allows the users to define custom merge functions,
  to use in-memory transactions (to keep track of reads as well as writes) and
  to define event-driven workflows using a notification mechanism.

These abstractions allow developers to create applications with concurrent
behaviors which are both efficient and safe.

### Bindings to other languages

- **Go** [irmin-go](https://github.com/magnuss/irmin-go)
- **JavaScript** [irmin-js](https://github.com/talex5/irmin-js)

### Backends

Irmin ships with various backends. It provides the following OCamlfind pacakges:

- `irmin.mem` is an in-memory backend.
- `irmin.git` uses the Git format to persist data on disk.
- `irmin.fs` uses [bin_prot](https://github.com/janestreet/bin_prot) to persist
  data on disk.
- `irmin.http` uses JSON over HTTP to speak with an Irmin server.

Other external backends are available as external OPAM packages
(use `opam install <pkg>` to install):

- [irmin-chunk](https://github.com/mirage/irmin-chunk) stores raw contents into
  a well-balanced rope where leafs are chunks of all the same size.
- [irmin-indexdb](https://github.com/talex5/irmin-indexeddb) is a backend
  for a web browser's IndexedDB store.

### Datastructures

- [merge-queues](https://github.com/mirage/merge-queues) is an implementation
  of mergeable queues.
- [merge-ropes](https://github.com/mirage/merge-ropes) is an implementation
  of mergeable ropes.
- [diff-datatypes](https://github.com/gprano/diff-datatypes) is a collection
  of automatic merge functions based on edit scripts. It is fairly generic but
  contains specific implementation for mergeable trees, stacks and queues.
- [irmin-datatypes](https://github.com/kayceesrk/irmin-datatypes) is a
  collection of mergeable datatypes, including LWW registers, queues and sets.

### Use-Cases

Here a list of Irmin users:

- [Cuekeeper](https://github.com/talex5/cuekeeper) a
  version-controlled TODO list in the browser.
- [imaplet](https://github.com/gregtatcam/imaplet-lwt), a version-controlled
  IMAP server and client.
- [jitsu](https://github.com/mirage/jitsu), a DNS server that automatically
  starts unikernels on demand. The database is persisted with Irmin.
- [Irmin+Xenstore](https://github.com/djs55/ocaml-xenstore/tree/irminsule), the
  Xenstore deamon rewritten to use Irmin to persist its data.
- [irmin-arp](https://github.com/yomimono/irmin-arp), a distributed ARP cache.
- [dog](https://github.com/samoht/dog), a synchronisation tool.
- [irminFS](https://github.com/dsheets/irminfs), a prototype version-controlled
  file-system using Fuse.

### Further Reading

- [What a Distributed, Version-Controlled ARP Cache Gets
You](http://www.somerandomidiot.com/blog/2015/04/24/what-a-distributed-version-controlled-ARP-cache-gets-you/).
Blog post describing how Irmin can be used with Mirage to store the
network stack's ARP cache, which allows the history to be viewed using
the git tools.

- [CueKeeper: Gitting Things Done in the
Browser](http://roscidus.com/blog/blog/2015/04/28/cuekeeper-gitting-things-done-in-the-browser/).
A GTD-based todo list running client-side in the browser, using Irmin
compiled to JavaScript to provide history, revert and synchronisation
between tabs. The data is stored using an IndexedDB Irmin backend.

- [Using Irmin to add fault-tolerance to the Xenstore
database.](https://mirage.io/blog/introducing-irmin-in-xenstore)
Porting the Xen hypervisor toolstack to support Git persistence via
Irmin.

- [Introducing Irmin: Git-like distributed, branchable
storage.](https://mirage.io/blog/introducing-irmin) This is the first
post that describes Irmin, the new Git-like storage layer for Mirage
OS 2.0.

### Getting Started

#### Install

Irmin is packaged with [opam](https://opam.ocaml.org):

```
opam install irmin-unix # install all the optional depencies
```

#### Usage

Irmin comes with a command-line tool called `irmin`. See `irmin
 --help` for further reading. Use either `irmin <command> --help` or
 `irmin help <command>` for more information on a specific command.

To get the full capabilites of Irmin, use the [API](https://mirage.github.io/irmin):

```ocaml
open Lwt.Infix
open Irmin_unix
module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)

let config = Irmin_git.config ~bare:true "/tmp/irmin/test"
let info fmt = Irmin_unix.info ~author:"me <me@moi.com>" fmt

let prog =
  Store.Repo.v config >>= Store.master >>= fun t ->
  Store.set t ~info:(info "Updating foo/bar") ["foo"; "bar"] "hi!" >>= fun () ->
  Store.get t ["foo"; "bar"] >>= fun x ->
  Printf.printf "Read: %s\n%!" x;
  Lwt.return_unit

let () = Lwt_main.run prog
```

To compile the example above, save it to a file called
`example.ml`. Install irmin and git with opam (`opam install irmin-unix`) and
run

```ocaml
$ ocamlfind ocamlopt example.ml -o example -package irmin.unix,lwt.unix -linkpkg
$ ./example
Read: hi!
```

The `examples` directory contains more examples. To build them, run

```ocaml
$ ./configure --enable-examples
$ make
```

#### Tutorial

Tutorials are available on the
[wiki](https://github.com/mirage/irmin/wiki/).

### Issues

To report any issues please use the [bugtracker on
Github](https://github.com/mirage/irmin/issues).

### Conditions

See the [LICENSE file](./LICENSE.md).

### Acknowledgements

Development of Irmin was supported in part by the EU FP7 User-Centric Networking
project, Grant No. 611001.
