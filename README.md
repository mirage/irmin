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
[![codecov](https://codecov.io/gh/mirage/irmin/branch/main/graph/badge.svg?token=n4mWfgURqT)](https://codecov.io/gh/mirage/irmin)
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

<hr />

Irmin is based on distributed version-control systems (DVCs),
extensively used in software development to enable developers to keep
track of change provenance and expose modifications in the source
code. Irmin applies DVC's principles to large-scale distributed data
and exposes similar functions to Git (clone, push, pull, branch,
rebase). It is highly customizable: users can define their types to
store application-specific values and define custom storage layers (in
memory, on disk, in a remote Redis database, in the browser,
etc.). The Git workflow was initially designed for humans to manage
changes within source code. Irmin scales this to handle automatic
programs performing a very high number of operations per second, with
a fully automated handling of update conflicts. Finally, Irmin exposes
an event-driven API to define programmable dynamic behaviours and to
program distributed dataflow pipelines.

Irmin was created at the University of Cambridge in 2013 to be the
default storage layer for [MirageOS][] applications (both to store and
orchestrate unikernel binaries and the data that these unikernels are
using). As such, Irmin is not, strictly speaking, a complete database
engine. Instead, similarly to other MirageOS components, it is a
collection of libraries designed to solve different flavours of the
challenges raised by the [CAP Theorem][]. Each application
can select the right combination of libraries to solve its particular
distributed problem.

Irmin consists of a core of well-defined low-level data structures
that specify how data should be persisted and be shared across
nodes. It defines algorithms for efficient synchronization of those
distributed low-level constructs. It also builds a collection of
higher-level data structures that developers can use without knowing
precisely how Irmin works underneath. Some of these components even
have a [formal semantics][], including [Conflict-free Replicated
Data-Types (CRDT)][]. Since it's a part of MirageOS, Irmin does not
make strong assumptions about the OS environment that it runs in. This
makes the system very portable: it works well for in-memory databases
and slower persistent serialization such as SSDs, hard drives, web
browser local storage, or even the Git file format.

Irmin is primarily developed and maintained by [Tarides][], with
contributions from many [contributors][] from various
organizations. External maintainers and contributors are welcome.

[MirageOS]: https://mirage.io
[CAP Theorem]: http://en.wikipedia.org/wiki/CAP_theorem
[formal semantics]: https://kcsrk.info/papers/banyan_aplas20.pdf
[Conflict-free Replicated Data-Types (CRDT)]: https://arxiv.org/abs/2203.14518
[Tarides]: https://tarides.com
[contributors]: https://github.com/mirage/irmin/graphs/contributors

<div class="toc">

* [Features](#Features)
* [Documentation](#Documentation)
* [Installation](#Installation)
  * [Prerequisites](#Prerequisites)
  * [Development Version](#Development-Version)
* [Usage](#Usage)
  * [Example](#Example)
  * [Command-line](#Commandline)
* [Context](#Context)
  * * [Irmin as a portable and efficient structured key-value store](#Irmin-as-a-portable-and-efficient-structured-keyvalue-store)
    * [Irmin as a distributed store](#Irmin-as-a-distributed-store)
    * [Irmin as a dataflow scheduler](#Irmin-as-a-dataflow-scheduler)
* [Issues](#Issues)
* [License](#License)
* [Acknowledgements](#Acknowledgements)

</div>

## Features

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

To install Irmin with the command-line tool and all unix backends using `opam`:

<!-- $MDX skip -->
```bash
  opam install irmin-cli
```

A minimal installation containing the reference in-memory backend can be
installed by running:

<!-- $MDX skip -->
```bash
  opam install irmin
```

The following packages have are available on `opam`:

- `irmin` - the base package, plus an in-memory storage implementation
- `irmin-chunk` - chunked storage
- `irmin-cli` - a simple command-line tool
- `irmin-fs` - filesystem-based storage using `bin_prot`
- `irmin-git` - Git compatible storage
- `irmin-graphql` - GraphQL server
- `irmin-mirage` - mirage compatibility
- `irmin-mirage-git` - Git compatible storage for mirage
- `irmin-mirage-graphql` - mirage compatible GraphQL server
- `irmin-pack` - compressed, on-disk, posix backend
- `ppx_irmin` - PPX deriver for Irmin content types (see [README_PPX.md][ppx_irmin-readme])
- `irmin-containers` - collection of simple, ready-to-use mergeable data structures

To install a specific package, simply run:

<!-- $MDX skip -->
```bash
  opam install <package-name>
```

### Development Version

To install the development version of Irmin in your current `opam switch`, clone
this repository and `opam install` the packages inside:

<!-- $MDX skip -->
```bash
  git clone https://github.com/mirage/irmin
  cd irmin/
  opam install .
```

## Usage

### Example

Below is a simple example of setting a key and getting the value out of a
Git-based, filesystem-backed store.

<!-- $MDX file=examples/readme.ml -->
```ocaml
(* Irmin store with string contents *)
module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)

(* Database configuration *)
let config = Irmin_git.config ~bare:true "/tmp/irmin/test"

(* Commit author *)
let author = "Example <example@example.com>"

(* Commit information *)
let info fmt = Irmin_git_unix.info ~author fmt

let main () =
  (* Create the switch *)
  Eio.Switch.run @@ fun sw ->
  (* Open the repo *)
  let repo = Store.Repo.v ~sw config in

  (* Load the main branch *)
  let t = Store.main repo in

  (* Set key "foo/bar" to "testing 123" *)
  Store.set_exn t ~info:(info "Updating foo/bar") [ "foo"; "bar" ] "testing 123";

  (* Get key "foo/bar" and print it to stdout *)
  let x = Store.get t [ "foo"; "bar" ] in
  Printf.printf "foo/bar => '%s'\n" x

(* Run the program *)
let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
```

The example is contained in [examples/readme.ml](./examples/readme.ml) It can
be compiled and executed with dune:

<!-- $MDX skip -->
```bash
$ dune build examples/readme.exe
$ dune exec examples/readme.exe
foo/bar => 'testing 123'
```

The [examples](./examples/) directory also contains more advanced examples,
which can be executed in the same way.

### Command-line

The same thing can also be accomplished using `irmin`, the command-line
application installed with `irmin-cli`, by running:

```bash
$ echo "root: ." > irmin.yml
$ irmin init
$ irmin set foo/bar "testing 123"
$ irmin get foo/bar
testing 123
```

`irmin.yml` allows for `irmin` flags to be set on a per-directory basis. You
can also set flags globally using `$HOME/.irmin/config.yml`. Run
`irmin help irmin.yml` for further details.

Also see `irmin --help` for list of all commands and either
`irmin <command> --help` or `irmin help <command>` for more help with a
specific command.

## Context

Irmin's initial desing is directly inspired from
[XenStore](https://dl.acm.org/doi/10.1145/1631687.1596581), with:

- the need for efficient optimistic concurrency control features to be
  able to let thousands of virtual machine concurrently access and
  modify a central configuration database (the Xen stack uses XenStore
  as an RPC mechanism to setup VM configuration on boot). Very early
  on, the initial focus was to specify and handle [potential
  conflicts](https://hal.inria.fr/hal-01099136v1/document) when the
  optimistic assumptions do not usually work so well.
- the need for a convenient way to debug and audit possible issues
  that might happen in that system. Our [initial
  experiments](https://mirage.io/blog/introducing-irmin-in-xenstore)
  showed that it was possible to design a reliable system using Git as
  backend to persist configuation data reliably (to safely restart
  after a crash), while making system debugging easy and go really
  fast, thanks to efficient merging strategy.

In 2014, the first release of Irmin was announced part of the MirageOS
2.0 release [here](https://mirage.io/blog/introducing-irmin). Since
then, several projects started using and improving Irmin. These can
roughly be split into 3 categories: (i) use Irmin as a portable,
structured key-value store (with expressive, mergeable types); (ii)
use Irmin as distributed database (with a customizable consistency
semantics) and (iii) an event-driven dataflow engine.


#### Irmin as a portable and efficient structured key-value store

- [XenStored](https://github.com/xen-project/xen/tree/master/tools/ocaml/xenstored)
  is an information storage space shared between all the Xen virtual
  machines running in the same host. Each virtual machines gets its
  own path in the store. When values are changed in the store, the
  appropriate drivers are notified. The initial OCaml implementation
  was later extended to use Irmin
  [here](https://github.com/mirage/ocaml-xenstore-server). More
  details
  [here](https://mirage.io/blog/introducing-irmin-in-xenstore).
- [Jitsu](https://github.com/mirage/jitsu) is an experimental
  orchestrator for unikernels. It uses Irmin to store the unikernel
  configuration (and manage dynamic DNS entries). See more details
  [here](https://www.usenix.org/system/files/conference/nsdi15/nsdi15-paper-madhavapeddy.pdf).
- [Cuekeeper](https://github.com/talex5/cuekeeper) is a web-based GTD
  (a fancy TODO list) that runs entirely in the browser. It uses Irmin
  in the browser to store data locally, with support for structured
  concurrent editing and snapshot export and import. More details
  [here](https://roscidus.com/blog/blog/2015/04/28/cuekeeper-gitting-things-done-in-the-browser/).
- [Canopy](https://github.com/Engil/Canopy) and
  [Unipi](https://github.com/roburio/unipi) both use Irmin to serve
  static websites pull from Git repositories and deployed as
  unikernels.
- [Caldav](https://github.com/roburio/caldav) is using Irmin to store
  calendar entries and back them into a Git repository. More
  information [here](https://robur.io/Our%20Work/Projects).
- [Datakit](https://github.com/moby/datakit) was developed at Docker
  and provided a 9p interface to the Irmin API. It was used to manage
  the configuration of Docker for Desktop, with merge policies on
  upgrade, full auditing, and snapshot/rollback capabilites.
- [Tezos](https://gitlab.com/tezos/tezos/) started using Irmin in 2017
  to store the
  ledger state. The first prototype used irmin-git before switching to
  irmin-lmdb and irmin-leveldb (and now irmin-pack). More details
  [here](https://tarides.com/blog/2019-11-21-irmin-v2#tezos-and-irmin-pack).

#### Irmin as a distributed store

- An [IMAP](ttps://github.com/gregtatcam/imaplet-lwt) server using
  Irmin to store emails. More details
  [here](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-918.pdf). The
  goal of that project was both to use Irmin to store emails (so using
  Irmin as a local key-value store) but also to experiment with
  replacing the IMAP on-wire protocol by an explicit Git push/pull
  mechanism.
- [irmin-ARP](https://github.com/yomimono/irmin-arp) uses Irmin to
  store and audit ARP configuration. It's using Irmin as a local
  key-value store for very low-level information (which are normally
  stored very deep in the kernel layers), but the main goal was really
  to replace the broadcasting on-wire protocol by point-to-point
  pull/push synchronisation primitives, with a full audit log of ARP
  operations over a network. More details
  [here](http://somerandomidiot.com/blog/2015/04/24/what-a-distributed-version-controlled-ARP-cache-gets-you/).
- [Banyan](https://github.com/prismlab/irmin-scylla) uses Irmin to
  implement a distributed cache over a geo-replicated cluster. It's
  using [Cassandra](https://cassandra.apache.org/_/index.html) as a
  storage backend. More information
  [here](https://kcsrk.info/papers/banyan_aplas20.pdf).
- [irmin-fdb](https://github.com/andreas/irmin-fdb) implements an
  Irmin store backed by
  [FoundationDB](https://www.foundationdb.org/). More details
  [here](https://www.youtube.com/watch?v=NArvw-9axeg&ab_channel=TheLinuxFoundation).

#### Irmin as a dataflow scheduler

- [Datakit CI](https://github.com/moby/datakit/tree/master/ci) is a
  continuous integration service that monitors GitHub project and
  tests each branch, tag and pull request. It displays the test
  results as status indicators in the GitHub UI. It keeps all of its
  state and logs in DataKit, rather than a traditional relational
  database, allowing review with the usual Git tools. The core of the
  project is a scheduler that manage dataflow pipelines across Git
  repositories. It was used for a few years as the CI system test
  Docker for Desktop on bare-metal and virtual machines, as well as
  all the new opam package submissions to ocaml/opam-repository. More
  details
  [here](https://www.docker.com/blog/docker-unikernels-open-source/).
- [Causal RPC](https://github.com/CraigFe/causal-rpc) implements an
  RPC framework using Irmin as a network substrate. More details
  [here](https://www.craigfe.io/causalrpc.pdf).
- [CISO](https://github.com/samoht/ciso) is an experimental
  (distributed) Continuous Integration engine for OPAM. It was
  designed as a replacement of Datakit-CI and finally turned into
  [ocurrent](https://github.com/ocurrent/ocurrent).

## Issues

Feel free to report any issues using the [GitHub bugtracker](https://github.com/mirage/irmin/issues).

## License

See the [LICENSE file](./LICENSE.md).

## Acknowledgements

Development of Irmin was supported in part by the EU FP7 User-Centric Networking
project, Grant No. 611001.

[ppx_irmin-readme]: ./README_PPX.md
