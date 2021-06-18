# Release process

This file documents the necessary steps for releasing Irmin to its various users
(via GitHub, `opam-repository` and Tezos).

At a high level, releasing Irmin consists of publishing the following artefacts:

- a Git [commit tag][git-tags];
- a set of documentation on GitHub pages (e.g. [`mirage.github.io/irmin`][pages-docs]);
- a release archive (`.tbz` file containing the project source) on GitHub;
- a set of `.opam` files in [`opam-repository`][opam-repo] that point to this
  release archive;
- (optionally) a copy of these `.opam` files in the Tezos
  [`opam-repository`][tezos-opam-repo].

Most of this can be handled automatically by [`dune-release`][dune-release], as
described in the instructions below.

[git-tags]: https://git-scm.com/book/en/v2/Git-Basics-Tagging
[pages-docs]: https://mirage.github.io/irmin
[dune-release]: https://github.com/ocamllabs/dune-release
[opam-repo]: https://github.com/ocaml/opam-repository


## Manual benchmarking

Before releasing, it is important to make sure that the new version doesn't
induce performance regressions. The trace replay benchmarks should be used
for that purpose.

The performances of individual releases of irmin are saved inside the
benchmarking server at `/bench/releases/`:
```
.
└── ncommits_200k
    ├── irmin2.7.0_index1.4.0_repr0.4.0
    │   ├── packet_b
    │   │   ├── stat_summary.json
    │   │   └── stat_trace.repr
    │   └── packet_a
    │       ├── stat_summary.json
    │       └── stat_trace.repr
    ├── irmin2.5.4_index1.3.1_repr0.3.0
    │   ├── packet_b
    │   │   ├── stat_summary.json
    │   │   └── stat_trace.repr
    │   └── packet_a
    │       ├── stat_summary.json
    │       └── stat_trace.repr
    └── irmin2.2.0_index1.2.1
        ├── packet_b
        │   ├── stat_summary.json
        │   └── stat_trace.repr
        └── packet_a
            ├── stat_summary.json
            └── stat_trace.repr
```

To test a new release, setup an `c3-small-x86-01` Equinix instance, fetch a large
enough replay trace and setup the right versions of the right libraries
before running the benchmarks.

The benchmark should be run at least twice (to give insights about the noise), 
the two resulting `stat_trace.repr` files should be copied to 
`/bench/releases/` using the same naming convention as above. Also set the file
permissions to read-only using `chmod 444 stat_trace.repr`.

A run of the benchmark is expected to last \~35min.

See [this script](https://github.com/tarides/irmin-tezos/blob/master/bench.sh) for an example of a setup and a bench run.

### Visualising the results

A `stat_summary.json` can be computed from a `stat_trace.repr`. 

If missing or out of date (resulting in a parsing error), the JSON can be
(re)generated using the following commands:
```
dune exec -- bench/irmin-pack/trace_stats.exe summarise stat_trace.repr >stat_summary.json
```

A conversion is expected to last \~4 min.

One or more summaries can be pretty printed together. The following command 
will pretty print both runs of 2 releases side by side:
```sh
; export ROOT='/bench/releases/ncommits_200k/irmin2.7.0_index1.4.0_repr0.4.0/'
; export ROOT_OLD='/bench/releases/ncommits_200k/irmin2.2.0_index1.2.1/'
; dune exec -- bench/irmin-pack/trace_stats.exe pp \
  -f old,$ROOT_OLD/packet_a/stat_summary.json \
  -f old,$ROOT_OLD/packet_b/stat_summary.json \
  -f old,$ROOT/packet_a/stat_summary.json \
  -f old,$ROOT/packet_b/stat_summary.json
```

See [this script](https://github.com/tarides/irmin-tezos/blob/master/summarise.sh) for an example of a batch conversion to JSON.

## Releasing to opam-repository and GitHub

- Check that no `.opam` files contain `pin-depends` fields. If so, release those
  packages first.

```sh
; git grep -A 10 "pin-depends" *.opam
```

- Make a pull-request to this repository containing pre-release changes (drop
  `pin-depends`, add release number to `CHANGES.md`, any new constraints) and an
  empty commit to host the release tag.

```sh
; git fetch upstream
; git checkout -B release-X.Y.Z upstream/main
; git commit -m "Prepare X.Y.Z release" -- CHANGES.md
; git commit --allow-empty -m "Release X.Y.Z"
; hub pull-request
```

- Wait for the CI to pass on the release PR, then perform the following steps to
  release to `opam-repository`:

```sh
; opam pin add odoc.2.0.0 https://github.com/ocaml/odoc.git  # Use a modern Odoc

; dune-release tag                        #  Create appropriate Git tag by reading CHANGES.md
; dune-release distrib --skip-tests       #  Build release archive
; dune-release publish distrib --verbose  #  Push release archive to GitHub
; dune-release publish doc --verbose      #  Push documentation to GitHub pages
; dune-release opam pkg                   #  Generate `opam` files for `opam-repository`
; dune-release opam submit                #  Make PR to `opam-repository`
```

- Once the release PR is merged on `opam-repository`, merge the release PR on
  the development repository. If any changes to `.opam` files were necessary in
  the `opam-repository` PR, add those to the release PR before merging.

### Re-cutting a failed Opam release

It may be necessary to re-cut an attempted release, for instance if the
`opam-repository` CI raised issues that couldn't be fixed via if the
`opam-repository`.

First delete the release distribution via the GitHub UI, then cleanup the Git
tags and re-perform the required release steps:

```sh
; git tag -d X.Y.Z                     #  Erase git tag locally
; git push -d upstream X.Y.Z           #  Erase git tag on GitHib
; dune-release distrib --skip-tests
; dune-release publish doc --verbose   # ... if necessary
; dune-release opam pkg
; dune-release opam submit ^C          #  Exit at prompt to avoid creating pull request
; cd <opam-repository>
; git push -u origin --force           #  Add new `.opam` files to PR
```

## Releasing to Tezos' opam-repository

The Tezos project uses [its own `opam-repository`][tezos-opam-repo] to source
its dependencies, so upgrading its dependencies requires making a separate
release to this _after_ having released to the main `opam-repository`. The
process is as follows:

```sh
for p in <released_packages>; do
  # Remove old version of this package from Tezos' opam-repository
  rm ~/t/tezos-opam-repository/packages/$p/*/ -rf

  # Copy opam file for the new release of this package
  cp ~/t/{opam-repository,tezos-opam-repository}/packages/$p/$p.<release_version> -r
done
```

(The above process is somewhat automated by [this
script][tezos-downstream-script].) Once this is done, make an MR to the Tezos
opam-repository and – if necessary – a corresponding MR to Tezos to adjust to
any API changes.

[tezos-opam-repo]: https://gitlab.com/tezos/opam-repository
[tezos-downstream-script]: https://github.com/CraigFe/dotfiles/blob/main/scripts/.scripts/tezos-downstream
