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
induce performance regressions.

The performances of individual releases of irmin (+index, +repr) are saved in
`/bench/releases`, inside the benchmarking server.

From the benchmarking server, setup the right versions of the right libraries
and then run Irmin's trace replay:
```sh
; export ROOT='/bench/releases/irminX.Y.Z-indexX.Y.Z-reprX.Y.Z'
; dune exec -- bench/irmin-pack/tree.exe --mode trace --ncommits-trace 100000 /bench/replay-traces/tezos-replayable-v0.repr --verbosity info --path-conversion v0+v1 --artefacts $ROOT --keep-stat-trace
```

The following command will pretty print 2 releases side by side for manual
comparison:
```sh
; export ROOT_OLD='/bench/releases/irminX.Y.Z-indexX.Y.Z-reprX.Y.Z'
; dune exec -- bench/irmin-pack/trace_stats.exe pp -f old,$ROOT_OLD/boostrap_summary.json -f new,$ROOT/boostrap_summary.json
```

If one of the old `boostrap_summary.json` file is out of date (resulting in
JSON parsing a error), it may be re-generated with the following commands:
```
; rm $ROOT_OLD/boostrap_summary.json
; dune exec -- bench/irmin-pack/trace_stats.exe summarise $ROOT_OLD/stat_trace.repr >$ROOT_OLD/boostrap_summary.json
; chmod 444 $ROOT_OLD/boostrap_summary.json
```

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
