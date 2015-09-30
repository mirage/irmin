## Irmin-chunk

This package provides an Irmin backend to cut raw contents into blocks
of the same size, while preserving the keys used in the store. It can
be used to optimize space usage when dealing with large files or as a
an intermediate layer for a raw block device backend.

[![Build Status](https://travis-ci.org/mirage/irmin-chunk.svg)](https://travis-ci.org/mirage/irmin-chunk)
[![docs](https://img.shields.io/badge/doc-online-blue.svg)](https://mirage.github.io/irmin-chunk/)

### Install

Use opam:

```shell
opam install irmin-chunk
```

### Use

```ocaml
(* Build an Irmin store, where blobs are cut into chunks of same size *)
module AO = Irmin_chunk.AO_stable(Irmin_mem.Link)(Irmin_mem.AO)
module Store = Irmin.Make(AO)(Irmin_mem.RW)
```
