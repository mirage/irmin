# ppx_irmin

PPX extension for automatically generating Irmin type witnesses.

## Overview

## Installation

You can install `ppx_irmin` using [opam](https://opam.ocaml.org/):

```
$ git clone git@github.com:CraigFe/ppx_irmin
$ opam pin --yes ./ppx_irmin
```

If you're using the [dune](https://github.com/ocaml/dune) build system, add the following field to your `library`, `executable` or `test` stanza:

```
(preprocess (pps ppx_irmin))
```

You can now use `[@@deriving irmin]` after a type declaration in your code to automatically derive an Irmin type witness with the same name.
