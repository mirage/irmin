# ppx_irmin

PPX extension for automatically generating Irmin type witnesses.

[![Build Status](https://travis-ci.com/CraigFe/ppx_irmin.svg?branch=master)](https://travis-ci.com/CraigFe/ppx_irmin)
## Overview

`ppx_irmin` automatically generates Irmin type witnesses (values of type `_ Irmin.Type.t`)
corresponding to type declarations in your code. For example:

```ocaml
type tree =
  | Branch of tree * bool option * tree
  | Leaf of int32 * string [@@deriving irmin]
```

will be expanded to:

```ocaml
type tree = (* as above *)

let tree_t =
  let open Irmin.Type in
  mu (fun tree_t ->
      variant "tree" (fun branch leaf -> function
          | Branch (x1, x2, x3) -> branch (x1, x2, x3)
          | Leaf   (x1, x2)     -> leaf (x1, x2))
      |~ case1 "Branch" (triple tree_t (option bool) tree_t) (fun (x1, x2, x3) -> Branch (x1, x2, x3))
      |~ case1 "Leaf"   (pair int32 string)                  (fun (x1, x2) -> Leaf (x1, x2))
      |> sealv)
```

## Installation and usage

You can install `ppx_irmin` using [opam](https://opam.ocaml.org/):

```
$ git clone git@github.com:CraigFe/ppx_irmin
$ opam pin --yes ./ppx_irmin
```

If you're using the [dune](https://github.com/ocaml/dune) build system, add the following field to
your `library`, `executable` or `test` stanza:

```
(preprocess (pps ppx_irmin))
```

You can now use `[@@deriving irmin]` after a type declaration in your code to automatically derive
an Irmin type witness with the same name.


## Specifics

`ppx_irmin` supports all of the type combinators exposed in the
[Irmin.Type](https://docs.mirage.io/irmin/Irmin/Type/index.html) module (basic types, variants,
records, recursive types etc.). Irmin types must fully grounded (no polymorphic type variables).

### Naming scheme
The generated witness will be called `<type-name>_t`, unless the type-name is `t`, in which case the
witness is simply `t`. This behaviour can be overridden using the `name` argument, as in:

```ocaml
type foo = string list * int32 [@@deriving irmin { name = "foo_witness" }]

(* generates the value *)
val foo_witness = Irmin.Type.(pair (list string) int32)
```

If the type references another user-defined type, `ppx_irmin` will expect the witness for that type
to use the standard naming scheme. This can be overridden using the `[@witness ...]` attribute, as in:

```ocaml
type bar = (foo [@witness foo_witness], string) result [@@deriving irmin]

(* generates the value *)
val bar_t = Irmin.Type.(result foo_witness string)
```

### Signature type definitions

The `ppx_irmin` deriver can also be used in signatures to expose the auto-generated value:

```ocaml
module Contents : sig
  type t = int32 [@@deriving irmin]

  (* exposes witness in signature *)
  val t : t Irmin.Type.t

end = struct
  type t = int32 [@@deriving irmin]

  (* generates witness value *)
  val t = Irmin.Type.int32
end

```
