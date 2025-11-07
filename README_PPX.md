## `ppx_irmin`

PPX extension for automatically generating Irmin type representations.

### Overview

`ppx_irmin` automatically generates Irmin type representations (values of type
`_ Irmin.Type.t`) corresponding to type declarations in your code. For example:

```ocaml
type 'a tree =
  | Branch of tree * bool option * tree
  | Leaf of 'a [@@deriving irmin]
```

will be expanded to:

```ocaml
type 'a tree = (* as above *)

let tree_t leaf_t =
  let open Irmin.Type in
  mu (fun tree_t ->
      variant "tree" (fun branch leaf -> function
          | Branch (x1, x2, x3) -> branch (x1, x2, x3)
          | Leaf   x1           -> leaf x1
      |~ case1 "Branch" (triple tree_t (option bool) tree_t) (fun (x1, x2, x3) -> Branch (x1, x2, x3))
      |~ case1 "Leaf"   leaf_t                               (fun x1 -> Leaf x1)
      |> sealv)
```

Type representations can also be derived inline using the `[%typ: <core-type>]`
extension point.

### Installation and Usage

`ppx_irmin` may be installed via [opam](https://opam.ocaml.org/):

```
opam install ppx_irmin
```

If you're using the [Dune](https://github.com/ocaml/dune) build system, add the
following field to your `library`, `executable`, or `test` stanza:

```
(preprocess (pps ppx_irmin))
```

You can now use `[@@deriving irmin]` after a type declaration in your code to
automatically derive an Irmin type representation with the same name.

### Specifics

`ppx_irmin` supports all of the type combinators exposed in the
[Irmin.Type](https://docs.mirage.io/irmin/Irmin/Type/index.html) module (basic
types, records, variants (plain and closed polymorphic), recursive types, etc.).
Types with parameters will result in parameterised representations (i.e., type
`'a t` is generated a representation of type `'a Type.t -> 'a t Type.t`).

To supply base representations from a module other than `Irmin.Type` (such as
when `Irmin.Type` is aliased to a different module path), the `lib` argument
can be passed to `@@deriving irmin`:

```ocaml
type foo = unit [@@deriving irmin { lib = Some "Mylib.Types" }]

(* generates the value *)
val foo_t = Mylib.Types.unit
```

This argument can also be passed as a command-line option (i.e., `--lib
Mylib.Types`, with `--lib ''` interpreted as the current module).

#### Naming Scheme

The generated type representation will be called `<type-name>_t`, unless the
type-name is `t`, in which case the representation is simply `t`. This
behaviour can be overridden using the `name` argument, as in:

```ocaml
type foo = string list * int32 [@@deriving irmin { name = "foo_repr" }]

(* generates the value *)
val foo_repr = Irmin.Type.(pair (list string) int32)
```

If the type contains an abstract type, `ppx_irmin` will expect to find a
corresponding type representation using its own naming rules. This can be
overridden using the `[@repr ...]` attribute, as in:

```ocaml
type bar = (foo [@repr foo_repr], string) result [@@deriving irmin]

(* generates the value *)
val bar_t = Irmin.Type.(result foo_repr string)
```

Built-in abstract types such as `unit` are assumed to be represented in
`Irmin.Type`. This behaviour can be overridden with the `[@nobuiltin]`
attribute:

```ocaml
type t = unit [@nobuiltin] [@@deriving irmin]

(* generates the value *)
let t = unit_t (* not [Irmin.Type.unit] *)
```

#### Signature Type Definitions

The `ppx_irmin` deriver can also be used in signatures to expose the
auto-generated value:

```ocaml
module Contents : sig
  type t = int32 [@@deriving irmin]

  (* exposes repr in signature *)
  val t : t Irmin.Type.t

end = struct
  type t = int32 [@@deriving irmin]

  (* generates repr value *)
  let t = Irmin.Type.int32
end
```
