type t = value_length:int -> Pack_value.Kind.t -> bool
(** The type of configurations for [irmin-pack]'s indexing strategy, which
    dictates whether or not newly-appended pack entries should also be added
    to the index. Strategies are parameterised over:

    - the length of the binary encoding of the {i object} inside the pack
      entry (i.e. not accounting for the encoded hash and kind character);
    - the kind of the pack object having been added.

    Indexing more than the {!minimal} strategy only impacts performance and
    not correctness: more indexing results in a larger index and a smaller
    pack file. *)

val always : t
(** The strategy that indexes all objects. *)

val minimal : t
(** The strategy that indexes as few objects as possible while still
    maintaing store integrity. *)

val minimal_with_contents : t
(** The strategy that is similar to the minimal strategy but it also indexes
    contents objects. *)

val default : t
(** [default] is the indexing strategy used by [irmin-pack] instances that
    do not explicitly set an indexing strategy in {!Irmin_pack.config}.
    Currently set to {!always}. *)
