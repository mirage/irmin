open! Import

module type S = sig
  include Irmin.Key.S

  val null : t
  val unfindable_of_hash : hash -> t
end

module type Sigs = sig
  type 'hash t
  (** The type of {i keys} referencing values stored in the [irmin-pack]
      backend. *)

  (** The internal state of a key (read with {!inspect}).

      Invariant: keys of the form {!Indexed} always reference values that have
      entries in the index (as otherwise these keys could not be dereferenced). *)
  type 'hash state =
    | Direct of { hash : 'hash; offset : int63; length : int }
        (** A "direct" pointer to a value stored at [offset] in the pack-file
            (with hash [hash] and length [length]). Such keys can be
            dereferenced from the store with a single IO read, without needing
            to consult the index.

            They are built in-memory (e.g. after adding a fresh value to the
            pack file), but have no corresponding encoding format, as the pack
            format keeps length information with the values themselves.

            When decoding a inode, which references its children as single
            offsets, we fetch the length information of the child at the same
            time as fetching its hash (which we must do anyway in order to do an
            integrity check), creating keys of this form. *)
    | Indexed of 'hash
        (** A pointer to an object in the pack file that is indexed. Reading the
            object necessitates consulting the index, after which the key can be
            promoted to {!Direct}.

            Such keys result from decoding pointers to other store objects
            (nodes or commits) from commits or from the branch store. *)

  val inspect : 'hash t -> 'hash state
  val v_direct : hash:'h -> offset:int63 -> length:int -> 'h t
  val v_indexed : 'h -> 'h t
  val promote_exn : 'h t -> offset:int63 -> length:int -> unit

  module type S = sig
    type hash

    (** @inline *)
    include S with type t = hash t and type hash := hash
  end

  module Make (Hash : Irmin.Hash.S) : S with type hash = Hash.t

  module type Store_spec = sig
    type ('h, _) contents_key = 'h t
    type 'h node_key = 'h t
    type 'h commit_key = 'h t
  end

  module Store_spec : Store_spec
end
