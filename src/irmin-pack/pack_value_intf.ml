open! Import

(** This module defines abstractions over entries in the pack file, which are
    encoded as the following sequence:

    - the (fixed-length) hash of the data stored in this entry;
    - the {i kind} of data being stored (contents, nodes, blob etc.);
    - the data itself, optionally with a length header that contains the encoded
      size of the data section (excluding the header itself). *)

type 'kind length_header =
  [ `Never | `Sometimes of 'kind -> [ `Varint | `Int32_be ] option ]
(** The type of descriptions of length header formats for the {i data} sections
    of pack entries.

    NOTE: [`Never] is equivalent to [`Sometimes (Fun.const None)], but enables a
    more efficient store implementation. *)

module type S = sig
  include Irmin.Type.S

  type hash
  type key
  type kind

  val hash : t -> hash
  val kind : t -> kind
  val length_header : kind length_header

  val encode_bin :
    dict:(string -> int option) ->
    offset_of_key:(key -> int63 option) ->
    hash ->
    t Irmin.Type.encode_bin

  val decode_bin :
    dict:(int -> string option) ->
    key_of_offset:(int63 -> key) ->
    key_of_hash:(hash -> key) ->
    t Irmin.Type.decode_bin

  val decode_bin_length : string -> int -> int
end

module type Persistent = sig
  type hash

  include S with type hash := hash and type key = hash Pack_key.t
end

module type T = sig
  type t
end

module type Sigs = sig
  module Kind : sig
    type t =
      | Commit_v0
      | Commit_v1
      | Contents
      | Inode_v0_unstable
      | Inode_v0_stable
      | Inode_v1_root
      | Inode_v1_nonroot
    [@@deriving irmin]

    val to_magic : t -> char
    val of_magic_exn : char -> t
    val pp : t Fmt.t
  end

  module type S = S with type kind := Kind.t
  module type Persistent = Persistent with type kind := Kind.t

  module Of_contents (_ : sig
    val contents_length_header : [ `Varint | `None ]
  end)
  (Hash : Irmin.Hash.S)
  (Key : T)
  (Contents : Irmin.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t and type key = Key.t

  module Of_commit
      (Hash : Irmin.Hash.S)
      (Key : T)
      (Commit : Irmin.Commit.Generic_key.S
                  with type node_key = Key.t
                   and type commit_key = Key.t) :
    S with type t = Commit.t and type hash = Hash.t and type key = Key.t
end
