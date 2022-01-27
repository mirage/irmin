open! Import

module type Weighted = sig
  type data
  type t

  val weight : t -> int
  val data : t -> data
  val weighted_value : data -> int -> t
end

module type S = sig
  include Irmin.Type.S

  type hash
  type kind

  val hash : t -> hash
  val kind : t -> kind

  val encode_bin :
    dict:(string -> int option) ->
    offset:(hash -> int63 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) ->
    hash:(int63 -> hash) ->
    string ->
    int ->
    int * t

  val decode_bin_length : string -> int -> int

  module Weighted : Weighted with type data = t
end

module type Sigs = sig
  module Kind : sig
    type t = Commit | Contents | Inode | Node [@@deriving irmin]

    val to_magic : t -> char
    val of_magic_exn : char -> t
    val pp : t Fmt.t
  end

  module type S = S with type kind := Kind.t

  module Make (_ : sig
    val selected_kind : Kind.t
  end) (Weighted : sig
    type 'a t

    val weight : 'a t -> int
    val data : 'a t -> 'a
    val weighted_value : 'a -> int -> 'a t
  end)
  (Hash : Irmin.Hash.S)
  (Data : Irmin.Type.S) : S with type hash = Hash.t

  module Of_contents (Hash : Irmin.Hash.S) (Contents : Irmin.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t

  module Of_commit (Hash : Irmin.Hash.S) (Commit : Irmin.Private.Commit.S) :
    S with type t = Commit.t and type hash = Hash.t
end
