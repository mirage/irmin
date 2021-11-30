open! Import

type length_header = [ `Varint ] option

module type S = sig
  include Irmin.Type.S

  type hash
  type key
  type kind

  val hash : t -> hash
  val kind : t -> kind
  val length_header : [ `Never | `Sometimes of kind -> [ `Varint ] option ]

  val encode_bin :
    dict:(string -> int option) ->
    offset:(key -> int63 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int63 -> key) -> string -> int ref -> t

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
    type t = Commit | Contents | Inode | Node [@@deriving irmin]

    val all : t list
    val to_enum : t -> int
    val to_magic : t -> char
    val of_magic_exn : char -> t
    val pp : t Fmt.t
  end

  module type S = S with type kind := Kind.t
  module type Persistent = Persistent with type kind := Kind.t

  module Make (_ : sig
    val selected_kind : Kind.t
    val length_header : length_header
  end)
  (Hash : Irmin.Hash.S)
  (Key : T)
  (Data : Irmin.Type.S) : S with type hash = Hash.t and type key = Key.t

  module Of_contents (_ : sig
    val contents_length_header : length_header
  end)
  (Hash : Irmin.Hash.S)
  (Key : T)
  (Contents : Irmin.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t and type key = Key.t

  module Of_commit (Hash : Irmin.Hash.S) (Key : T) (Commit : Irmin.Commit.S) :
    S with type t = Commit.t and type hash = Hash.t and type key = Key.t
end
