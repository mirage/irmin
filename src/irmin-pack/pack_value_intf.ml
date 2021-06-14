open! Import

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
end

module type Sigs = sig
  module Kind : sig
    type t = Commit | Contents | Inode | Node [@@deriving irmin]

    val to_magic : t -> char
    val pp : t Fmt.t
  end

  module type S = S with type kind := Kind.t

  module Make (_ : sig
    val selected_kind : Kind.t
  end)
  (Hash : Irmin.Hash.S)
  (Data : Irmin.Type.S) : S with type hash = Hash.t

  module Of_contents (Hash : Irmin.Hash.S) (Contents : Irmin.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t

  module Of_commit (Hash : Irmin.Hash.S) (Commit : Irmin.Private.Commit.S) :
    S with type t = Commit.t and type hash = Hash.t
end
