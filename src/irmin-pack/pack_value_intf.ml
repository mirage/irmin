open! Import

module type S = sig
  include Irmin.Type.S

  type hash
  type key
  type kind

  val hash : t -> hash
  val kind : t -> kind

  val encode_bin :
    dict:(string -> int option) ->
    offset_of_key:(key -> int63 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) ->
    key_of_offset:(int63 -> key) ->
    string ->
    int ->
    int * t

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
  module Kind = Pack_value_kind (* XXX: drop alias *)

  module type S = S with type kind := Kind.t
  module type Persistent = Persistent with type kind := Kind.t

  module Make (_ : sig
    val selected_kind : Kind.t
  end)
  (Hash : Irmin.Hash.S)
  (Key : T)
  (Data : Irmin.Type.S) : S with type hash = Hash.t and type key = Key.t

  module Of_contents
      (Hash : Irmin.Hash.S)
      (Key : T)
      (Contents : Irmin.Contents.S) :
    S with type t = Contents.t and type hash = Hash.t and type key = Key.t

  module Of_commit (Hash : Irmin.Hash.S) (Key : T) (Commit : Irmin.Commit.S) :
    S with type t = Commit.t and type hash = Hash.t and type key = Key.t
end
