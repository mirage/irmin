open! Import
include Pack_value_intf

module Kind = struct
  type t = Commit | Contents | Inode | Node

  let to_magic = function
    | Commit -> 'C'
    | Contents -> 'B'
    | Inode -> 'I'
    | Node -> 'N'

  let of_magic_exn = function
    | 'C' -> Commit
    | 'B' -> Contents
    | 'I' -> Inode
    | 'N' -> Node
    | c -> Fmt.failwith "Kind.of_magic: unexpected magic char %C" c

  let t = Irmin.Type.(map char) of_magic_exn to_magic
  let pp = Fmt.using to_magic Fmt.char
end

type ('h, 'a) value = { hash : 'h; kind : Kind.t; v : 'a } [@@deriving irmin]

module type S = S with type kind := Kind.t

module Make (Config : sig
  val selected_kind : Kind.t
end) (Weighted : sig
  type 'a t

  val weight : 'a t -> int
  val data : 'a t -> 'a
  val weighted_value : 'a -> int -> 'a t
end)
(Hash : Irmin.Hash.S)
(Data : Irmin.Type.S) =
struct
  module Hash = Irmin.Hash.Typed (Hash) (Data)

  type t = Data.t [@@deriving irmin]
  type hash = Hash.t

  let hash = Hash.hash
  let kind = Config.selected_kind
  let value = [%typ: (Hash.t, Data.t) value]
  let encode_value = Irmin.Type.(unstage (encode_bin value))
  let decode_value = Irmin.Type.(unstage (decode_bin value))
  let encode_bin ~dict:_ ~offset:_ v hash = encode_value { kind; hash; v }

  let decode_bin ~dict:_ ~hash:_ s off =
    let len, t = decode_value s off in
    (len, t.v)

  let decode_bin_length =
    match Irmin.Type.(Size.of_encoding value) with
    | Unknown ->
        Fmt.failwith "Type must have a recoverable encoded length: %a"
          Irmin.Type.pp_ty t
    | Static n -> fun _ _ -> n
    | Dynamic f -> f

  let kind _ = Config.selected_kind

  module Weighted = struct
    type data = Data.t
    type t = data Weighted.t

    let weight = Weighted.weight
    let data = Weighted.data
    let weighted_value = Weighted.weighted_value
  end
end

module Of_contents =
  Make
    (struct
      let selected_kind = Kind.Contents
    end)
    (struct
      type 'a t = 'a * int

      let weight (_, w) = w
      let data (d, _) = d
      let weighted_value d w = (d, w)
    end)

module Of_commit =
  Make
    (struct
      let selected_kind = Kind.Commit
    end)
    (struct
      type 'a t = 'a

      let weight _ = 1000
      let data d = d
      let weighted_value d _ = d
    end)
