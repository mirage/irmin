open! Import
include Pack_value_intf

module Kind = struct
  type t = Commit | Contents | Inode | Node

  let to_magic = function
    | Commit -> 'C'
    | Contents -> 'B'
    | Inode -> 'I'
    | Node -> 'N'

  let of_magic = function
    | 'C' -> Commit
    | 'B' -> Contents
    | 'I' -> Inode
    | 'N' -> Node
    | c -> Fmt.failwith "Kind.of_magic: unexpected magic char %C" c

  let t = Irmin.Type.(map char) of_magic to_magic
  let pp = Fmt.using to_magic Fmt.char
end

type ('h, 'a) value = { hash : 'h; kind : Kind.t; v : 'a } [@@deriving irmin]

module type S = S with type kind := Kind.t

module Make (Config : sig
  val selected_kind : Kind.t
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
end

module Of_contents = Make (struct
  let selected_kind = Kind.Contents
end)

module Of_commit = Make (struct
  let selected_kind = Kind.Commit
end)
