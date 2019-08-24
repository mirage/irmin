type ('a, 'b) t = (module S.SERIALIZE with type key = 'a and type t = 'b)

module Default (K : Type.S) (V : Type.S) : S.SERIALIZE with type t = V.t and type key = K.t = struct
  type t = V.t
  type key = K.t

  let encode_bin (_key : K.t) = Type.encode_bin V.t
  let decode_bin (_key : K.t) = Type.decode_bin V.t
  let size_of (_key : K.t) = Type.size_of V.t
end

let to_bin_string (type a b) (t : (a, b) t) (key : a) (x : b) =
  let (module T) = t in
  let seq = T.encode_bin key x in
  let len =
    match T.size_of key x with None -> 1024 | Some n -> n
  in
  let buf = Buffer.create len in
  seq (Buffer.add_string buf);
  Buffer.contents buf

let of_bin_string (type a b) (t : (a, b) t) (key : a) buf : (b, [`Msg of string]) result =
  let (module T) = t in
  let last, v = T.decode_bin key buf 0 in
  assert (last = String.length buf);
  Ok v
