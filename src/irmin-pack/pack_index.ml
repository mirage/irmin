(* Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

module type S = sig
  type t

  type key

  type value = int64 * int * char

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?shared:bool ->
    log_size:int ->
    fan_out_size:int ->
    string ->
    t

  val clear : t -> unit

  val flush : t -> unit

  val add : t -> key -> value -> unit

  val find_all : t -> key -> value list
end

module Make (K : Irmin.Hash.S) = struct
  module Key = struct
    type t = string

    let pp = Fmt.fmt "%S"

    let hash = Hashtbl.hash

    (* Hashtbl.hash uses 30 bits *)
    let hash_size = 30

    let equal x y = String.equal x y

    let encode x = x

    let encoded_size = min 8 K.hash_size

    let decode s off = String.sub s off encoded_size
  end

  let key s =
    let s = Irmin.Type.to_bin_string K.t s in
    String.sub s 0 Key.encoded_size

  module Val = struct
    type t = int64 * int * char

    let pp = Irmin.Type.(pp (triple int64 int char))

    let encode (off, len, kind) =
      Irmin.Type.(to_bin_string (triple int64 int32 char))
        (off, Int32.of_int len, kind)

    let decode s off =
      let off, len, kind =
        snd (Irmin.Type.(decode_bin (triple int64 int32 char)) s off)
      in
      (off, Int32.to_int len, kind)

    let encoded_size = (64 / 8) + (32 / 8) + 1
  end

  module Index = Index_unix.Make (Key) (Val)

  type t = Index.t

  type key = K.t

  type value = Val.t

  let v = Index.v

  let clear = Index.clear

  let flush = Index.flush

  let add t k v = Index.add t (key k) v

  let find_all t k = Index.find_all t (key k)
end
