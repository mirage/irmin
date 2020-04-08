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
  include Index.S with type value = int64 * int * char

  val find : t -> key -> value option

  val add : t -> key -> value -> unit

  val close : t -> unit

  module Stats = Index.Stats
end

module Make (K : Irmin.Hash.S) = struct
  module Key = struct
    type t = K.t

    let pp ppf t = Irmin.Type.pp K.t ppf t

    let hash t = Irmin.Type.short_hash K.t t

    let hash_size = 30

    let equal x y = Irmin.Type.equal K.t x y

    let encode x = Irmin.Type.to_bin_string K.t x

    let encoded_size = K.hash_size

    let decode s off =
      let _, v = Irmin.Type.decode_bin ~headers:false K.t s off in
      v
  end

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

  module Stats = Index.Stats
  module Index = Index_unix.Make (Key) (Val)
  include Index

  let add t k v = replace t k v

  let find t k = match find t k with exception Not_found -> None | h -> Some h
end
