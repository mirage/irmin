(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Ir_merge.OP

module Log = Log.Make(struct let section = "CONTENTS" end)

module type S = sig
  include Tc.I0
  type origin
  module Origin: Ir_origin.S with type t = origin
  val merge: (t, origin) Ir_merge.t
end

module type STORE = sig
  include Ir_ao.STORE
  val merge: t -> (key, origin) Ir_merge.t
  module Key: Ir_hash.S with type t = key
  module Val: S with type t = value and type origin = origin
end

module Json (O: Ir_origin.S) = struct

  module Origin = O

  let rec encode t: Ezjsonm.t =
    match t with
    | `Null
    | `Bool _
    | `Float _  -> t
    | `String s -> Ezjsonm.encode_string s
    | `A l      -> `A (List.rev_map encode l)
    | `O l      -> `O (List.rev_map (fun (k,v) -> k, encode v) l)

  let rec decode t: Ezjsonm.t =
    match t with
    | `Null
    | `Bool _
    | `Float _
    | `String _ -> t
    | `A l      -> `A (List.rev_map decode l)
    | `O l      ->
      match Ezjsonm.decode_string t with
      | Some s -> `String s
      | None   -> `O (List.rev_map (fun (k,v) -> k, encode v) l)

  module S = struct

    type t =
      [ `Null
      | `Bool of bool
      | `Float of float
      | `String of string
      | `A of t list
      | `O of (string * t) list ]

    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)

    let rec to_sexp t =
      let open Sexplib.Type in
      match t with
      | `Null -> List []
      | `Bool b -> Atom (string_of_bool b)
      | `Float f -> Atom (string_of_float f)
      | `String s -> Atom s
      | `A tl -> List (List.map to_sexp tl)
      | `O dl ->
        let aux (k, v) = List [ Atom k; to_sexp v ] in
        List (List.map aux dl)

    let to_json = encode

    let of_json = decode

    let to_string t =
      Ezjsonm.to_string (to_json t)

    let of_string s =
      of_json (Ezjsonm.from_string s)

    let write t buf =
      let str = to_string t in
      let len = String.length str in
      Cstruct.blit_from_string str 0 buf 0 len;
      Cstruct.shift buf len

    let read buf =
      Mstruct.get_string buf (Mstruct.length buf)
      |> of_string

    let size_of t =
      let str = to_string t in
      String.length str

  end

  include S
  type origin = Origin.t

  let rec merge origin ~old x y =
    match old, x, y with
    | `O old, `O x, `O y ->
      Ir_merge.alist (module Tc.S) (module S) merge origin ~old x y >>| fun x ->
      ok (`O x)
    | _ -> conflict "JSON"

end

module String (O: Ir_origin.S) = struct
  include Tc.S
  module Origin = O
  type origin = O.t
  let merge = Ir_merge.default (module Tc.S)
end

module Cstruct (O: Ir_origin.S) = struct
  module S = struct
    type t = Cstruct.t
    module Origin = O
    type origin = O.t

    let to_hex t =
      let buf = Buffer.create (Cstruct.len t) in
      Cstruct.hexdump_to_buffer buf t;
      Buffer.contents buf

    let hash = Hashtbl.hash
    let equal x y = Cstruct.to_bigarray x = Cstruct.to_bigarray y
    let compare x y =
      Pervasives.compare (Cstruct.to_bigarray x) (Cstruct.to_bigarray y)

    let to_sexp t = Sexplib.Type.Atom (to_hex t)
    let to_json t = Cstruct.to_string t |> Ezjsonm.encode_string
    let of_json j = Ezjsonm.decode_string_exn j |> Cstruct.of_string
    let size_of t = Cstruct.len t
    let read b = Mstruct.to_cstruct b

    let write t buf =
      let len = Cstruct.len t in
      Cstruct.blit t 0 buf 0 len;
      Cstruct.shift buf len
  end
  include S
  let merge = Ir_merge.default (module S)
end

module type MAKER =
  functor (K: Ir_hash.S) ->
  functor (V: S) ->
    STORE with type key = K.t and type value = V.t and type origin = V.origin

module Make (S: Ir_ao.MAKER) (K: Ir_hash.S) (V: S) = struct

  include S(K)(V)(V.Origin)

  module Key  = K
  module Val = V

  let merge t origin =
    Ir_merge.biject'
      (module V) (module K) V.merge (add t origin) (read_exn t origin)
      origin

end

module Rec (S: STORE) = struct
  include S.Key
  module Origin = S.Val.Origin
  type origin = S.origin
  let merge = S.merge (S.create ())
end
