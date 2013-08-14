(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open IrminTypes

module type S = sig
  include VALUE
  include IO with type t := t
end

type blob = B of string

module Blob(C: CHANNEL) = IrminIO.String(C)(struct
    type t = blob
    let to_string (B s) = s
    let of_string s = B s
  end)

type 'a revision = {
  parents : 'a list;
  contents: 'a;
}

module Revision (C: CHANNEL) (K: IrminKey.S with type channel = C.t) = struct

  module Ks = IrminIO.List(C)(K)

  type t = K.t revision

  type channel = C.t

  let pretty r =
    Printf.sprintf "[%s => %s]"
      (String.concat "-" (List.map K.pretty r.parents))
      (K.pretty r.contents)

  let of_json (json:IrminJSON.t) = match json with
    | `O [ ("parents", parents); ("contents", contents) ] ->
      let parents = IrminJSON.to_list K.of_json parents in
      let parents = List.sort compare parents in
      let contents = K.of_json contents in
      { parents; contents }
    | _ -> failwith "Revision.of_json"

  let to_json r =
    let parents = IrminJSON.of_list K.to_json r.parents in
    let contents = K.to_json r.contents in
    `O [ ("parents", parents); ("contents", contents) ]

  let read fd =
    lwt keys = Ks.read fd in
    match keys with
    | []   -> failwith "Revision.read"
    | h::t -> Lwt.return { contents = h; parents = t }

  let write fd r =
    Ks.write fd (r.contents :: r.parents)

end

type 'a t =
  | Blob of blob
  | Revision of 'a revision

type 'a value = 'a t

module Make (C: CHANNEL) (K: IrminKey.S with type channel = C.t) = struct

  module Blob = Blob(C)
  module Revision = Revision(C)(K)

  type t = K.t value

  type channel = C.t

  let pretty = function
    | Blob b     -> "B" ^ Blob.pretty b
    | Revision r -> "R" ^ Revision.pretty r

  let to_json = function
    | Blob b     -> `O [ "blob"    , Blob.to_json b ]
    | Revision r -> `O [ "revision", Revision.to_json r ]

  let of_json = function
    | `O [ "blob"    , json ] -> Blob (Blob.of_json json)
    | `O [ "revision", json ] -> Revision (Revision.of_json json)
    | _ -> failwith "value_of_json"

  cstruct hdr {
      uint8_t kind
    } as big_endian

  let read fd =
    lwt buf = C.read fd sizeof_hdr in
    let kind = get_hdr_kind buf in
    match kind with
    | 0 -> lwt b = Blob.read fd in Lwt.return (Blob b)
    | 1 -> lwt r = Revision.read fd in Lwt.return (Revision r)
    | _ -> failwith "Value.of_cstruct"

  let write fd t =
    let kind, cont = match t with
      | Blob b     -> 0, fun () -> Blob.write fd b
      | Revision r -> 1, fun () -> Revision.write fd r in
    let buf = Cstruct.create sizeof_hdr in
    set_hdr_kind buf kind;
    lwt () = C.write fd buf in
    cont ()

end
