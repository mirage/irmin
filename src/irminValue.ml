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

module Make (K: KEY)  (B: VALUE with module Key = K) = struct

  module Blob = B

  module Key = K

  module Keys = IrminIO.List(K)

  type revision = {
    parents : Key.t list;
    contents: Key.t;
  }

  type t =
    | Blob of Blob.t
    | Revision of revision

  let of_string str =
    Blob (B.of_string str)

  module Revision = struct

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

    let sizeof t =
      Keys.sizeof (t.contents :: t.parents)

    let read buf =
      lwt keys = Keys.read buf in
      match keys with
      | []   -> failwith "Revision.read"
      | h::t -> Lwt.return { contents = h; parents = t }

    let write buf t =
      Keys.write buf (t.contents :: t.parents)

  end

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

  let sizeof t =
    4
    + match t with
      | Blob b     -> Blob.sizeof b
      | Revision r -> Revision.sizeof r

  let read buf =
    lwt kind = IrminIO.get_uint8 buf in
    match kind with
    | 0 -> lwt b = Blob.read buf in Lwt.return (Blob b)
    | 1 -> lwt r = Revision.read buf in Lwt.return (Revision r)
    | _ -> failwith "Value.of_cstruct"

  let write buf t =
    let kind = match t with
      | Blob _     -> 0
      | Revision _ -> 1 in
    lwt () = IrminIO.set_uint8 buf kind in
    match t with
    | Blob b     -> Blob.write buf b
    | Revision r -> Revision.write buf r

  let pred = function
    | Revision { parents } -> parents
    | Blob _               -> []

  let key = function
    | Blob b     -> B.key b
    | Revision r -> Key.concat (r.contents :: r.parents)

  let merge merge_keys v1 v2 =
    if v1 = v2 then Some v1
    else match v1, v2 with
      | Blob b1    , Blob b2     ->
        begin match B.merge merge_keys b1 b2 with
          | None   -> None
          | Some b -> Some (Blob b)
        end
      | Revision r1, Revision r2 ->
        let parents = [key v1; key v2] in
        begin match merge_keys r1.contents r2.contents with
          | None          -> None
          | Some contents -> Some (Revision { parents; contents })
        end
      | _ -> None

end

type blob = B of string

module Blob (K: KEY) = struct

  include IrminIO.String(struct
      type t = blob
      let to_string (B s) = s
      let of_string s = B s
    end)

  let merge _ b1 b2 = None

  let pred _ = []

  let succ _ = []

  let key (B str) =
    K.of_string str

  module Key = K

  let of_string str = B str

end
