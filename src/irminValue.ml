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

  let debug fmt = IrminMisc.debug "VALUE" fmt

  module Blob = B

  module Key = K

  module Keys = IrminIO.List(K)

  type revision = {
    parents : Key.Set.t;
    contents: Key.t option;
  }

  type t =
    | Blob of Blob.t
    | Revision of revision

  let is_blob = function
    | Blob _     -> true
    | Revision _ -> false

  let of_string str =
    Blob (B.of_string str)

  let to_string = function
    | Blob b     -> B.to_string b
    | Revision _ -> None

  let revision contents parents =
    Revision { contents; parents }

  module Revision = struct

    let debug fmt = IrminMisc.debug "REVISION" fmt

    let pretty r =
      Printf.sprintf "[parents:%s | contents:%s]"
        (K.Set.pretty r.parents)
        (match r.contents with None -> "" | Some c -> K.pretty c)

    let of_json (json:IrminJSON.t) = match json with
      | `O list ->
        let contents =
          try Some (List.assoc "contents" list)
          with Not_found -> None in
        let contents = match contents with
          | None   -> None
          | Some j -> Some (K.of_json j) in
        let parents =
          try Some (List.assoc "parents" list)
          with Not_found -> None in
        let parents = match parents with
          | None   -> []
          | Some j -> IrminJSON.to_list K.of_json j in
        let parents = Key.Set.of_list parents in
        { parents; contents }
      | _ -> IrminIO.parse_error "Revision.of_json"

    let to_json r =
      let parents = match Key.Set.to_list r.parents with
        | []      -> []
        | parents -> [ "parents", IrminJSON.of_list K.to_json parents ] in
      let contents = match r.contents with
        | None    -> []
        | Some c  -> [ "contents", K.to_json c ] in
      `O (parents @ contents)

    module CP = IrminIO.Pair(IrminIO.Option(Key))(Key.Set)

    let sizeof t =
      debug "sizeof";
      CP.sizeof (t.contents, t.parents)

    let get buf =
      debug "get";
      let contents, parents = CP.get buf in
      { contents; parents }

    let set buf t =
      debug "set %s" (pretty t);
      CP.set buf (t.contents, t.parents)

    let equal r1 r2 =
      CP.equal (r1.contents, r1.parents) (r2.contents, r2.parents)

    let compare r1 r2 =
      CP.compare (r1.contents, r1.parents) (r2.contents, r2.parents)

  end

  let hash = Hashtbl.hash

  let pretty = function
    | Blob b     -> "B" ^ Blob.pretty b
    | Revision r -> "R" ^ Revision.pretty r

  let to_json = function
    | Blob b     -> `O [ "blob"    , Blob.to_json b ]
    | Revision r -> `O [ "revision", Revision.to_json r ]

  let of_json = function
    | `O [ "blob"    , json ] -> Blob (Blob.of_json json)
    | `O [ "revision", json ] -> Revision (Revision.of_json json)
    | _ -> IrminIO.parse_error "value_of_json"

  let sizeof t =
    debug "sizeof";
    1
    + match t with
      | Blob b     -> Blob.sizeof b
      | Revision r -> Revision.sizeof r

  let get buf =
    debug "get";
    let kind = IrminIO.get_uint8 buf in
    match kind with
    | 0 -> Blob (Blob.get buf)
    | 1 -> Revision (Revision.get buf)
    | _ -> IrminIO.parse_error_buf buf "Value.of_cstruct"

  let set buf t =
    debug "set %s" (pretty t);
    let kind = match t with
      | Blob _     -> 0
      | Revision _ -> 1 in
    IrminIO.set_uint8 buf kind;
    IrminIO.dump_buffer buf;
    let () = match t with
      | Blob b     -> Blob.set buf b
      | Revision r -> Revision.set buf r in
    IrminIO.dump_buffer buf

  let parents = function
    | Revision { parents; _ } -> parents
    | Blob _                  -> Key.Set.empty

  let contents = function
    | Revision { contents; _ } -> contents
    | Blob _                   -> None

  let key = function
    | Blob b     -> B.key b
    | Revision r ->
      match r.contents with
      | None   -> Key.concat (Key.Set.to_list r.parents)
      | Some c -> Key.concat (c :: Key.Set.to_list r.parents)

  let merge merge_keys v1 v2 =
    if v1 = v2 then Some v1
    else match v1, v2 with
      | Blob b1    , Blob b2     ->
        begin match B.merge merge_keys b1 b2 with
          | None   -> None
          | Some b -> Some (Blob b)
        end
      | Revision r1, Revision r2 ->
        let parents = Key.Set.of_list [key v1; key v2] in
        let revision contents =
          Some (Revision { parents; contents }) in
        begin match r1.contents, r2.contents with
          | None   , None    -> revision None
          | Some _ , None    -> revision r1.contents
          | None   , Some _  -> revision r2.contents
          | Some c1, Some c2 ->
            let contents = merge_keys c1 c2 in
            match contents with
            | None           -> None
            | Some _         -> revision contents
        end
      | Blob _    , Revision _
      | Revision _, Blob _ -> None

  let equal v1 v2 =
    match v1, v2 with
    | Blob b1    , Blob b2     -> B.equal b1 b2
    | Revision r1, Revision r2 -> Revision.equal r1 r2
    | Blob _     , Revision _
    | Revision _ , Blob _      -> false

  let compare v1 v2 =
    match v1, v2 with
    | Blob _     , Revision _  -> 1
    | Revision _ , Blob _      -> -1
    | Blob b1    , Blob b2     -> B.compare b1 b2
    | Revision r1, Revision r2 -> Revision.compare r1 r2

end

type blob = B of string

module Blob (K: KEY) = struct

  include IrminIO.String(struct
      let name = "BLOB"
      type t = blob
      let to_string (B s) = s
      let of_string s = B s
    end)

  let merge _ _ _ = None

  let parents _ = K.Set.empty

  let contents _ = None

  let is_blob _ = true

  let key (B str) =
    K.of_string str

  module Key = K

  let of_string str = B str

  let to_string (B str) = Some str

  let revision _ =
    failwith "Blob.revision"

  let equal (B b1) (B b2) = String.compare b1 b2 = 0

end
