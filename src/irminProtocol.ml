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

open Lwt
open IrminTypes

type key = K of string

type blob = B of string

type revision = {
  parents : key list;
  contents: key;
}

type value =
  | Blob of blob
  | Revision of revision

type tag = T of string

type action =
  | Pull_keys
  | Pull_tags
  | Push_keys
  | Push_tags
  | Watch


module type S = sig

  type channel

  module Key: sig
    include KEY
    include IO with type t := t
                and type channel := channel
  end

  module Value: sig
    include VALUE
    include IO with type t := t
                and type channel := channel
  end

  module Tag: sig
    include TAG
    include IO with type t := t
                and type channel := channel
  end

  module Client: REMOTE with type channel := channel
                         and type key = Key.t
                         and type tag = Tag.t

  module Server: sig
    val dispatch: channel -> unit Lwt.t
  end

end

module Make (C: CHANNEL) = struct

  type channel = C.t

  module type StringArg = sig
    type t
    val to_string: t -> string
    val of_string: string -> t
  end

  module StringBase(A: StringArg) = struct

    type t = A.t

    type channel = C.t

    let to_string s =
      Printf.sprintf "%S" (A.to_string s)

    let to_json t =
      IrminJSON.of_string (A.to_string t)

    let of_json j =
      A.of_string (IrminJSON.to_string j)

    cstruct hdr {
        uint32_t length
      } as big_endian

    let read fd =
      lwt buf = C.read fd sizeof_hdr in
      let len = Int32.to_int (get_hdr_length buf) in
      lwt str = C.read_string fd len in
      return (A.of_string str)

    let write fd t =
      let str = A.to_string t in
      let len = String.length str in
      let buf = Cstruct.create sizeof_hdr in
      set_hdr_length buf (Int32.of_int len);
      lwt () = C.write fd buf in
      C.write_string fd str

  end

  module MakeList (E: IO with type channel = C.t) = struct

    type t = E.t list

    type channel = C.t

    let to_string t =
      String.concat "\n" (List.rev (List.rev_map E.to_string t))

    let to_json t =
      `A (List.rev (List.rev_map E.to_json t))

    let of_json = function
      | `A l -> List.rev (List.rev_map E.of_json l)
      | _    -> failwith "MakeList.of_json"

    cstruct hdr {
        uint32_t keys
      } as big_endian

    let read fd =
      lwt buf = C.read fd sizeof_hdr in
      let keys = Int32.to_int (get_hdr_keys buf) in
      let rec aux acc i =
        if i <= 0 then return (List.rev acc)
        else
          lwt t = E.read fd in
          aux (t :: acc) (i-1) in
      aux [] keys

    let write fd t =
      Lwt_list.iter_s (E.write fd) t

  end

  module MakeProduct
      (K: IO with type channel = C.t)
      (V: IO with type channel = C.t)
  = struct

    type t = K.t * V.t

    type channel = C.t

    let to_string (key, value) =
      Printf.sprintf "%s:%s" (K.to_string key) (V.to_string value)

    let to_json (key, value) =
      `O [ ("tag", K.to_json key);
           ("key", V.to_json value)]

    let of_json = function
      | `O l ->
        let key =
          try List.assoc "tag" l
          with Not_found -> failwith "MakeProduct.of_json: missing tag" in
        let value =
          try List.assoc "key" l
          with Not_found -> failwith "MakeProduct.of_json: missing key" in
        (K.of_json key, V.of_json value)
      | _ -> failwith "Product.of_json: not an object"

    let read fd =
      lwt tag = K.read fd in
      lwt key = V.read fd in
      return (tag, key)

    let write fd (key, value) =
      lwt () = K.write fd key in
      V.write fd value

  end

  module Blob = struct

    module S = StringBase(struct
        type t = blob
        let to_string (B s) = s
        let of_string s = B s
      end)

    include S

  end

  module Key = struct

    type t = key

    type channel = C.t

    let to_string (K k) =
      Printf.sprintf "%s" k

    let to_json (K k) =
      IrminJSON.of_string k

    let of_json j =
      K (IrminJSON.to_string j)

    let create value =
      let str = Marshal.to_string value [] in
      K (IrminMisc.sha1 str)

    let key_length = 20

    let length (K _) = key_length

    let read fd =
      lwt key = C.read_string fd key_length in
      return (K key)

    let write fd (K k) =
      C.write_string fd k

  end

  module Keys = MakeList(Key)

  module KeyPair = MakeProduct(Key)(Key)

  module KeyPairs = MakeList(KeyPair)

  module Revision = struct

    type t = revision

    type channel = C.t

    let to_string r =
      Printf.sprintf "[%s => %s]"
        (String.concat "-" (List.map Key.to_string r.parents))
        (Key.to_string r.contents)

    let of_json (json:IrminJSON.t) = match json with
      | `O [ ("parents", parents); ("contents", contents) ] ->
        let parents = IrminJSON.to_list Key.of_json parents in
        let parents = List.sort compare parents in
        let contents = Key.of_json contents in
        { parents; contents }
      | _ -> failwith "Revision.of_json"

    let to_json r =
      let parents = IrminJSON.of_list Key.to_json r.parents in
      let contents = Key.to_json r.contents in
      `O [ ("parents", parents); ("contents", contents) ]

    let read fd =
      lwt keys = Keys.read fd in
      match keys with
      | []   -> failwith "Revision.read"
      | h::t -> return { contents = h; parents = t }

    let write fd r =
      Keys.write fd (r.contents :: r.parents)

  end

  module Value = struct

    type t = value

    type channel = C.t

    let to_string = function
      | Blob b     -> Blob.to_string b
      | Revision r -> Revision.to_string r

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
      | 0 -> lwt b = Blob.read fd in return (Blob b)
      | 1 -> lwt r = Revision.read fd in return (Revision r)
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

  module Tag = StringBase(struct
      type t = tag
      let to_string (T s) = s
      let of_string s = T s
    end)

  module Tags = MakeList(Tag)

  module TagKey = MakeProduct(Tag)(Key)

  module TagKeys = MakeList(TagKey)

  module Action = struct

    type t = action

    let to_string = function
      | Pull_keys -> "pull-keys"
      | Pull_tags -> "pull-tags"
      | Push_keys -> "push-keys"
      | Push_tags -> "push-tags"
      | Watch     -> "watch"

    let to_json t =
      IrminJSON.of_string (to_string t)

    let of_json j = match IrminJSON.to_string j with
      | "pull-keys" -> Pull_keys
      | "pull-tags" -> Pull_tags
      | "push-keys" -> Push_keys
      | "push-tags" -> Push_tags
      | "watch"    -> Watch
      | _          -> failwith "Action.of_json"

    cstruct hdr {
        uint8_t kind
      } as big_endian

    type channel = C.t

    let read fd =
      lwt buf = C.read fd sizeof_hdr in
      let kind = match get_hdr_kind buf with
        | 0 -> Pull_keys
        | 1 -> Pull_tags
        | 2 -> Push_keys
        | 3 -> Push_tags
        | 4 -> Watch
        | _ -> failwith "Action.read" in
      return kind

    let write fd t =
      let kind = match t with
        | Pull_keys -> 0
        | Pull_tags -> 1
        | Push_keys -> 2
        | Push_tags -> 3
        | Watch     -> 4 in
      C.write_string fd (string_of_int kind)

  end

  module Graph = MakeProduct(Keys)(KeyPairs)

  module TagsGraph = MakeProduct(Tags)(Graph)

  module Client = struct

    type key = Key.t
    type tag = Tag.t
    type channel = C.t
    type graph = key list * (key * key) list

    let pull_keys fd roots tags =
      lwt () = Action.write fd Pull_keys in
      lwt () = Keys.write fd roots in
      lwt () = Tags.write fd tags in
      Graph.read fd

    let pull_tags fd =
      lwt () = Action.write fd Pull_tags in
      TagKeys.read fd

    let push_keys fd graph tags =
      lwt () = Action.write fd Push_keys in
      lwt () = Graph.write fd graph in
      TagKeys.write fd tags

    let push_tags fd tags =
      lwt () = Action.write fd Push_tags in
      TagKeys.write fd tags

    let watch fd tags callback =
      lwt () = Action.write fd Watch in
      lwt () = Tags.write fd tags in
      let read () =
        try
          lwt (tags, graph) = TagsGraph.read fd in
          callback tags graph
        with End_of_file ->
          return () in
      read ()

  end

  module Server = struct

    type channel = C.t
    type key = Key.t
    type tag = Tag.t
    type graph = key list * (key * key) list

    module Algo = IrminAlgo.Make(Key)(Tag)

    let pull_keys fd =
      lwt keys = Keys.read fd in
      lwt tags = Tags.read fd in
      lwt graph = Algo.pull_keys () keys tags in
      Graph.write fd graph

    let pull_tags fd =
      lwt tags = Algo.pull_tags () in
      TagKeys.write fd tags

    let push_keys fd =
      lwt graph = Graph.read fd in
      lwt tags = TagKeys.read fd in
      Algo.push_keys () graph tags

    let push_tags fd =
      lwt tags = TagKeys.read fd in
      Algo.push_tags () tags

    let watch fd =
      lwt tags = Tags.read fd in
      try_lwt
        Algo.watch () tags (fun tags graph ->
            TagsGraph.write fd (tags, graph)
          )
      with _ ->
        C.close fd

    let dispatch fd =
      lwt action = Action.read fd in
      match action with
      | Pull_keys -> pull_keys fd
      | Pull_tags -> pull_tags fd
      | Push_keys -> push_keys fd
      | Push_tags -> push_tags fd
      | Watch     -> watch fd

  end

end
