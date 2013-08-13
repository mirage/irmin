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

type action =
  | Pull_keys
  | Pull_tags
  | Push_keys
  | Push_tags
  | Watch

module Action (C: IrminAPI.CHANNEL): IrminAPI.BASE
  with type t = action
   and type channel = C.t
= struct

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

  include IrminImp.Iter(struct
      type t = action
      let read = read
      let write = write
    end)

end

module Client
    (C: IrminAPI.CHANNEL)
    (K: IrminAPI.KEY with type channel = C.t)
    (T: IrminAPI.TAG with type channel = C.t): IrminAPI.REMOTE
  with module C = C
   and module K = K
   and module T = T
= struct

  module C = C
  module K = K
  module T = T
  module A = Action(C)

  module TRV = IrminImp.TagVal(T.R)

  module TLV = IrminImpl.TagVal(T.L)

  let pull_keys fd roots tags =
    lwt () = A.write fd Pull_keys in
    lwt () = K.writes fd roots in
    lwt () = T.R.writes fd tags in
    K.Graph.read fd

  let pull_tags fd =
    lwt () = A.write fd Pull_tags in
    failwith "TODO"

  let push_keys _ =
    failwith "TODO"

  let push_tags _ =
    failwith "TODO"

  let watch _ =
    failwith "TODO"

end

(*
(* XXX: should be replaced by a streaming API *)
let discover_of_json = function
  | `O [ ("local", local); ("remote", remote) ] ->
    let keys = keys_of_json local in
    let tags = tags_of_json remote in
    keys, tags
  | _ -> failwith "discover_of_json"

let json_of_discover (keys, tags) =
  `O [ ("local", json_of_keys keys); ("remote", json_of_tags tags) ]

*)

module Client = IrminProtocol.Client(IrminImpl.Channel)(IrminImpl.Key)(IrminImpl.Tag)
