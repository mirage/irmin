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

module Action (C: IrminAPI.CHANNEL) = struct

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

module MakeClient
    (C: IrminAPI.CHANNEL)
    (K: IrminAPI.KEY with type channel = C.t)
    (T: IrminAPI.TAG with type channel = C.t)
= struct

  module Action = Action(C)

  type channel = C.t

  module K = K
  module Ks = IrminImpl.MakeList(C)(K)

  module T = T

  module TRK = IrminImpl.MakeProduct(C)(T.R)(K)
  module TRKs = IrminImpl.MakeList(C)(TRK)

  module TLK = IrminImpl.MakeProduct(C)(T.L)(K)
  module TLKs = IrminImpl.MakeList(C)(TLK)

  module TRs = IrminImpl.MakeList(C)(T.R)

  module TRsG = IrminImpl.MakeProduct(C)(TRs)(K.Graph)

  let pull_keys fd roots tags =
    lwt () = Action.write fd Pull_keys in
    lwt () = Ks.write fd roots in
    lwt () = TRs.write fd tags in
    K.Graph.read fd

  let pull_tags fd =
    lwt () = Action.write fd Pull_tags in
    TRKs.read fd

  let push_keys fd graph tags =
    lwt () = Action.write fd Push_keys in
    lwt () = K.Graph.write fd graph in
    TLKs.write fd tags

  let push_tags fd tags =
    lwt () = Action.write fd Push_tags in
    TRKs.write fd tags

  let watch fd tags =
    lwt () = Action.write fd Watch in
    lwt () = TRs.write fd tags in
    let read () =
      try
        lwt t = TRsG.read fd in
        return (Some t)
      with End_of_file ->
        return None in
    return (Lwt_stream.from read)

end

module Client (C: IrminAPI.CHANNEL) =
  MakeClient(C)(IrminImpl.Key(C))(IrminImpl.Tag(C))

module MakeServer
    (C: IrminAPI.CHANNEL)
    (K: IrminAPI.KEY with type channel = C.t)
    (T: IrminAPI.TAG with type channel = C.t)
    (R: IrminAPI.REMOTE with type channel = unit
                         and module K = K
                         and module T = T)
= struct

  module Action = Action(C)

  type channel = C.t

  module K = K
  module Ks = IrminImpl.MakeList(C)(K)

  module T = T

  module TRK = IrminImpl.MakeProduct(C)(T.R)(K)
  module TRKs = IrminImpl.MakeList(C)(TRK)

  module TLK = IrminImpl.MakeProduct(C)(T.L)(K)
  module TLKs = IrminImpl.MakeList(C)(TLK)

  module TRs = IrminImpl.MakeList(C)(T.R)

  module TRsG = IrminImpl.MakeProduct(C)(TRs)(K.Graph)

  let pull_keys fd =
    lwt keys = Ks.read fd in
    lwt tags = TRs.read fd in
    lwt graph = R.pull_keys () keys tags in
    K.Graph.write fd graph

  let pull_tags fd =
    lwt tags = R.pull_tags () in
    TRKs.write fd tags

  let push_keys fd =
    lwt graph = K.Graph.read fd in
    lwt tags = TLKs.read fd in
    R.push_keys () graph tags

  let push_tags fd =
    lwt tags = TRKs.read fd in
    R.push_tags () tags

  let watch fd =
    lwt tags = TRs.read fd in
    lwt stream = R.watch () tags in
    let rec loop () =
      lwt event = Lwt_stream.get stream in
      match event with
      | None   -> C.close fd
      | Some e ->
        lwt () = TRsG.write fd e in
        loop () in
    loop ()

  let dispatch fd =
    lwt action = Action.read fd in
    match action with
    | Pull_keys -> pull_keys fd
    | Pull_tags -> pull_tags fd
    | Push_keys -> push_keys fd
    | Push_tags -> push_tags fd
    | Watch     -> watch fd

end

module Server
    (C: IrminAPI.CHANNEL)
    (R: IrminAPI.REMOTE with type channel = unit
                         and module K = IrminImpl.Key(C)
                         and module T = IrminImpl.Tag(C)
    ) =
  MakeServer(C)(IrminImpl.Key(C))(IrminImpl.Tag(C))(R)
