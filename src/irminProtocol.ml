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

type action =
  | Pull_keys
  | Pull_tags
  | Push_keys
  | Push_tags
  | Watch

module Action (C: CHANNEL) = struct

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

module type S = sig
  type channel
  module Key: KEY
  module Value: VALUE
  module Tag: TAG
  module Client: REMOTE with type key = Key.t
                         and type tag = Tag.t
                         and type channel = channel
  module Server
      (KS: KEY_STORE with type key = Key.t)
      (TS: TAG_STORE with type key = Key.t and type tag = Tag.t):
  sig
    val dispatch: channel -> unit Lwt.t
  end
end

module Make (C: CHANNEL) (K: IrminKey.S with type channel = C.t) = struct

  type channel = C.t

  module Action = Action(C)

  module Key = K
  module Keys = IrminIO.List(C)(Key)
  module KeyPair = IrminIO.Pair(C)(Key)(Key)
  module KeyPairs = IrminIO.List(C)(KeyPair)

  module Value = IrminValue.Make(C)(Key)

  module Tag = IrminTag.Make(C)
  module Tags = IrminIO.List(C)(Tag)

  module TagKey = IrminIO.Pair(C)(Tag)(Key)
  module TagKeys = IrminIO.List(C)(TagKey)

  module Graph = IrminIO.Pair(C)(Keys)(KeyPairs)

  module TagsGraph = IrminIO.Pair(C)(Tags)(Graph)

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

  module Server
      (KS: KEY_STORE with type key = Key.t)
      (TS: TAG_STORE with type key = Key.t and type tag = Tag.t)
  = struct

    module Algo = IrminAlgo.Make(KS)(TS)

    type channel = C.t
    type key = Key.t
    type tag = Tag.t
    type graph = key list * (key * key) list

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
