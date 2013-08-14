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

type t =
  (* Key store *)
  | Key_add_key
  | Key_add_relation
  | Key_list
  | Key_pred
  | Key_succ
  (* Value store *)
  | Value_write
  | Value_read
  (* Tag store *)
  | Tag_update
  | Tag_remove
  | Tag_read
  | Tag_list
  (* Sync *)
  | Sync_pull_keys
  | Sync_pull_tags
  | Sync_push_keys
  | Sync_push_tags
  | Sync_watch

module Action (C: CHANNEL) = struct

  let actions = [|
    Key_add_key      , "key-add-key";
    Key_add_relation , "key-add-relation";
    Key_list         , "key-list";
    Key_pred         , "key-pred";
    Key_succ         , "key-succ";
    Value_write      , "value-write";
    Value_read       , "value-read";
    Tag_update       , "tag-update";
    Tag_remove       , "tag-remove";
    Tag_read         , "tag-read";
    Tag_list         , "tag-list";
    Sync_pull_keys   , "sync-pull-keys";
    Sync_pull_tags   , "sync-pull-tags";
    Sync_push_keys   , "sync-pull-keys";
    Sync_push_tags   , "sync-push-tags";
    Sync_watch       , "watch";
  |]

  let find pred =
    let rec aux i =
      if i <= 0 then raise Not_found
      else
        let a, s = actions.(i) in
        if pred (a, s) then (a, s, i)
        else aux (i-1) in
    aux (Array.length actions)

  let assoc a =
    let _, s, _ =
      try find (fun (aa,_) -> aa=a)
      with Not_found -> assert false
    in s

  let rev_assoc s =
    try let a, _, _ = find (fun (_,ss) -> ss=s) in Some a
    with Not_found -> None

  let index a =
    let _, _, i =
      try find (fun (aa,_) -> aa=a)
      with Not_found -> assert false
    in i

  let action i =
    if i >= Array.length actions then None
    else
      Some (fst (actions.(i)))

  let pretty a =
    assoc a

  let to_json t =
    IrminJSON.of_string (pretty t)

  let of_json j =
    try rev_assoc (IrminJSON.to_string j)
    with Not_found -> failwith "Action.of_json"

  cstruct hdr {
      uint8_t kind
    } as big_endian

  let read fd =
    lwt buf = C.read fd sizeof_hdr in
    let kind =
      try action (get_hdr_kind buf)
      with Not_found -> failwith "Action.read" in
    return kind

  let write fd t =
    let kind = index t in
    C.write_string fd (string_of_int kind)

end

module Client (C: CHANNEL) (K: IrminKey.S with type channel = C.t) = struct

  module Action = Action(C)
  module Key = K
  module Keys = IrminIO.List(C)(Key)
  module KeyPair = IrminIO.Pair(C)(Key)(Key)
  module KeyPairs = IrminIO.List(C)(KeyPair)
  module Value = IrminValue.Make(C)(Key)
  module ValueOption = IrminIO.Option(C)(Value)
  module KeyOption = IrminIO.Option(C)(Key)
  module Tag = IrminTag.Make(C)
  module Tags = IrminIO.List(C)(Tag)
  module TagKey = IrminIO.Pair(C)(Tag)(Key)
  module TagKeys = IrminIO.List(C)(TagKey)
  module Graph = IrminIO.Pair(C)(Keys)(KeyPairs)
  module TagsGraph = IrminIO.Pair(C)(Tags)(Graph)

  module T = struct
    type t = C.t
    type key = Key.t
    type tag = Tag.t
    type value = Value.t
    type graph = key list * (key * key) list
  end

  include T

  module Key_store = struct

    include T

    let add_key fd key =
      lwt () = Action.write fd Key_add_key in
      Key.write fd key

    let add_relation fd k1 k2 =
      lwt () = Action.write fd Key_add_relation in
      lwt () = Key.write fd k1 in
      Key.write fd k2

    let list fd =
      lwt () = Action.write fd Key_list in
      Keys.read fd

    let pred fd k =
      lwt () = Action.write fd Key_pred in
      lwt () = Key.write fd k in
      Keys.read fd

    let succ fd k =
      lwt () = Action.write fd Key_succ in
      lwt () = Key.write fd k in
      Keys.read fd

  end

  module Value_store = struct

    include T

    let write fd v =
      lwt () = Action.write fd Value_write in
      lwt () = Value.write fd v in
      Key.read fd

    let read fd k =
      lwt () = Action.write fd Value_read in
      lwt () = Key.write fd k in
      ValueOption.read fd

  end

  module Tag_store = struct

    include T

    let update fd tag key =
      lwt () = Action.write fd Tag_update in
      lwt () = Tag.write fd tag in
      Key.write fd key

    let remove fd tag =
      lwt () = Action.write fd Tag_remove in
      Tag.write fd tag

    let read fd tag =
      lwt () = Action.write fd Tag_read in
      lwt () = Tag.write fd tag in
      KeyOption.read fd

    let list fd =
      lwt () = Action.write fd Tag_list in
      Tags.read fd

  end

  module Sync = struct

    include T

    let pull_keys fd roots tags =
      lwt () = Action.write fd Sync_pull_keys in
      lwt () = Keys.write fd roots in
      lwt () = Tags.write fd tags in
      Graph.read fd

    let pull_tags fd =
      lwt () = Action.write fd Sync_pull_tags in
      TagKeys.read fd

    let push_keys fd graph tags =
      lwt () = Action.write fd Sync_push_keys in
      lwt () = Graph.write fd graph in
      TagKeys.write fd tags

    let push_tags fd tags =
      lwt () = Action.write fd Sync_push_tags in
      TagKeys.write fd tags

    let watch fd tags callback =
      lwt () = Action.write fd Sync_watch in
      lwt () = Tags.write fd tags in
      let read () =
        try
          lwt (tags, graph) = TagsGraph.read fd in
          callback tags graph
        with End_of_file ->
          return () in
      read ()

  end

end

module type SERVER = sig
  type channel
  module KS: KEY_STORE
  module TS: TAG_STORE
  module VS: VALUE_STORE
  type t = {
    keys: KS.t;
    tags: TS.t;
    values: VS.t;
  }
  val run: t -> channel -> unit Lwt.t
end

module Server (C: CHANNEL)
    (K: IrminKey.S with type channel = C.t)
    (KS: KEY_STORE with type key = K.t)
    (TS: TAG_STORE with type key = K.t and type tag = IrminTag.t)
    (VS: VALUE_STORE with type key = K.t and type value = K.t IrminValue.t)
= struct

  module Action = Action(C)
  module Key = K
  module Keys = IrminIO.List(C)(Key)
  module KeyPair = IrminIO.Pair(C)(Key)(Key)
  module KeyPairs = IrminIO.List(C)(KeyPair)
  module Value = IrminValue.Make(C)(Key)
  module ValueOption = IrminIO.Option(C)(Value)
  module KeyOption = IrminIO.Option(C)(Key)
  module Tag = IrminTag.Make(C)
  module Tags = IrminIO.List(C)(Tag)
  module TagKey = IrminIO.Pair(C)(Tag)(Key)
  module TagKeys = IrminIO.List(C)(TagKey)
  module Graph = IrminIO.Pair(C)(Keys)(KeyPairs)
  module TagsGraph = IrminIO.Pair(C)(Tags)(Graph)

  type channel = C.t
  module KS = KS
  module TS = TS
  module VS = VS

  module T = struct
    type t = {
      keys:   KS.t;
      tags  : TS.t;
      values: VS.t;
    }
    type key = Key.t
    type tag = Tag.t
    type value = Value.t
    type graph = key list * (key * key) list
  end
  include T

  module Key_store = struct

    include T

    let add_key t fd =
      lwt k = Key.read fd in
      KS.add_key t.keys k

    let add_relation t fd =
      lwt k1 = Key.read fd in
      lwt k2 = Key.read fd in
      KS.add_relation t.keys k1 k2

    let list t fd =
      lwt keys = KS.list t.keys in
      Keys.write fd keys

    let pred t fd =
      lwt k = Key.read fd in
      lwt keys = KS.pred t.keys k in
      Keys.write fd keys

    let succ t fd =
      lwt k = Key.read fd in
      lwt keys = KS.succ t.keys k in
      Keys.write fd keys

  end

  module Value_store = struct

    include T

    let write t fd =
      lwt v = Value.read fd in
      lwt k = VS.write t.values v in
      Key.write fd k

    let read t fd =
      lwt k = Key.read fd in
      lwt vo = VS.read t.values k in
      ValueOption.write fd vo

  end

  module Tag_store = struct

    include T

    let update t fd =
      lwt tag = Tag.read fd in
      lwt key = Key.read fd in
      TS.update t.tags tag key

    let remove t fd =
      lwt tag = Tag.read fd in
      TS.remove t.tags tag

    let read t fd =
      lwt tag = Tag.read fd in
      lwt ko = TS.read t.tags tag in
      KeyOption.write fd ko

    let list t fd =
      lwt tags = TS.list t.tags in
      Tags.write fd tags

  end

  module Sync = struct

    include T

    module S = IrminSync.Make(KS)(TS)

    let pull_keys t fd =
      lwt keys = Keys.read fd in
      lwt tags = Tags.read fd in
      lwt graph = S.pull_keys () keys tags in
      Graph.write fd graph

    let pull_tags t fd =
      lwt tags = S.pull_tags () in
      TagKeys.write fd tags

    let push_keys t fd =
      lwt graph = Graph.read fd in
      lwt tags = TagKeys.read fd in
      S.push_keys () graph tags

    let push_tags t fd =
      lwt tags = TagKeys.read fd in
      S.push_tags () tags

    let watch t fd =
      lwt tags = Tags.read fd in
      try_lwt
        S.watch () tags (fun tags graph ->
            TagsGraph.write fd (tags, graph)
          )
      with _ ->
        C.close fd

  end

  let run t fd =
    lwt action = Action.read fd in
    let action = match action with
      | None   -> failwith "Unknown action"
      | Some a -> a in
    let fn = match action with
      | Key_add_key      -> Key_store.add_key
      | Key_add_relation -> Key_store.add_relation
      | Key_list         -> Key_store.list
      | Key_pred         -> Key_store.pred
      | Key_succ         -> Key_store.succ
      | Value_write      -> Value_store.write
      | Value_read       -> Value_store.read
      | Tag_update       -> Tag_store.update
      | Tag_remove       -> Tag_store.remove
      | Tag_read         -> Tag_store.read
      | Tag_list         -> Tag_store.list
      | Sync_pull_keys   -> Sync.pull_keys
      | Sync_pull_tags   -> Sync.pull_tags
      | Sync_push_keys   -> Sync.push_keys
      | Sync_push_tags   -> Sync.push_tags
      | Sync_watch       -> Sync.watch in
    fn t fd

end

module Disk  (C: CHANNEL) (K: IrminKey.S with type channel = C.t) = struct

  module Key = K
  module Keys = IrminIO.List(C)(Key)
  module KeyPair = IrminIO.Pair(C)(Key)(Key)
  module KeyPairs = IrminIO.List(C)(KeyPair)
  module Value = IrminValue.Make(C)(Key)
  module ValueOption = IrminIO.Option(C)(Value)
  module KeyOption = IrminIO.Option(C)(Key)
  module Tag = IrminTag.Make(C)
  module Tags = IrminIO.List(C)(Tag)
  module TagKey = IrminIO.Pair(C)(Tag)(Key)
  module TagKeys = IrminIO.List(C)(TagKey)
  module Graph = IrminIO.Pair(C)(Keys)(KeyPairs)
  module TagsGraph = IrminIO.Pair(C)(Tags)(Graph)

  type t = C.t
  type key = Key.t
  type tag = Tag.t
  type value = Value.t

  module Key_store = struct

    type t = C.t
    type key = Key.t

    let add_key t key =
      failwith "TODO"

    let add_relation t k1 k2 =
      failwith "TODO"

    let list t =
      failwith "TODO"

    let pred t k =
      failwith "TODO"

    let succ t k =
      failwith "TODO"

  end

  module Value_store = struct

    type t = C.t
    type key = Key.t
    type value = Value.t

    let write t v =
      failwith "TODO"

    let read t v =
      failwith "TODO"

  end

  module Tag_store = struct

    type t = C.t
    type tag = Tag.t
    type key = Key.t

    let update t tag key =
      failwith "TODO"

    let remove t tag =
      failwith "TODO"

    let read t tag =
      failwith "TODO"

    let list t =
      failwith "TODO"

  end

  module Sync = struct

    type t = C.t
    type key = Key.t
    type graph = key list * (key * key) list
    type tag = Tag.t

    let pull_keys t keys tags =
      failwith "TODO"

    let pull_tags t =
      failwith "TODO"

    let push_keys t graph keys =
      failwith "TODO"

    let push_tags t tags =
      failwith "TODO"

    let watch t tags =
      failwith "Bad operation"

  end

end
