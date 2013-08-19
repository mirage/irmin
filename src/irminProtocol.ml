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

module Action = struct

  type t = action

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
    match rev_assoc (IrminJSON.to_string j) with
    | None   -> failwith "Action.of_json"
    | Some t -> t

  let sizeof t =
    1

  let read buf =
    lwt kind = IrminIO.get_uint8 buf in
    let kind =
      match action kind with
      | None   -> failwith "Action.read"
      | Some t -> t in
    return kind

  let write buf t =
    let kind = index t in
    IrminIO.set_uint8 buf kind

end

module Client (K: KEY) (V: VALUE with type key = K.t) (T: TAG) = struct

  open IrminIO

  module Key = Channel(K)
  module Keys = Channel(List(Key))
  module KeyPair = Channel(Pair(Key)(Key))
  module KeyPairs = Channel(List(KeyPair))
  module KeyOption = Channel(Option(Key))

  module Value = Channel(V)
  module ValueOption = Channel(Option(Value))

  module Tag = Channel(T)
  module Tags = Channel(List(Tag))
  module KeysTags = Channel(Pair(Keys)(Tags))
  module TagKey = Channel(Pair(Tag)(Key))
  module TagKeys = Channel(List(TagKey))

  module Graph = Channel(Pair(Keys)(KeyPairs))
  module TagsGraph = Channel(Pair(Tags)(Graph))
  module GraphTagKeys = Channel(Pair(Graph)(TagKeys))

  module Action = Channel(Action)
  module ActionKey = Channel(Pair(Action)(Key))
  module ActionKeyPair = Channel(Pair(Action)(KeyPair))
  module ActionValue = Channel(Pair(Action)(Value))
  module ActionTag = Channel(Pair(Action)(Tag))
  module ActionTagKey = Channel(Pair(Action)(TagKey))
  module ActionTagKeys = Channel(Pair(Action)(TagKeys))
  module ActionKeysTags = Channel(Pair(Action)(KeysTags))
  module ActionGraphTagKeys = Channel(Pair(Action)(GraphTagKeys))
  module ActionTags = Channel(Pair(Action)(Tags))

  module T = struct
    type t = Lwt_unix.file_descr
    type key = Key.t
    type tag = Tag.t
    type value = Value.t
    type graph = key list * (key * key) list
  end

  module Key_store = struct

    include T

    let add_key fd key =
      ActionKey.write_fd fd (Key_add_key, key)

    let add_relation fd k1 k2 =
      ActionKeyPair.write_fd fd (Key_add_relation, (k1, k2))

    let list fd =
      lwt () = Action.write_fd fd Key_list in
      Keys.read_fd fd

    let pred fd key =
      lwt () = ActionKey.write_fd fd (Key_pred, key) in
      Keys.read_fd fd

    let succ fd key =
      lwt () = ActionKey.write_fd fd (Key_succ, key) in
      Keys.read_fd fd

  end

  module Value_store = struct

    include T

    let write fd value =
      lwt () = ActionValue.write_fd fd (Value_write, value) in
      Key.read_fd fd

    let read fd key =
      lwt () = ActionKey.write_fd fd (Value_read, key) in
      ValueOption.read_fd fd

  end

  module Tag_store = struct

    include T

    let update fd tag key =
      ActionTagKey.write_fd fd (Tag_update, (tag, key))

    let remove fd tag =
      ActionTag.write_fd fd (Tag_remove, tag)

    let read fd tag =
      lwt () = ActionTag.write_fd fd (Tag_read, tag) in
      KeyOption.read_fd fd

    let list fd =
      lwt () = Action.write_fd fd Tag_list in
      Tags.read_fd fd

  end

  module Sync = struct

    include T

    let pull_keys fd roots tags =
      lwt () = ActionKeysTags.write_fd fd (Sync_pull_keys, (roots, tags)) in
      Graph.read_fd fd

    let pull_tags fd =
      lwt () = Action.write_fd fd Sync_pull_tags in
      TagKeys.read_fd fd

    let push_keys fd graph tags =
      ActionGraphTagKeys.write_fd fd (Sync_push_keys, (graph, tags))

    let push_tags fd tags =
      ActionTagKeys.write_fd fd (Sync_push_tags, tags)

    let watch fd tags callback =
      lwt () = ActionTags.write_fd fd (Sync_watch, tags) in
      let read () =
        try
          lwt (tags, graph) = TagsGraph.read_fd fd in
          callback tags graph
        with End_of_file ->
          return () in
      read ()

  end

end

module type SERVER = sig
  type channel = Lwt_unix.file_descr
  type key_store
  type tag_store
  type value_store
  type t = {
    keys  : key_store;
    tags  : tag_store;
    values: value_store;
  }
  val run: t -> channel -> unit Lwt.t
end

module Server (K: KEY) (V: VALUE with type key = K.t) (T: TAG)
    (KS: KEY_STORE with type key = K.t)
    (VS: VALUE_STORE with type key = K.t and type value = V.t)
    (TS: TAG_STORE with type key = K.t and type tag = T.t)
= struct

  open IrminIO

  module Key = Channel(K)
  module Keys = Channel(List(Key))
  module KeyPair = Channel(Pair(Key)(Key))
  module KeyPairs = Channel(List(KeyPair))
  module KeyOption = Channel(Option(Key))

  module Value = Channel(V)
  module ValueOption = Channel(Option(Value))

  module Tag = Channel(T)
  module Tags = Channel(List(Tag))
  module KeysTags = Channel(Pair(Keys)(Tags))
  module TagKey = Channel(Pair(Tag)(Key))
  module TagKeys = Channel(List(TagKey))

  module Graph = Channel(Pair(Keys)(KeyPairs))
  module TagsGraph = Channel(Pair(Tags)(Graph))
  module GraphTagKeys = Channel(Pair(Graph)(TagKeys))

  module Action = Channel(Action)
  module ActionKey = Channel(Pair(Action)(Key))
  module ActionKeyPair = Channel(Pair(Action)(KeyPair))
  module ActionValue = Channel(Pair(Action)(Value))
  module ActionTag = Channel(Pair(Action)(Tag))
  module ActionTagKey = Channel(Pair(Action)(TagKey))
  module ActionTagKeys = Channel(Pair(Action)(TagKeys))
  module ActionKeysTags = Channel(Pair(Action)(KeysTags))
  module ActionGraphTagKeys = Channel(Pair(Action)(GraphTagKeys))
  module ActionTags = Channel(Pair(Action)(Tags))

  module T = struct
    type channel = IrminIO.Lwt_channel.t
    type key_store = KS.t
    type value_store = VS.t
    type tag_store = TS.t
    type t = {
      keys:   key_store;
      tags  : tag_store;
      values: value_store;
    }
    type key = Key.t
    type tag = Tag.t
    type value = Value.t
    type graph = key list * (key * key) list
  end
  include T

  module Key_store = struct

    include T

    let add_key t buf =
      lwt k = Key.read buf in
      KS.add_key t.keys k

    let add_relation t buf =
      lwt (k1, k2) = KeyPair.read buf in
      KS.add_relation t.keys k1 k2

    let list t buf =
      lwt keys = KS.list t.keys in
      Keys.write buf keys

    let pred t buf =
      lwt k = Key.read buf in
      lwt keys = KS.pred t.keys k in
      Keys.write buf keys

    let succ t buf =
      lwt k = Key.read buf in
      lwt keys = KS.succ t.keys k in
      Keys.write buf keys

  end

  module Value_store = struct

    include T

    let write t buf =
      lwt v = Value.read buf in
      lwt k = VS.write t.values v in
      Key.write buf k

    let read t buf =
      lwt k = Key.read buf in
      lwt vo = VS.read t.values k in
      ValueOption.write buf vo

  end

  module Tag_store = struct

    include T

    let update t buf =
      lwt tag = Tag.read buf in
      lwt key = Key.read buf in
      TS.update t.tags tag key

    let remove t buf =
      lwt tag = Tag.read buf in
      TS.remove t.tags tag

    let read t buf =
      lwt tag = Tag.read buf in
      lwt ko = TS.read t.tags tag in
      KeyOption.write buf ko

    let list t buf =
      lwt tags = TS.list t.tags in
      Tags.write buf tags

  end

  module Sync = struct

    include T

    module S = IrminSync.Make(KS)(TS)

    let pull_keys t buf =
      lwt keys = Keys.read buf in
      lwt tags = Tags.read buf in
      lwt graph = S.pull_keys () keys tags in
      Graph.write buf graph

    let pull_tags t buf =
      lwt tags = S.pull_tags () in
      TagKeys.write buf tags

    let push_keys t buf =
      lwt graph = Graph.read buf in
      lwt tags = TagKeys.read buf in
      S.push_keys () graph tags

    let push_tags t buf =
      lwt tags = TagKeys.read buf in
      S.push_tags () tags

    let watch fd t buf =
      lwt tags = Tags.read buf in
      try_lwt
        S.watch () tags (fun tags graph ->
            TagsGraph.write buf (tags, graph)
          )
      with _ ->
        IrminIO.Lwt_channel.close fd

  end

  let run t fd =
    lwt len = IrminIO.Lwt_channel.read_header fd in
    let buf = IrminIO.create len (IrminIO.Lwt_channel.ready fd) in
    lwt action = Action.read buf in
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
      | Sync_watch       -> Sync.watch fd in
    fn t buf

end

module Disk (K: KEY) (V: VALUE with type key = K.t) (T: TAG) = struct

  type t = unit

  let with_file _ fn = fn ()

  let init _ = Lwt.return ()

  module Key_store = struct
    type key = unit
    type t = unit
    let succ _ = failwith "TODO"
    let pred _ = failwith "TODO"
    let list _ = failwith "TODO"
    let add_relation _ = failwith "TODO"
    let add_key _ = failwith "TODO"
    let key _ = failwith "TODO"
  end

  module Value_store = struct
    type t = unit
    type key = unit
    type value = unit
    let write _ = failwith "TODO"
    let read _ = failwith "TODO"
  end

  module Tag_store = struct
    type t = unit
    type tag = unit
    type key = unit
    let update _ = failwith "TODO"
    let remove _ = failwith "TODO"
    let read _ = failwith "TODO"
    let list _ = failwith "TODO"
  end

end
