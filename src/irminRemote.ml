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

type action =
  (* Key store *)
  | Key_add
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
    Key_add          , "key-add";
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
    Lwt.return kind

  let write buf t =
    let kind = index t in
    IrminIO.set_uint8 buf kind

end

(** Signature for clients *)
module type CLIENT = sig

  (** Abstract channels *)
  type t

  (** Access the remote key store *)
  module Key_store: KEY_STORE with type t = t

  (** Access the remote value store *)
  module Value_store: VALUE_STORE with type t = t

  (** Access the remote tag store *)
  module Tag_store: TAG_STORE with type t = t

  (** Sync with a remote server *)
  module Sync: SYNC with type t = t

end

module Client (K: KEY) (V: VALUE with module Key = K) (T: TAG) = struct

  open IrminIO

  module XKey = Channel(K)
  module XKeys = Channel(List(K))
  module XKeyKeys = Channel(Pair(K)(XKeys))
  module XKeyPair = Channel(Pair(K)(K))
  module XKeyPairs = Channel(List(XKeyPair))
  module XKeyOption = Channel(Option(K))

  module XValue = Channel(V)
  module XValueOption = Channel(Option(V))

  module XTag = Channel(T)
  module XTags = Channel(List(T))
  module XKeysTags = Channel(Pair(XKeys)(XTags))
  module XTagKey = Channel(Pair(T)(K))
  module XTagKeys = Channel(List(XTagKey))

  module XGraph = Channel(Pair(XKeys)(XKeyPairs))
  module XTagsGraph = Channel(Pair(XTags)(XGraph))
  module XGraphTagKeys = Channel(Pair(XGraph)(XTagKeys))

  module XAction = Channel(Action)
  module XActionKey = Channel(Pair(Action)(K))
  module XActionKeys = Channel(Pair(Action)(XKeys))
  module XActionValue = Channel(Pair(Action)(V))
  module XActionTag = Channel(Pair(Action)(T))
  module XActionTags = Channel(Pair(Action)(XTags))
  module XActionTagKey = Channel(Pair(Action)(XTagKey))
  module XActionTagKeys = Channel(Pair(Action)(XTagKeys))
  module XActionKeysTags = Channel(Pair(Action)(XKeysTags))
  module XActionGraphTagKeys = Channel(Pair(Action)(XGraphTagKeys))

  type t = Lwt_channel.t

  module T = struct

    module Key = K

    module Value = V

    module Tag = T

    type graph = Key.t list * (Key.t * Key.t) list

    type t = Lwt_channel.t

  end


  module Key_store = struct

    include T

    let add fd key preds =
      XActionKeys.write_fd fd (Key_add, (key :: (K.Set.to_list preds)))

    let list fd =
      lwt () = XAction.write_fd fd Key_list in
      XKeys.read_fd fd

    let pred fd key =
      lwt () = XActionKey.write_fd fd (Key_pred, key) in
      lwt keys = XKeys.read_fd fd in
      Lwt.return (K.Set.of_list keys)

    let succ fd key =
      lwt () = XActionKey.write_fd fd (Key_succ, key) in
      lwt keys = XKeys.read_fd fd in
      Lwt.return (K.Set.of_list keys)

  end

  module Value_store = struct

    include T

    let write fd value =
      lwt () = XActionValue.write_fd fd (Value_write, value) in
      XKey.read_fd fd

    let read fd key =
      lwt () = XActionKey.write_fd fd (Value_read, key) in
      XValueOption.read_fd fd

  end

  module Tag_store = struct

    include T

    let update fd tag key =
      XActionTagKey.write_fd fd (Tag_update, (tag, key))

    let remove fd tag =
      XActionTag.write_fd fd (Tag_remove, tag)

    let read fd tag =
      lwt () = XActionTag.write_fd fd (Tag_read, tag) in
      XKeyOption.read_fd fd

    let list fd =
      lwt () = XAction.write_fd fd Tag_list in
      XTags.read_fd fd

  end

  module Sync = struct

    include T

    let pull_keys fd roots tags =
      lwt () = XActionKeysTags.write_fd fd (Sync_pull_keys, (roots, tags)) in
      XGraph.read_fd fd

    let pull_tags fd =
      lwt () = XAction.write_fd fd Sync_pull_tags in
      XTagKeys.read_fd fd

    let push_keys fd graph tags =
      XActionGraphTagKeys.write_fd fd (Sync_push_keys, (graph, tags))

    let push_tags fd tags =
      XActionTagKeys.write_fd fd (Sync_push_tags, tags)

    let watch fd tags callback =
      lwt () = XActionTags.write_fd fd (Sync_watch, tags) in
      let read () =
        try
          lwt (tags, graph) = XTagsGraph.read_fd fd in
          callback tags graph
        with End_of_file ->
          Lwt.return () in
      read ()

  end

end

module type SERVER = sig
  type t
  module Key_store: KEY_STORE
  module Value_store: VALUE_STORE
  module Tag_store: TAG_STORE
  type stores = {
    keys  : Key_store.t;
    values: Value_store.t;
    tags  : Tag_store.t;
  }
  val run: stores -> t -> unit Lwt.t
end

module Server (K: KEY) (V: VALUE with module Key = K) (T: TAG)
    (KS: KEY_STORE with module Key = K)
    (VS: VALUE_STORE with module Key = K and module Value = V)
    (TS: TAG_STORE with module Key = K and module Tag = T)
= struct

  open IrminIO

  module XKey = Channel(K)
  module XKeys = Channel(List(K))
  module XKeyKeys = Channel(Pair(K)(XKeys))
  module XKeyPair = Channel(Pair(K)(K))
  module XKeyPairs = Channel(List(XKeyPair))
  module XKeyOption = Channel(Option(K))

  module XValue = Channel(V)
  module XValueOption = Channel(Option(V))

  module XTag = Channel(T)
  module XTags = Channel(List(T))
  module XKeysTags = Channel(Pair(XKeys)(XTags))
  module XTagKey = Channel(Pair(T)(K))
  module XTagKeys = Channel(List(XTagKey))

  module XGraph = Channel(Pair(XKeys)(XKeyPairs))
  module XTagsGraph = Channel(Pair(XTags)(XGraph))
  module XGraphTagKeys = Channel(Pair(XGraph)(XTagKeys))

  type stores = {
    keys:   KS.t;
    values: VS.t;
    tags  : TS.t;
  }

  module XKey_store = struct

    let add t buf =
      lwt (k1, k2s) = XKeyKeys.read buf in
      KS.add t.keys k1 (K.Set.of_list k2s)

    let list t buf =
      lwt keys = KS.list t.keys in
      XKeys.write buf keys

    let pred t buf =
      lwt k = XKey.read buf in
      lwt keys = KS.pred t.keys k in
      XKeys.write buf (K.Set.to_list keys)

    let succ t buf =
      lwt k = XKey.read buf in
      lwt keys = KS.succ t.keys k in
      XKeys.write buf (K.Set.to_list keys)

  end

  module XValue_store = struct

    let write t buf =
      lwt v = XValue.read buf in
      lwt k = VS.write t.values v in
      XKey.write buf k

    let read t buf =
      lwt k = XKey.read buf in
      lwt vo = VS.read t.values k in
      XValueOption.write buf vo

  end

  module XTag_store = struct

    let update t buf =
      lwt (tag, key) = XTagKey.read buf in
      TS.update t.tags tag key

    let remove t buf =
      lwt tag = XTag.read buf in
      TS.remove t.tags tag

    let read t buf =
      lwt tag = XTag.read buf in
      lwt ko = TS.read t.tags tag in
      XKeyOption.write buf ko

    let list t buf =
      lwt tags = TS.list t.tags in
      XTags.write buf tags

  end

  module XSync = struct

    module S = IrminSync.Make(KS)(TS)

    let pull_keys t buf =
      lwt (keys, tags) = XKeysTags.read buf in
      lwt graph = S.pull_keys () keys tags in
      XGraph.write buf graph

    let pull_tags t buf =
      lwt tags = S.pull_tags () in
      XTagKeys.write buf tags

    let push_keys t buf =
      lwt (graph, tags) = XGraphTagKeys.read buf in
      S.push_keys () graph tags

    let push_tags t buf =
      lwt tags = XTagKeys.read buf in
      S.push_tags () tags

    let watch fd t buf =
      lwt tags = XTags.read buf in
      try_lwt
        S.watch () tags (fun tags graph ->
            XTagsGraph.write buf (tags, graph)
          )
      with _ ->
        IrminIO.Lwt_channel.close fd

  end

  let run t fd =
    lwt len = IrminIO.Lwt_channel.read_length fd in
    let buf = IrminIO.create len (IrminIO.Lwt_channel.ready fd) in
    lwt action = Action.read buf in
    let fn = match action with
      | Key_add          -> XKey_store.add
      | Key_list         -> XKey_store.list
      | Key_pred         -> XKey_store.pred
      | Key_succ         -> XKey_store.succ
      | Value_write      -> XValue_store.write
      | Value_read       -> XValue_store.read
      | Tag_update       -> XTag_store.update
      | Tag_remove       -> XTag_store.remove
      | Tag_read         -> XTag_store.read
      | Tag_list         -> XTag_store.list
      | Sync_pull_keys   -> XSync.pull_keys
      | Sync_pull_tags   -> XSync.pull_tags
      | Sync_push_keys   -> XSync.push_keys
      | Sync_push_tags   -> XSync.push_tags
      | Sync_watch       -> XSync.watch fd in
    fn t buf

  type t = Lwt_channel.t
  module Key_store = KS
  module Value_store = VS
  module Tag_store = TS

end
