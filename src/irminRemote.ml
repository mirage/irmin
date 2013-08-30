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

  let actions = [|
    Key_add          , "key-add";
    Key_list         , "key-list";
    Key_pred         , "key-pred";
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

  module T = struct

    type t = action

    let compare = Pervasives.compare

    let equal = (=)

    let pretty a =
      assoc a

  end

  module Set = IrminMisc.SetMake(T)

  include T

  let to_json t =
    IrminJSON.of_string (pretty t)

  let of_json j =
    match rev_assoc (IrminJSON.to_string j) with
    | None   -> failwith "Action.of_json"
    | Some t -> t

  let sizeof t =
    1

  let read buf =
    let kind = IrminIO.get_uint8 buf in
    let kind =
      match action kind with
      | None   -> failwith "Action.read"
      | Some t -> t in
    kind

  let write buf t =
    let kind = index t in
    IrminIO.set_uint8 buf kind

end

(** Signature for clients *)
module type CLIENT = sig
  type t = IrminIO.Lwt_channel.t
  include STORE with type t := t
                 and type Key_store.t = t
                 and type Value_store.t = t
                 and type Tag_store.t = t
  module Sync: SYNC with type t = t
end

module Client (C: CORE) = struct

  open C
  open IrminIO

  module XKey = Wire(Key)
  module XKeys = Wire(Set(Key))
  module XKeyKeys = Wire(Pair(Key)(XKeys))
  module XKeyPair = Wire(Pair(Key)(Key))
  module XKeyPairs = Wire(List(XKeyPair))

  module XValue = Wire(Value)
  module XValueOption = Wire(Option(Value))

  module XTag = Wire(Tag)
  module XTags = Wire(Set(Tag))
  module XKeysTags = Wire(Pair(XKeys)(XTags))
  module XTagKeys = Wire(Pair(Tag)(XKeys))
  module XTagKeyss = Wire(List(XTagKeys))

  module XGraph = Wire(Pair(XKeys)(XKeyPairs))
  module XTagsGraph = Wire(Pair(XTags)(XGraph))
  module XGraphTagKeyss = Wire(Pair(XGraph)(XTagKeyss))

  module XAction = Wire(Action)
  module XActionKey = Wire(Pair(Action)(Key))
  module XActionKeyKeys = Wire(Pair(Action)(Pair(XKey)(XKeys)))
  module XActionValue = Wire(Pair(Action)(Value))
  module XActionTag = Wire(Pair(Action)(Tag))
  module XActionTags = Wire(Pair(Action)(XTags))
  module XActionTagKeys = Wire(Pair(Action)(XTagKeys))
  module XActionTagKeyss = Wire(Pair(Action)(XTagKeyss))
  module XActionKeysTags = Wire(Pair(Action)(XKeysTags))
  module XActionGraphTagKeyss = Wire(Pair(Action)(XGraphTagKeyss))

  module Types = struct

    type t = Lwt_channel.t

    module C = C

    let key_store t = t

    let value_store t = t

    let tag_store t = t

    type graph = Key.Set.t * (Key.t * Key.t) list

  end

  include Types

  let read_unit = Lwt_channel.read_unit

  module Key_store = struct

    include Types

    let add fd key preds =
      lwt () = XActionKeyKeys.write_fd fd (Key_add, (key, preds)) in
      read_unit fd

    let all fd =
      lwt () = XAction.write_fd fd Key_list in
      lwt keys = XKeys.read_fd fd in
      Lwt.return keys

    let pred fd key =
      lwt () = XActionKey.write_fd fd (Key_pred, key) in
      lwt keys = XKeys.read_fd fd in
      Lwt.return keys

  end

  module Value_store = struct

    include Types

    let write fd value =
      lwt () = XActionValue.write_fd fd (Value_write, value) in
      XKey.read_fd fd

    let read fd key =
      lwt () = XActionKey.write_fd fd (Value_read, key) in
      XValueOption.read_fd fd

  end

  module Tag_store = struct

    include Types

    let update fd tag keys =
      lwt () = XActionTagKeys.write_fd fd (Tag_update, (tag, keys)) in
      read_unit fd

    let remove fd tag =
      lwt () = XActionTag.write_fd fd (Tag_remove, tag) in
      read_unit fd

    let read fd tag =
      lwt () = XActionTag.write_fd fd (Tag_read, tag) in
      XKeys.read_fd fd

    let all fd =
      lwt () = XAction.write_fd fd Tag_list in
      XTags.read_fd fd

  end

  module Sync = struct

    include Types

    let pull_keys fd roots tags =
      lwt () = XActionKeysTags.write_fd fd (Sync_pull_keys, (roots, tags)) in
      XGraph.read_fd fd

    let pull_tags fd =
      lwt () = XAction.write_fd fd Sync_pull_tags in
      XTagKeyss.read_fd fd

    let push_keys fd graph tags =
      lwt () = XActionGraphTagKeyss.write_fd fd (Sync_push_keys, (graph, tags)) in
      read_unit fd

    let push_tags fd tags =
      lwt () = XActionTagKeyss.write_fd fd (Sync_push_tags, tags) in
      read_unit fd

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
  type t = IrminIO.Lwt_channel.t
  module State: STORE
  val run: State.t -> t -> unit Lwt.t
end

module Server (S: STORE) = struct

  open IrminIO

  module State = S

  module C = S.C
  open C

  type t = Lwt_channel.t

  module Key_store = S.Key_store
  module Value_store = S.Value_store
  module Tag_store = S.Tag_store

  module XKey = Wire(Key)
  module XKeys = Wire(Set(Key))
  module XKeyKeys = Wire(Pair(Key)(XKeys))
  module XKeyPair = Wire(Pair(Key)(Key))
  module XKeyPairs = Wire(List(XKeyPair))

  module XValue = Wire(Value)
  module XValueOption = Wire(Option(Value))

  module XTag = Wire(Tag)
  module XTags = Wire(Set(Tag))
  module XKeysTags = Wire(Pair(XKeys)(XTags))
  module XTagKeys = Wire(Pair(Tag)(XKeys))
  module XTagKeyss = Wire(List(XTagKeys))

  module XGraph = Wire(Pair(XKeys)(XKeyPairs))
  module XTagsGraph = Wire(Pair(XTags)(XGraph))
  module XGraphTagKeyss = Wire(Pair(XGraph)(XTagKeyss))

  let write_unit = Lwt_channel.write_unit

  module XKey_store = struct

    let proj = State.key_store

    let add t buf fd =
      let (k1, k2s) = XKeyKeys.read buf in
      lwt () = Key_store.add (proj t) k1 k2s in
      write_unit fd

    let all t buf fd =
      lwt keys = Key_store.all (proj t) in
      XKeys.write_fd fd keys

    let pred t buf fd =
      let k = XKey.read buf in
      lwt keys = Key_store.pred (proj t) k in
      XKeys.write_fd fd keys

  end

  module XValue_store = struct

    let proj = State.value_store

    let write t buf fd =
      let v = XValue.read buf in
      lwt k = Value_store.write (proj t) v in
      XKey.write_fd fd k

    let read t buf fd =
      let k = XKey.read buf in
      lwt vo = Value_store.read (proj t) k in
      XValueOption.write_fd fd vo

  end

  module XTag_store = struct

    let proj = State.tag_store

    let update t buf fd =
      let (tag, keys) = XTagKeys.read buf in
      lwt () = Tag_store.update (proj t) tag keys in
      write_unit fd

    let remove t buf fd =
      let tag = XTag.read buf in
      lwt () = Tag_store.remove (proj t) tag in
      write_unit fd

    let read t buf fd =
      let tag = XTag.read buf in
      lwt keys = Tag_store.read (proj t) tag in
      XKeys.write_fd fd keys

    let all t buf fd =
      lwt tags = Tag_store.all (proj t) in
      XTags.write_fd fd tags

  end

  module XSync = struct

    module Sync = IrminSync.Make(S)

    let pull_keys _ buf fd =
      let (keys, tags) = XKeysTags.read buf in
      lwt graph = Sync.pull_keys () keys tags in
      XGraph.write_fd fd graph

    let pull_tags _ buf fd =
      lwt tags = Sync.pull_tags () in
      XTagKeyss.write_fd fd tags

    let push_keys _ buf fd =
      let (graph, tags) = XGraphTagKeyss.read buf in
      lwt () = Sync.push_keys () graph tags in
      write_unit fd

    let push_tags _ buf fd =
      let tags = XTagKeyss.read buf in
      lwt () = Sync.push_tags () tags in
      write_unit fd

    let watch _ buf fd =
      let tags = XTags.read buf in
      try_lwt
        Sync.watch () tags (fun tags graph ->
            XTagsGraph.write_fd fd (tags, graph)
          )
      with _ ->
        IrminIO.Lwt_channel.close fd

  end

  let run t fd =
    let process (fd, sockaddr) =
      let name = match sockaddr with
        | Lwt_unix.ADDR_UNIX s         -> s
        | Lwt_unix.ADDR_INET (ip,port) ->
          Unix.string_of_inet_addr ip ^ ":" ^ string_of_int port in
      Printf.printf "New connection from %s\n%!" name;
      let fd = IrminIO.Lwt_channel.create fd name in
      lwt len = IrminIO.Lwt_channel.read_length fd in
      let buf = IrminIO.create len in
      let action = Action.read buf in
      let fn = match action with
        | Key_add          -> XKey_store.add
        | Key_list         -> XKey_store.all
        | Key_pred         -> XKey_store.pred
        | Value_write      -> XValue_store.write
        | Value_read       -> XValue_store.read
        | Tag_update       -> XTag_store.update
        | Tag_remove       -> XTag_store.remove
        | Tag_read         -> XTag_store.read
        | Tag_list         -> XTag_store.all
        | Sync_pull_keys   -> XSync.pull_keys
        | Sync_pull_tags   -> XSync.pull_tags
        | Sync_push_keys   -> XSync.push_keys
        | Sync_push_tags   -> XSync.push_tags
        | Sync_watch       -> XSync.watch in
      fn t buf fd in
    Printf.printf "Listening on %s.\n%!" (Lwt_channel.name fd);
    Sys.catch_break true;
    try
      while_lwt true do
        lwt client = Lwt_unix.accept (Lwt_channel.channel fd) in
        process client
      done
    with e ->
      Printf.printf "Closing connection ...\n%!";
      lwt () = Lwt_channel.close fd in
      raise_lwt e

end
