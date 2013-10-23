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
  | Sync_watch

module Action = struct

  let debug fmt = IrminMisc.debug "ACTION" fmt

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
    Sync_watch       , "watch";
  |]

  let find pred =
    let rec aux i =
      if i <= 0 then raise Not_found
      else
        let a, s = actions.(i) in
        if pred (a, s) then (a, s, i)
        else aux (i-1) in
    aux (Array.length actions - 1)

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

    let hash = Hashtbl.hash

    let pretty a =
      assoc a

    let to_json t =
      IrminJSON.of_string (pretty t)

    let of_json j =
      match rev_assoc (IrminJSON.to_string j) with
      | None   -> failwith "Action.of_json"
      | Some t -> t

    let sizeof _ =
      1

    let get buf =
      debug "get";
      let kind = IrminIO.get_uint8 buf in
      let kind =
        match action kind with
        | None   -> failwith "Action.read"
        | Some t -> t in
      kind

    let set buf t =
      debug "set %s" (pretty t);
      let kind = index t in
      IrminIO.set_uint8 buf kind

  end

  module Set = IrminContainer.Set(T)

  include T

end

(** Signature for clients *)
module type CLIENT = sig
  type t = unit -> IrminIO.Lwt_channel.t Lwt.t
  include STORE with type t := t
                 and type Key_store.t = t
                 and type Value_store.t = t
                 and type Tag_store.t = t
  module Sync: SYNC with type t = t
end

module Make_client (C: CORE) = struct

  open IrminIO

  module XKey = Channel(C.Key)
  module XKeys = Channel(Key.Set)
  module XKeyKeys = Channel(Pair(Key)(XKeys))
  module XKeyPair = Channel(Pair(Key)(Key))
  module XKeyPairs = Channel(List(XKeyPair))
  module XKeysO = Channel(Option(XKeys))

  module XValue = Channel(Value)
  module XValueOption = Channel(Option(Value))

  module XTag = Channel(Tag)
  module XTags = Channel(Tag.Set)
  module XTagKeys = Channel(Pair(Tag)(XKeys))
  module XTagKeyss = Channel(List(XTagKeys))

  module XGraph = Channel(Key.Graph)
  module XTagsGraph = Channel(Pair(XTags)(XGraph))
  module XGraphTagKeyss = Channel(Pair(XGraph)(XTagKeyss))

  module Event = IrminSync.Event (C)
  module XEvent = Channel(Event)

  module XAction = Channel(Action)
  module XActionKey = Channel(Pair(Action)(Key))
  module XActionKeysOKeysO = Channel(Pair(Action)(Pair(XKeysO)(XKeysO)))
  module XActionKeysTags = Channel(Pair(Action)(Pair(XKeys)(XTags)))
  module XActionKeyKeys = Channel(Pair(Action)(Pair(XKey)(XKeys)))
  module XActionValue = Channel(Pair(Action)(Value))
  module XActionTag = Channel(Pair(Action)(Tag))
  module XActionTags = Channel(Pair(Action)(XTags))
  module XActionTagKeys = Channel(Pair(Action)(XTagKeys))
  module XActionTagKeyss = Channel(Pair(Action)(XTagKeyss))
  module XActionGraphTagKeyss = Channel(Pair(Action)(XGraphTagKeyss))

  module Types = struct

    type t = unit -> C.Channel.t Lwt.t

    module C = C

    let key_store t = t

    let value_store t = t

    let tag_store t = t

  end

  include Types

  let read_unit = C.Channel.read_unit

  module Key_store = struct

    include Types

    let add t key preds =
      lwt fd = t () in
      lwt () = XActionKeyKeys.write_fd fd (Key_add, (key, preds)) in
      read_unit fd

    let keys t ?sources ?sinks () =
      lwt fd = t () in
      lwt () = XActionKeysOKeysO.write_fd fd (Key_list, (sources, sinks)) in
      lwt g = XGraph.read_fd fd in
      return g

    let pred t key =
      lwt fd = t () in
      lwt () = XActionKey.write_fd fd (Key_pred, key) in
      lwt keys = XKeys.read_fd fd in
      return keys

  end

  module Value_store = struct

    include Types

    let write t value =
      lwt fd = t () in
      lwt () = XActionValue.write_fd fd (Value_write, value) in
      XKey.read_fd fd

    let read t key =
      lwt fd = t () in
      lwt () = XActionKey.write_fd fd (Value_read, key) in
      XValueOption.read_fd fd

  end

  module Tag_store = struct

    include Types

    let update t tag keys =
      lwt fd = t () in
      lwt () = XActionTagKeys.write_fd fd (Tag_update, (tag, keys)) in
      read_unit fd

    let remove t tag =
      lwt fd = t () in
      lwt () = XActionTag.write_fd fd (Tag_remove, tag) in
      read_unit fd

    let read t tag =
      lwt fd = t () in
      lwt () = XActionTag.write_fd fd (Tag_read, tag) in
      XKeys.read_fd fd

    let all t =
      lwt fd = t () in
      lwt () = XAction.write_fd fd Tag_list in
      XTags.read_fd fd

  end

  module Sync = struct

    include Types

    module Event = Event

    let pull_keys t ~sources ~sinks =
      lwt fd = t () in
      lwt () = XActionKeysTags.write_fd fd (Sync_pull_keys, (sources, sinks)) in
      XGraph.read_fd fd

    let pull_tags t =
      lwt fd = t () in
      lwt () = XAction.write_fd fd Sync_pull_tags in
      XTagKeyss.read_fd fd

    let watch t tags callback =
      lwt fd = t () in
      lwt () = XActionTags.write_fd fd (Sync_watch, tags) in
      let read () =
        try
          lwt event = XEvent.read_fd fd in
          callback event
        with End_of_file ->
          return_unit in
      read ()

  end

end

module type SERVER = sig
  type t = IrminIO.Lwt_channel.t
  module State: STORE
  val run: State.t -> ?timeout:float -> t -> unit Lwt.t
end

module Server (S: STORE) = struct

  let debug fmt = IrminMisc.debug "SERVER" fmt

  open IrminIO

  module State = S

  module C = S.C

  type t = Lwt_channel.t

  module Key_store = S.Key_store
  module Value_store = S.Value_store
  module Tag_store = S.Tag_store

  module XKey = Channel(Key)
  module XKeys = Channel(Key.Set)
  module XKeyKeys = Channel(Pair(Key)(XKeys))
  module XKeyPair = Channel(Pair(Key)(Key))
  module XKeyPairs = Channel(List(XKeyPair))
  module XKeysO = Channel(Option(XKeys))
  module XKeysOKeysO = Channel(Pair(XKeysO)(XKeysO))

  module XValue = Channel(Value)
  module XValueOption = Channel(Option(Value))

  module XTag = Channel(Tag)
  module XTags = Channel(Tag.Set)
  module XKeysTags = Channel(Pair(XKeys)(XTags))
  module XTagKeys = Channel(Pair(Tag)(XKeys))
  module XTagKeyss = Channel(List(XTagKeys))

  module XGraph = Channel(Key.Graph)
  module XGraphTagKeyss = Channel(Pair(XGraph)(List(Pair(Tag)(Key.Set))))
  module Sync = IrminSync.Make(S)
  module XEvent = Channel(Sync.Event)

  let write_unit = Lwt_channel.write_unit

  module XKey_store = struct

    let proj = State.key_store

    let add t buf fd =
      let (k1, k2s) = XKeyKeys.get buf in
      lwt () = Key_store.add (proj t) k1 k2s in
      write_unit fd

    let keys t buf fd =
      let (sources, sinks) = XKeysOKeysO.get buf in
      lwt g = Key_store.keys (proj t) ?sources ?sinks () in
      XGraph.write_fd fd g

    let pred t buf fd =
      let k = XKey.get buf in
      lwt keys = Key_store.pred (proj t) k in
      XKeys.write_fd fd keys

  end

  module XValue_store = struct

    let proj = State.value_store

    let write t buf fd =
      let v = XValue.get buf in
      lwt k = Value_store.write (proj t) v in
      XKey.write_fd fd k

    let read t buf fd =
      let k = XKey.get buf in
      lwt vo = Value_store.read (proj t) k in
      XValueOption.write_fd fd vo

  end

  module XTag_store = struct

    let proj = State.tag_store

    let update t buf fd =
      let (tag, keys) = XTagKeys.get buf in
      lwt () = Tag_store.update (proj t) tag keys in
      write_unit fd

    let remove t buf fd =
      let tag = XTag.get buf in
      lwt () = Tag_store.remove (proj t) tag in
      write_unit fd

    let read t buf fd =
      let tag = XTag.get buf in
      lwt keys = Tag_store.read (proj t) tag in
      XKeys.write_fd fd keys

    let all t _ fd =
      lwt tags = Tag_store.all (proj t) in
      XTags.write_fd fd tags

  end

  module XSync = struct

    let pull_keys _ buf fd =
      let (sources, sinks) = XKeysTags.get buf in
      lwt g = Sync.pull_keys () ~sources ~sinks in
      XGraph.write_fd fd g

    let pull_tags _ _ fd =
      lwt tags = Sync.pull_tags () in
      XTagKeyss.write_fd fd tags

    let watch _ buf fd =
      let tags = XTags.get buf in
      try_lwt
        Sync.watch () tags (XEvent.write_fd fd)
      with _ ->
        IrminIO.Lwt_channel.close fd

  end

  let channel (fd, sockaddr) =
    let name = match sockaddr with
      | Lwt_unix.ADDR_UNIX s         -> "unix-"^s
      | Lwt_unix.ADDR_INET (ip,port) ->
        "inet-"^Unix.string_of_inet_addr ip^":"^string_of_int port in
    IrminIO.Lwt_channel.create fd name

  let run t ?timeout listen =
    let process client =
      let client = channel client in
      Printf.printf "New connection from %s\n%!" (Lwt_channel.name client);
      lwt len = IrminIO.Lwt_channel.read_length client in
      debug " ... processs";
      lwt buf = Lwt_channel.read_buf client len in
      let action = Action.get buf in
      let fn = match action with
        | Key_add          -> XKey_store.add
        | Key_list         -> XKey_store.keys
        | Key_pred         -> XKey_store.pred
        | Value_write      -> XValue_store.write
        | Value_read       -> XValue_store.read
        | Tag_update       -> XTag_store.update
        | Tag_remove       -> XTag_store.remove
        | Tag_read         -> XTag_store.read
        | Tag_list         -> XTag_store.all
        | Sync_pull_keys   -> XSync.pull_keys
        | Sync_pull_tags   -> XSync.pull_tags
        | Sync_watch       -> XSync.watch in
      fn t buf client in
    Printf.printf "Listening on %s.\n%!" (Lwt_channel.name listen);
    Sys.catch_break true;
    try
      while_lwt true do
        lwt client = Lwt_unix.accept (Lwt_channel.channel listen) in
        let events = match timeout with
          | None   -> [ process client ]
          | Some t ->
            let timeout =
              lwt () = Lwt_unix.sleep t in
              Printf.printf "Timeout!\n%!";
              return () in
            [ process client; timeout ] in
        lwt () =
          try_lwt Lwt.pick events
          with _ -> return () in
        try_lwt
          Lwt_unix.close (fst client)
        with _ ->
          return_unit
      done
    with e ->
      Printf.printf "Closing connection ...\n%!";
      lwt () = Lwt_channel.close listen in
      raise_lwt e

end
