(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log = Log.Make(struct let section = "CRUD" end)

(* ~uri *)
let uri_key =
  Irmin.Conf.key
    ~docv:"URI"
    ~doc:"Location of the remote store."
    "uri" Irmin.Conf.(some uri) None

let config uri =
  Irmin.Conf.add Irmin.Conf.empty uri_key (Some uri)

module type Config = sig val suffix: string option end

module Helper (Client: Cohttp_lwt.Client) = struct

  exception Error of string

  let uri_append t path = match Uri.path t :: path with
    | []   -> t
    | path -> Uri.with_path t (Irmin.Path.String.to_hum path)

  let result_of_json json =
    let error =
      try Some (Ezjsonm.find json ["error"])
      with Not_found -> None in
    let result =
      try Some (Ezjsonm.find json ["result"])
      with Not_found -> None in
    match error, result with
    | None  , None   -> raise (Error "result_of_json")
    | Some e, None   -> raise (Error (Ezjsonm.decode_string_exn e))
    | None  , Some r -> r
    | Some _, Some _ -> raise (Error "result_of_json")

  let map_string_response fn (_, b) =
    Cohttp_lwt_body.to_string b >>= fun b ->
    Log.debugf "response: body=%s" b;
    let j = Ezjsonm.from_string b in
    try return (fn (result_of_json j))
    with Error e -> fail (Error e)

  let map_stream_response fn (_, b) =
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let stream = Lwt_stream.map result_of_json stream in
    Lwt_stream.map fn stream

  let map_get t path fn =
    Log.debugf "get %s" (Uri.to_string (uri_append t path));
    Client.get (uri_append t path) >>=
    fn

  let get t path fn =
    map_get t path (map_string_response fn)

  let get_stream t path fn  =
    let (stream: 'a Lwt_stream.t option ref) = ref None in
    let rec get () =
      match !stream with
      | Some s -> Lwt_stream.get s
      | None   ->
        map_get t path (fun b ->
            let s = map_stream_response fn b in
            stream := Some s;
            return_unit) >>= fun () ->
        get () in
    Lwt_stream.from get

  let delete t path fn =
    Log.debugf "delete %s" (Uri.to_string (uri_append t path));
    Cohttp_lwt_unix.Client.delete (uri_append t path) >>=
    map_string_response fn

  let post t path body fn =
    let body =
      let params = `O [ "params", body ] in
      Ezjsonm.to_string params in
    Log.debugf "post %s %s" (Uri.to_string (uri_append t path)) body;
    let body = Cohttp_lwt_body.of_string body in
    Cohttp_lwt_unix.Client.post ~body (uri_append t path) >>=
    map_string_response fn

end

module Low (Client: Cohttp_lwt.Client)
    (P: Irmin.Path.S)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct

  include Helper (Client)

  module RO (C: Config) (K: Irmin.Hum.S) (V: Tc.S0) = struct

    type t = {
      mutable uri: Uri.t;
      task: Irmin.task;
      config: Irmin.config;
    }

    let task t = t.task
    let config t = t.config

    type key = K.t
    type value = V.t

    let some fn x =
      Some (fn x)

    let create_aux config task =
      let uri = match Irmin.Conf.get config uri_key with
        | None   -> failwith "Irmin_http.create: No URI specified"
        | Some u -> u
      in
      let uri = match C.suffix with
        | None   -> uri
        | Some p -> uri_append uri [p]
      in
      { uri; config; task = task }

    let create config task =
      return (fun a -> create_aux config (task a))

    let read { uri; _ } key =
      catch
        (fun () -> get uri ["read"; K.to_hum key] (some V.of_json))
        (fun _  -> return_none)

    let read_exn { uri; _ } key =
      get uri ["read"; K.to_hum key] V.of_json

    let mem { uri; _ } key =
      get uri ["mem"; K.to_hum key] Ezjsonm.get_bool

    let list { uri; _ } key =
      get uri ["list"; K.to_hum key] (Ezjsonm.get_list K.of_json)

    let dump { uri; _ } =
      get uri ["dump"] (Ezjsonm.get_list (Ezjsonm.get_pair K.of_json V.of_json))

  end

  module AO (C: Config) (K: Irmin.Hash.S) (V: Tc.S0) = struct

    include RO (C)(K)(V)

    let add { uri; _ } value =
      post uri ["add"] (V.to_json value) K.of_json

  end

  module RW (C: Config) (K: Irmin.Hum.S) (V: Tc.S0) = struct

    include RO (C)(K)(V)

    let update { uri; _ } key value =
      post uri ["update"; K.to_hum key] (V.to_json value) Ezjsonm.get_unit

    let remove { uri; _ } key =
      delete uri ["remove"; K.to_hum key] Ezjsonm.get_unit

    let watch { uri; _ } path =
      get_stream uri ["watch"; K.to_hum path] (Ezjsonm.get_option V.of_json)

  end

  module X = struct
    module Contents = Irmin.Contents.Make(struct
        module Key = H
        module Val = C
        include AO(struct let suffix = Some "contents" end)(H)(C)
      end)
    module Node = struct
      module Key = H
      module Path = P
      module Val = Irmin.Node.Make(H)(H)(P)
      include AO(struct let suffix = Some "node" end)(Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Irmin.Commit.Make(H)(H)
      include AO(struct let suffix = Some "commit" end)(Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW(struct let suffix = Some "tag" end)(Key)(Val)
    end
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)(Tag)
    module Sync = Irmin.Private.Sync.None(H)(T)
  end

  include Irmin.Make_ext(X)
end


module AO (Client: Cohttp_lwt.Client) (K: Irmin.Hash.S) (V: Tc.S0) = struct
  module V = struct
    include V
    let merge ~old:_ _ _ = failwith "Irmin_git.AO.merge"
  end
  module M = Low (Client)(Irmin.Path.String)(V)(Irmin.Tag.String_list)(K)
  include M.X.Contents
end

module RW (Client: Cohttp_lwt.Client) (K: Irmin.Hum.S) (V: Irmin.Hash.S) = struct
  module K = struct
    include K
    let master = K.of_hum "master"
  end
  module M = Low (Client)(Irmin.Path.String)(Irmin.Contents.String)(K)(V)
  include M.X.Tag
end

module Make (Client: Cohttp_lwt.Client)
    (P: Irmin.Path.S)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct

  include Helper (Client)
  module Head = H

  (* Implementing a high-level HTTP BC backend is a bit tricky as we
     need to keep track of some hidden state which is not directly
     exposed by the interface. This is the case when we are in
     `detached` mode, and an high-level update does not return the new
     head value.

     We solve this by tapping information in lower-level bindings. *)

  (* The low-level bindings: every high-level operation is decomposed
     into lower level operations at the backend level. For instance
     inserting a new value in the store (1 high-level operation) will
     result in multiple low-level operations: read the corresponding
     tree nodes for the sub-directories, creating new tree nodes,
     etc. *)
  module L = Low(Client)(P)(C)(T)(H)
  module LP = L.Private

  (* The high-level bindings: every high-level operation is simply
     forwarded to the HTTP server. *much* more efficient than using
     [L]. *)
  module H = L.RW(struct let suffix = None end)(P)(C)

  (* [t.s.uri] always point to the right location:
       - `$uri/` if branch = `Tag T.master
       - `$uri/tree/$tag` if branch = `Tag tag
       - `$uri/tree/$key if key = `Key key *)
  type t = {
    mutable branch: [`Tag of T.t | `Head of L.t ];
    mutable h: H.t;
    contents_t: LP.Contents.t;
    node_t: LP.Node.t;
    commit_t: LP.Commit.t;
    tag_t: LP.Tag.t;
    read_node: L.key -> LP.Node.key option Lwt.t;
    mem_node: L.key -> bool Lwt.t;
    update_node: L.key -> LP.Node.key -> unit Lwt.t;
  }

  let uri t =
    let base = t.h.H.uri in
    match t.branch with
    | `Tag tag ->
      if T.equal tag T.master then return base
      else return (uri_append base ["tree"; T.to_hum tag])
    | `Head h ->
      L.head_exn h >>= fun head ->
      return (uri_append base ["tree"; Head.to_hum head])

  let config t = H.config t.h
  let task t = H.task t.h

  let branch t = match t.branch with
    | `Head l -> L.branch l
    | `Tag  t -> `Tag t

  let set_tag t tag = t.branch <- `Tag tag
  let set_head t head = t.branch <- `Head head

  type key = H.key
  type value = H.value
  type head = L.head
  type tag = L.tag

  let create config task =
    H.create config task >>= fun h ->
    L.create config task >>= fun l ->
    let fn a =
      let h = h a in
      let l = l a in
      let branch = `Tag T.master in
      let contents_t = LP.contents_t l in
      let node_t = LP.node_t l in
      let commit_t = LP.commit_t l in
      let tag_t = LP.tag_t l in
      let read_node = LP.read_node l in
      let mem_node = LP.mem_node l in
      let update_node = LP.update_node l in
      { branch; h; contents_t; node_t; commit_t; tag_t;
        read_node; mem_node; update_node; }
    in
    return fn

  let of_tag config task tag =
    create config task >>= fun t ->
    return (fun a ->
        let t = t a in
        set_tag t tag;
        t
      )

  let of_head config task head =
    H.create config task >>= fun h ->
    L.of_head config task head >>= fun l ->
    let fn a =
      let h = h a in
      let l = l a in
      let branch = `Tag T.master in
      let contents_t = LP.contents_t l in
      let node_t = LP.node_t l in
      let commit_t = LP.commit_t l in
      let tag_t = LP.tag_t l in
      let read_node = LP.read_node l in
      let mem_node = LP.mem_node l in
      let update_node = LP.update_node l in
      { branch; h; contents_t; node_t; commit_t; tag_t;
        read_node; mem_node; update_node; }
    in
    return fn

  let read t = H.read t.h
  let read_exn t = H.read_exn t.h
  let mem t = H.mem t.h
  let list t = H.list t.h
  let dump t = H.dump t.h
  let watch t = H.watch t.h
  let remove t = H.remove t.h

  let update t key value =
    match t.branch with
    | `Head l  -> L.update l key value
    | `Tag _   -> H.update t.h key value

  let tag t = match t.branch with
    | `Head _ -> None
    | `Tag t  -> Some t

  let tag_exn t = match tag t with
    | None   -> raise Not_found
    | Some t -> t

  let tags t =
    uri t >>= fun uri ->
    get uri ["tags"] (Ezjsonm.get_list T.of_json)

  let head t = match t.branch with
    | `Head l -> L.head l
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["head"] (Ezjsonm.get_option Head.of_json)

  let head_exn t =
    head t >>= function
    | None   -> fail Not_found
    | Some h -> return h

  let update_tag t tag =
    uri t >>= fun uri ->
    get uri ["update-tag"; T.to_hum tag] Ezjsonm.get_string >>= function
    | "ok" -> set_tag t tag; return `Ok
    | _    -> return `Duplicated_tag

  let update_tag_force t tag =
    uri t >>= fun uri ->
    get uri ["update-tag-force"; T.to_hum tag] Ezjsonm.get_unit >>= fun () ->
    set_tag t tag;
    return_unit

  let switch t tag =
    match t.branch with
    | `Head l -> L.switch l tag
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["switch"; T.to_hum tag] Ezjsonm.get_unit >>= fun () ->
      set_tag t tag;
      return_unit

  let heads t =
    uri t >>= fun uri ->
    get uri ["heads"] (Ezjsonm.get_list Head.of_json)

  let detach t =
    match t.branch with
    | `Head _ -> return_unit
    | `Tag _  ->
      head t >>= function
      | None   -> return_unit
      | Some h ->
        L.of_head (config t) (fun () -> task t) h >>= fun h ->
        set_head t (h ());
        return_unit

  let update_head t head =
    match t.branch with
    | `Head l -> L.update_head l head
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["update-head"; Head.to_hum head] Ezjsonm.get_unit

  module M = Tc.App1 (Irmin.Merge.Result) (Tc.Unit)

  let merge_head t head =
    match t.branch with
    | `Head l -> L.merge_head l head
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["merge-head"; Head.to_hum head] M.of_json

  let merge_head_exn t head =
    merge_head t head >>=
    Irmin.Merge.exn

  module W = Tc.Pair (P)(Head)

  let watch_head t key =
    match t.branch with
    | `Head _ -> Lwt_stream.of_list []
    | `Tag _  ->
      Irmin.Watch.lwt_stream_lift (
        uri t >>= fun uri ->
        let s = get_stream uri ["watch-head"; P.to_hum key] W.of_json in
        return s
      )

  let clone t task tag =
    match t.branch with
    | `Head l ->
      begin L.clone l task tag >>= function
        | `Ok l -> return (`Ok (fun a -> { t with branch = `Head (l a) }))
        | `Duplicated_tag -> return `Duplicated_tag
      end
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["clone"; T.to_hum tag] Ezjsonm.get_string >>= function
      | "ok" -> of_tag (config t) task tag >>= fun t -> return (`Ok t)
      | _    -> return `Duplicated_tag

  let clone_force t task tag =
    match t.branch with
    | `Head l ->
      L.clone_force l task tag >>= fun l ->
      return (fun a -> { t with branch = `Head (l a) })
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["clone-force"; T.to_hum tag] Tc.Unit.of_json >>= fun () ->
      of_tag (config t) task tag

  let merge t tag =
    match t.branch with
    | `Head l -> L.merge l tag
    | `Tag _  ->
      uri t >>= fun uri ->
      get uri ["merge"; T.to_hum tag] M.of_json

  let merge_exn t tag =
    merge t tag >>=
    Irmin.Merge.exn

  module E = Tc.Pair
      (Tc.Pair (Tc.Option(Tc.Bool)) (Tc.Option(Tc.Int)))
      (Tc.Pair (Tc.List(Head)) (Tc.List(Head)))

  type slice = L.slice

  module Slice = L.Private.Slice

  let export ?full ?depth ?(min=[]) ?(max=[]) t =
    uri t >>= fun uri ->
    post uri ["export"] (E.to_json ((full, depth), (min, max)))
      L.Private.Slice.of_json

  module I = Tc.List(T)

  let import t slice =
    uri t >>= fun uri ->
    post uri ["import"] (Slice.to_json slice) I.of_json >>= function
    | [] -> return `Ok
    | l  -> return (`Duplicated_tags l)

  let import_force t slice =
    uri t >>= fun uri ->
    post uri ["import-force"] (Slice.to_json slice) Ezjsonm.get_unit

  type step = P.step
  module Key = P
  module Val = C
  module Tag = T
  module Private = struct
    include L.Private
    let contents_t t = t.contents_t
    let node_t t = t.node_t
    let commit_t t = t.commit_t
    let tag_t t = t.tag_t
    let update_node t = t.update_node
    let read_node t = t.read_node
    let mem_node t = t.mem_node
  end
end
