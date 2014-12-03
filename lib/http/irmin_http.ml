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

module IB = Irmin.Private
module Log = Log.Make(struct let section = "CRUD" end)

(* ~uri *)
module U: Tc.S0 with type t = Uri.t = struct
  type t = Uri.t
  let hash = Hashtbl.hash
  let compare x y = String.compare (Uri.to_string x) (Uri.to_string y)
  let equal x y = (Uri.to_string x) = (Uri.to_string y)
  let to_sexp = Uri.sexp_of_t
  let to_json t = Ezjsonm.encode_string (Uri.to_string t)
  let of_json t = Uri.of_string (Ezjsonm.decode_string_exn t)
  let size_of t = Tc.String.size_of (Uri.to_string t)
  let write t = Tc.String.write (Uri.to_string t)
  let read b = Uri.of_string (Tc.String.read b)
end

let of_uri, to_uri, _uri = IB.Config.univ (module U)
let uri_k = "uttp:uri"
let uri_key t = IB.Config.find t uri_k to_uri

let config uri =
  let uri = [ uri_k, of_uri uri ] in
  IB.Config.of_dict uri

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

  module RO (C: Config) (K: Irmin.HUM) (V: Tc.S0) = struct

    type t = {
      mutable uri: Uri.t;
      task: Irmin.task;
      config: Irmin.config;
    }

    let append_path t path = t.uri <- uri_append t.uri path
    let set_path t path = t.uri <- Uri.with_path t.uri path
    let get_path t = Uri.path t.uri

    let task t = t.task
    let config t = t.config

    type key = K.t
    type value = V.t

    let some fn x =
      Some (fn x)

    let create config task =
      let uri = match uri_key config with
        | None   -> failwith "Irmin_http.create: No URI specified"
        | Some u -> u
      in
      let uri = match C.suffix with
        | None   -> uri
        | Some p -> uri_append uri [p]
      in
      return { uri; config; task }

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

  module RW (C: Config) (K: Irmin.HUM) (V: Tc.S0) = struct

    include RO (C)(K)(V)

    let update { uri; _ } key value =
      post uri ["update"; K.to_hum key] (V.to_json value) Ezjsonm.get_unit

    let remove { uri; _ } key =
      delete uri ["remove"; K.to_hum key] Ezjsonm.get_unit

    let watch { uri; _ } path =
      get_stream uri ["watch"; K.to_hum path] (Ezjsonm.get_option V.of_json)

  end

  module X = struct
    module Contents = struct
      module Key = H
      module Val = C
      include AO(struct
          let suffix = Some "contents"
        end)(Key)(Val)
    end
    module Node = struct
      module Key = H
      module Path = P
      module Val = IB.Node.Make(H)(H)(P)
      include AO(struct
          let suffix = Some "node"
        end)(Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = IB.Commit.Make(H)(H)
      include AO(struct
          let suffix = Some "commit"
        end)(Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW(struct
          let suffix = Some "tag"
        end)(Key)(Val)
    end
    module Slice = IB.Slice.Make(Contents)(Node)(Commit)(Tag)
  end

  include Irmin.Make_ext(X)
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
    mutable branch: [`Tag of T.t | `Head of L.t];
    mutable h: H.t;
    contents_t: LP.Contents.t;
    node_t: LP.Contents.t * LP.Node.t;
    commit_t: LP.Contents.t * LP.Node.t * LP.Commit.t;
    tag_t: LP.Tag.t;
    read_node: L.key -> LP.Node.value option Lwt.t;
    mem_node: L.key -> bool Lwt.t;
    update_node: L.key -> LP.Node.value -> unit Lwt.t;
  }

  let uri t = t.h.H.uri
  let config t = H.config t.h
  let task t = H.task t.h

  let branch t = match t.branch with
    | `Head l -> L.branch l
    | `Tag  t -> `Tag t

  let root_uri t =
    let reset () = Filename.(dirname (dirname (H.get_path t.h))) in
    match t.branch with
    | `Head _  -> reset ()
    | `Tag tag -> if T.equal tag T.master then H.get_path t.h else reset ()

  let reset_uri t =
    H.set_path t.h (root_uri t)

  let set_tag t tag =
    reset_uri t;
    t.branch <- `Tag tag;
    if not (T.equal tag T.master) then
      H.append_path t.h ["tree"; T.to_hum tag]

  let sync_head t l cont =
    reset_uri t;
    L.head_exn l >>= fun head ->
    let head = Head.to_hum head in
    H.append_path t.h ["tree"; head];
    return cont

  let set_head t head =
    L.of_head (config t) (task t) head >>= fun l ->
    sync_head t l ()

  type key = H.key
  type value = H.value
  type head = L.head
  type tag = L.tag

  let create config task =
    H.create config task >>= fun h ->
    L.create config task >>= fun l ->
    let branch = `Tag T.master in
    let contents_t = LP.contents_t l in
    let node_t = LP.node_t l in
    let commit_t = LP.commit_t l in
    let tag_t = LP.tag_t l in
    let read_node = LP.read_node l in
    let mem_node = LP.mem_node l in
    let update_node = LP.update_node l in
    return { branch; h; contents_t; node_t; commit_t; tag_t;
             read_node; mem_node; update_node; }

  let of_tag config task tag =
    create config task >>= fun t ->
    set_tag t tag;
    return t

  let of_head config task head =
    create config task >>= fun t ->
    set_head t head >>= fun () ->
    return t

  let read t = H.read t.h
  let read_exn t = H.read_exn t.h
  let mem t = H.mem t.h
  let list t = H.list t.h
  let dump t = H.dump t.h
  let watch t = H.watch t.h
  let remove t = H.remove t.h

  let update t key value =
    match t.branch with
    | `Head l  -> L.update l key value >>= sync_head t l
    | `Tag _   -> H.update t.h key value

  let tag t = match t.branch with
    | `Head _ -> None
    | `Tag t  -> Some t

  let tag_exn t = match tag t with
    | None   -> raise Not_found
    | Some t -> t

  let tags t =
    get (uri t) ["tags"] (Ezjsonm.get_list T.of_json)

  let head t = match t.branch with
    | `Head l -> L.head l
    | `Tag _  -> get (uri t) ["head"] (Ezjsonm.get_option Head.of_json)

  let head_exn t =
    head t >>= function
    | None   -> fail Not_found
    | Some h -> return h

  let update_tag t tag =
    get (uri t) ["update-tag"; T.to_hum tag] Ezjsonm.get_string >>= function
    | "ok" -> set_tag t tag; return `Ok
    | _    -> return `Duplicated_tag

  let update_tag_force t tag =
    get (uri t) ["update-tag-force"; T.to_hum tag] Ezjsonm.get_unit >>= fun () ->
    set_tag t tag;
    return_unit

  let switch t tag =
    match t.branch with
    | `Head l -> L.switch l tag >>= sync_head t l
    | `Tag _  ->
      get (uri t) ["switch"; T.to_hum tag] Ezjsonm.get_unit >>= fun () ->
      set_tag t tag;
      return_unit

  let heads t =
    get (uri t) ["heads"] (Ezjsonm.get_list Head.of_json)

  let detach t =
    match t.branch with
    | `Head _ -> return_unit
    | `Tag _  ->
      head t >>= function
      | None   -> return_unit
      | Some h -> set_head t h

  let update_head t head =
    match t.branch with
    | `Head l -> L.update_head l head >>= sync_head t l
    | `Tag _  -> get (uri t) ["update-head"; Head.to_hum head] Ezjsonm.get_unit

  module M = Tc.App1 (Irmin.Merge.Result) (Tc.Unit)

  let merge_head t head =
    match t.branch with
    | `Tag _  -> get (uri t) ["merge-head"; Head.to_hum head] M.of_json
    | `Head l -> L.merge_head l head >>= sync_head t l

  module W = Tc.Pair (P)(Head)

  let watch_head t key =
    match t.branch with
    | `Head _ -> Lwt_stream.of_list []
    | `Tag _  -> get_stream (uri t) ["watch-head"; P.to_hum key] W.of_json

  let clone t tag =
    match t.branch with
    | `Head l ->
      begin L.clone l tag >>= function
        | `Ok l ->
          let t = { t with branch = `Head l } in
          sync_head t l (`Ok t)
        | `Duplicated_tag -> return `Duplicated_tag
      end
    | `Tag _  ->
      get (uri t) ["clone"; T.to_hum tag] Ezjsonm.get_string >>= function
      | "ok" -> of_tag (config t) (task t) tag >>= fun t -> return (`Ok t)
      | _    -> return `Duplicated_tag

  module Branch = struct
    let of_json = function
      | `O [ "tag" , j] -> `Tag (T.of_json j)
      | `O [ "head", j] -> `Head (Head.of_json j)
      | j -> Ezjsonm.parse_error j "Irmin_http.Branch.json_of"
  end

  let clone_force t tag =
    match t.branch with
    | `Head l ->
      begin L.clone_force l tag >>= fun l ->
        let t = { t with branch = `Head l } in
        sync_head t l t
      end
    | `Tag _  ->
      get (uri t) ["clone-force"; T.to_hum tag] Branch.of_json >>= function
      | `Head head -> of_head (config t) (task t) head
      | `Tag tag   -> of_tag (config t) (task t) tag

  let merge t tag =
    match t.branch with
    | `Head l -> L.merge l tag >>= sync_head t l
    | `Tag _  -> get (uri t) ["merge"; T.to_hum tag] M.of_json

  module E = Tc.Pair
      (Tc.Pair (Tc.Option(Tc.Bool)) (Tc.Option(Tc.Int)))
      (Tc.Pair (Tc.List(Head)) (Tc.List(Head)))

  type slice = L.slice

  module Slice = L.Private.Slice

  let export ?full ?depth ?(min=[]) ?(max=[]) t =
    post (uri t) ["export"] (E.to_json ((full, depth), (min, max)))
      L.Private.Slice.of_json

  module I = Tc.List(T)

  let import t slice =
    post (uri t) ["import"] (Slice.to_json slice) I.of_json >>= function
    | [] -> return `Ok
    | l  -> return (`Duplicated_tags l)

  let import_force t slice =
    post (uri t) ["import-force"] (Slice.to_json slice) Ezjsonm.get_unit

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
