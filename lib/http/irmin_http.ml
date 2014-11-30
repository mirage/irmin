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

let (/) = Filename.concat

module Helper (Client: Cohttp_lwt.Client) = struct

  exception Error of string

  let uri_append t path = match Uri.path t :: path with
    | []   -> t
    | path -> Uri.with_path t (Irmin.Path.String.to_hum path)

  type ('a, 'b) response =
    (Ezjsonm.t -> 'a) -> (Cohttp.Response.t * Cohttp_lwt_body.t) -> 'b

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

  let map_string_response fn (r,b) =
    Cohttp_lwt_body.to_string b >>= fun b ->
    Log.debugf "response: body=%s" b;
    let j = Ezjsonm.from_string b in
    try return (fn (result_of_json j))
    with Error e -> fail (Error e)

  let map_stream_response fn (r,b) =
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

    let list { uri; _ } keys =
      get uri ("list" :: List.map K.to_hum keys) (Ezjsonm.get_list K.of_json)

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

  module XContents = struct
    module Key = H
    module Val = C
    include AO(struct
        let suffix = Some "contents"
      end)(Key)(Val)
  end

  module XNode = struct
    module Key = H
    module Path = P
    module Val = IB.Node.Make(H)(H)(P)
    include AO(struct
        let suffix = Some "node"
      end)(Key)(Val)
  end

  module XCommit = struct
    module Key = H
    module Val = IB.Commit.Make(H)(H)
    include AO(struct
        let suffix = Some "commit"
      end)(Key)(Val)
  end

  module XTag = struct
    module Key = T
    module Val = H
    include RW(struct
        let suffix = Some "tag"
      end)(Key)(Val)
  end

  module XSync = IB.Sync.None(H)(T)

  include IB.Make_ext(XContents)(XNode)(XCommit)(XTag)(XSync)
end

module Make (Client: Cohttp_lwt.Client)
    (P: Irmin.Path.S)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct

  include Helper (Client)

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

  (* The high-level bindings: every high-level operation is simply
     forwarded to the HTTP server. *much* more efficient than using
     [L]. *)
  module S = L.RW(struct let suffix = None end)(P)(C)

  (* [t.s.uri] always point to the right location:
       - `$uri/` if branch = `Tag T.master
       - `$uri/tree/$tag` if branch = `Tag tag
       - `$uri/tree/$key if key = `Key key *)
  type t = {
    mutable branch: [`Tag of T.t | `Head of H.t];
    mutable s: S.t;
    l: L.t;
  }

  let uri t = t.s.S.uri

  let reset_uri t =
    let reset () =
      let path = Filename.(dirname (dirname (Uri.path t.s.S.uri))) in
      S.set_path t.s path
    in
    match t.branch with
    | `Head _  -> reset ()
    | `Tag tag -> if T.equal tag T.master then () else reset ()

  let set_tag t tag =
    reset_uri t;
    t.branch <- `Tag tag;
    let tag = T.to_hum tag in
    S.append_path t.s ["branch"; tag]

  let set_head t head =
    reset_uri t;
    t.branch <- `Head head;
    let head = H.to_hum head in
    S.append_path t.s ["head"; head]

  let with_head t fn =
    fn t.l >>= fun res ->
    L.head_exn t.l >>= fun head ->
    set_head t head;
    return res

  type key = S.key
  type value = S.value
  type head = H.t
  type tag = T.t

  let create config task =
    L.create config task >>= fun l ->
    S.create config task >>= fun s ->
    let branch = `Tag T.master in
    return { branch; l; s }

  let of_tag config task tag =
    create config task >>= fun t ->
    t.branch <- `Tag tag;
    if T.equal tag T.master then return t
    else
      let tag = T.to_hum tag in
      S.append_path t.s ["tree"; tag];
      return t

  let of_head config task head =
    S.create config task >>= fun s ->
    S.append_path s ["tree"; H.to_hum head];
    L.of_head config task head >>= fun l ->
    return { s; l; branch = `Head head }

  let config t = S.config t.s
  let task t = S.task t.s
  let read t = S.read t.s
  let read_exn t = S.read_exn t.s
  let mem t = S.mem t.s
  let list t = S.list t.s
  let dump t = S.dump t.s
  let watch t = S.watch t.s
  let remove t = S.remove t.s

  let update t key value =
    match t.branch with
    | `Head h  -> with_head t (fun l -> L.update l key value)
    | `Tag tag -> S.update t.s key value

  let tag t = match t.branch with
    | `Head _ -> None
    | `Tag t  -> Some t

  let tag_exn t = match tag t with
    | None   -> raise Not_found
    | Some t -> t

  let head t = match t.branch with
    | `Head k -> return (Some k)
    | `Tag _  -> get (uri t) ["head"] (Ezjsonm.get_option H.of_json)

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
    | `Head _ -> with_head t (fun l -> L.switch l tag)
    | `Tag _ ->
      get (uri t) ["switch"; T.to_hum tag] Ezjsonm.get_unit >>= fun () ->
      set_tag t tag;
      return_unit

  let heads t =
    get (uri t) ["heads"] (Ezjsonm.get_list H.of_json)

  let detach t =
    match t.branch with
    | `Head k -> return_unit
    | `Tag _  ->
      head t >>= function
      | None   -> return_unit
      | Some h ->
        set_head t h;
        return_unit

  let update_head t head =
    match t.branch with
    | `Head _ -> with_head t (fun l -> L.update_head l head)
    | `Tag _  -> get (uri t) ["update-head"; H.to_hum head] Ezjsonm.get_unit

  module M = Tc.App1 (Irmin.Merge.Result) (Tc.Unit)

  let merge_head t head =
    match t.branch with
    | `Head k -> with_head t (fun l -> L.merge_head l head)
    | `Tag _  -> get (uri t) ["merge-head"; H.to_hum head] M.of_json

  module W = Tc.Pair (P)(H)

  let watch_head t key =
    match t.branch with
    | `Head _ -> Lwt_stream.of_list []
    | `Tag _  -> get_stream (uri t) ["watch-head"; P.to_hum key] W.of_json

  let clone t tag =
    match t.branch with
    | `Head _ ->
      with_head t (fun l ->
          L.clone l tag >>= function
          | `Ok l -> return (`Ok { t with l })
          | `Duplicated_tag -> return `Duplicated_tag)
    | `Tag _  ->
      get (uri t) ["clone"; T.to_hum tag] Ezjsonm.get_string >>= function
      | "ok" -> of_tag (config t) (task t) tag >>= fun t -> return (`Ok t)
      | _    -> return `Duplicated_tag

  module Branch = struct
    let of_json = function
      | `O [ "tag" , j] -> `Tag (T.of_json j)
      | `O [ "head", j] -> `Head (H.of_json j)
      | j -> Ezjsonm.parse_error j "Irmin_http.Branch.json_of"
  end

  let clone_force t tag =
    match t.branch with
    | `Head _ -> with_head t (fun l ->
        L.clone_force l tag >>= fun l ->
        return { t with l })
    | `Tag _  ->
      get (uri t) ["clone-force"; T.to_hum tag] Branch.of_json >>= function
      | `Head head -> of_head (config t) (task t) head
      | `Tag tag   -> of_tag (config t) (task t) tag

  let merge t tag =
    match t.branch with
    | `Head _ -> with_head t (fun l -> L.merge l tag)
    | `Tag _  -> get (uri t) ["merge"; T.to_hum tag] M.of_json

  module E = Tc.Pair
      (Tc.Pair (Tc.Option(Tc.Bool)) (Tc.Option(Tc.Int)))
      (Tc.Pair (Tc.List(H)) (Tc.List(H)))

  type slice = L.slice

  module Slice = L.Slice

  let export ?full ?depth ?(min=[]) ?(max=[]) t =
    post (uri t) ["export"] (E.to_json ((full, depth), (min, max)))
      L.Slice.of_json

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
  module View = L.View
  module Snapshot = L.Snapshot
  module Dot = L.Dot
  module Sync = L.Sync
end
