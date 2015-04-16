(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Irmin.Merge.OP

module Log = Log.Make(struct let section = "HTTP" end)

(* ~uri *)
let uri =
  Irmin.Private.Conf.key
    ~docv:"URI"
    ~doc:"Location of the remote store."
    "uri" Irmin.Private.Conf.(some uri) None

let config x =
  Irmin.Private.Conf.singleton uri (Some x)

let invalid_arg fmt =
  Printf.ksprintf (fun str -> Lwt.fail (Invalid_argument str)) fmt

module type Config = sig
  val suffix: string option
end

module Helper (Client: Cohttp_lwt.Client) = struct

  exception Error of string

  let uri_append t path = match Uri.path t :: path with
    | []   -> t
    | path ->
      let buf = Buffer.create 10 in
      List.iter (function
          | "" -> ()
          | s  ->
            if s.[0] <> '/' then Buffer.add_char buf '/';
            Buffer.add_string buf s;
        ) path;
      let path = Buffer.contents buf in
      Uri.with_path t path

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

  let map_string_response (type t) (module M: Tc.S0 with type t = t) (_, b) =
    Cohttp_lwt_body.to_string b >>= fun b ->
    Log.debug "got response: %s" b;
    let j = Ezjsonm.from_string b in
    try
      Ezjsonm.value j
      |> result_of_json
      |> M.of_json
      |> return
    with Error e ->
      fail (Error e)

  let map_stream_response (type t) (module M: Tc.S0 with type t = t) (_, b) =
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let stream = Lwt_stream.map result_of_json stream in
    Lwt_stream.map (fun j ->
        Log.debug "stream get %s" Ezjsonm.(to_string (wrap j));
        M.of_json j
      ) stream

  let headers = Cohttp.Header.of_list [
      "Connection", "Keep-Alive"
    ]

  let map_get t path ?query fn =
    let uri = uri_append t path in
    let uri = match query with
      | None   -> uri
      | Some q -> Uri.with_query uri q
    in
    Log.debug "get %s" (Uri.path uri);
    Client.get ~headers uri >>= fun r ->
    fn r

  let get t path ?query fn =
    map_get t path ?query (map_string_response fn)

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
    let uri = uri_append t path in
    Log.debug "delete %s" (Uri.path uri);
    Client.delete uri >>=
    map_string_response fn

  let post t path ?query body fn =
    let body =
      let params = `O [ "params", body ] in
      Ezjsonm.to_string params in
    let uri = uri_append t path in
    let short_body =
      if String.length body > 80 then String.sub body 0 80 ^ ".." else body
    in
    let uri = match query with
      | None   -> uri
      | Some q -> Uri.with_query uri q
    in
    Log.debug "post %s %s" (Uri.path uri) short_body;
    let body = Cohttp_lwt_body.of_string body in
    Client.post ~body ~headers uri >>=
    map_string_response fn

end


module XRO (Client: Cohttp_lwt.Client) (C: Config) (K: Irmin.Hum.S) (V: Tc.S0) =
struct

  include Helper (Client)

  type t = {
    mutable uri: Uri.t;
    task: Irmin.task;
  }

  let task t = t.task

  type key = K.t
  type value = V.t

  let create_aux config task =
    let uri = match Irmin.Private.Conf.get config uri with
      | None   -> failwith "Irmin_http.create: No URI specified"
      | Some u -> u
    in
    let uri = match C.suffix with
      | None   -> uri
      | Some p -> uri_append uri [p]
    in
    { uri; task = task }

  let create config task =
    return (fun a -> create_aux config (task a))

  let read { uri; _ } key =
    get uri ["read"; K.to_hum key] (module Tc.Option(V))

  let err_not_found n k =
    invalid_arg "Irmin_http.%s: %s not found" n (K.to_hum k)

  let read_exn t key =
    read t key >>= function
    | None   -> err_not_found "read" key
    | Some v -> return v

  let mem { uri; _ } key =
    get uri ["mem"; K.to_hum key] Tc.bool

  let iter t fn =
    let fn key = fn key (read_exn t key) in
    Lwt_stream.iter_p fn (get_stream t.uri ["iter"] (module K))

end

module XAO (Client: Cohttp_lwt.Client) (C: Config) (K: Irmin.Hash.S) (V: Tc.S0) =
struct

  include XRO (Client)(C)(K)(V)

  let add { uri; _ } value =
    post uri ["add"] (V.to_json value) (module K)

end

module XRW (Client: Cohttp_lwt.Client) (C: Config) (K: Irmin.Hum.S) (V: Tc.S0) =
struct

  include XRO (Client)(C)(K)(V)

  let update { uri; _ } key value =
    post uri ["update"; K.to_hum key] (V.to_json value) Tc.unit

  let remove { uri; _ } key =
    delete uri ["remove"; K.to_hum key] Tc.unit

  module CS = Tc.Pair(Tc.Option(V))(Tc.Option(V))

  let compare_and_set { uri; _ } key ~test ~set =
    post uri ["compare-and-set"; K.to_hum key] (CS.to_json (test, set))
      Tc.bool

  let watch { uri; _ } path =
    get_stream uri ["watch"; K.to_hum path] (module Tc.Option(V))

  let watch_all { uri; _ } =
    get_stream uri ["watch-all"] (module Tc.Pair(K)(Tc.Option(V)))

end

module Low (Client: Cohttp_lwt.Client)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct
  module X = struct
    module Contents = Irmin.Contents.Make(struct
        module Key = H
        module Val = C
        include XAO(Client)(struct let suffix = Some "contents" end)(H)(C)
      end)
    module Node = struct
      module Key = H
      module Path = C.Path
      module Val = Irmin.Private.Node.Make(H)(H)(C.Path)
      include XAO(Client)(struct let suffix = Some "node" end)(Key)(Val)
    end
    module Commit = struct
      module Key = H
      module Val = Irmin.Private.Commit.Make(H)(H)
      include XAO(Client)(struct let suffix = Some "commit" end)(Key)(Val)
    end
    module Tag = struct
      module Key = T
      module Val = H
      include XRW(Client)(struct let suffix = Some "tag" end)(Key)(Val)
    end
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = Irmin.Private.Sync.None(H)(T)
  end
  include Irmin.Make_ext(X)
end

module Make (Client: Cohttp_lwt.Client)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
    (H: Irmin.Hash.S) =
struct

  module T = struct
    include T
    let to_hum t = Uri.pct_encode (to_hum t)
    let of_hum t = of_hum (Uri.pct_decode t)
  end

  module P = struct

    include C.Path

    let to_hum t =
      String.concat "/" (C.Path.map t (fun x -> Uri.pct_encode (Step.to_hum x)))

    let of_hum t =
      List.filter ((<>)"") (Stringext.split t ~on:'/')
      |> List.map (fun x -> Step.of_hum (Uri.pct_decode x))
      |> C.Path.create

  end

  include Helper (Client)

  (* Implementing a high-level HTTP BC backend is a bit tricky as we
     need to keep track of some hidden state which is not directly
     exposed by the interface. This is the case when we are in
     `detached` mode, and an high-level update does not return the new
     head value.

     We solve this by tapping updating the HTTP API to return more
     information than the OCaml API dictates. in lower-level
     bindings. *)

  (* The high-level bindings: every high-level operation is simply
     forwarded to the HTTP server. *much* more efficient than using
     [L]. *)
  module L = Low(Client)(C)(T)(H)
  module LP = L.Private
  module S  = XRW(Client)(struct let suffix = None end)(P)(C)

  (* [t.s.uri] always point to the right location:
       - `$uri/` if branch = `Tag T.master
       - `$uri/tree/$tag` if branch = `Tag tag
       - `$uri/tree/$key if key = `Key key *)
  type t = {
    branch: [`Tag of T.t | `Head of H.t | `Empty] ref;
    mutable h: S.t;
    config: Irmin.config;
    contents_t: LP.Contents.t;
    node_t: LP.Node.t;
    commit_t: LP.Commit.t;
    tag_t: LP.Tag.t;
    read_node: L.key -> LP.Node.key option Lwt.t;
    mem_node: L.key -> bool Lwt.t;
    update_node: L.key -> LP.Node.key -> unit Lwt.t;
    lock: Lwt_mutex.t;
  }

  let branch t = !(t.branch)

  let uri t =
    let base = t.h.S.uri in
    match branch t with
    | `Tag tag ->
      if T.equal tag T.master then base
      else uri_append base ["tree"; T.to_hum tag]
    | `Empty  -> failwith "TODO"
    | `Head h -> uri_append base ["tree"; H.to_hum h]

  let task t = S.task t.h
  let set_tag t tag = t.branch := `Tag tag
  let set_head t = function
    | None   -> t.branch := `Empty
    | Some h -> t.branch := `Head h

  type key = S.key
  type value = S.value
  type head = L.head
  type tag = L.tag

  let create_aux branch config h l =
    let fn a =
      let h = h a in
      let l = l a in
      let contents_t = LP.contents_t l in
      let node_t = LP.node_t l in
      let commit_t = LP.commit_t l in
      let tag_t = LP.tag_t l in
      let read_node = LP.read_node l in
      let mem_node = LP.mem_node l in
      let update_node = LP.update_node l in
      let lock = Lwt_mutex.create () in
      { branch; h; contents_t; node_t; commit_t; tag_t;
        read_node; mem_node; update_node; config;
        lock; }
    in
    return fn

  let create config task =
    S.create config task >>= fun h ->
    L.create config task >>= fun l ->
    let branch = ref (`Tag T.master) in
    create_aux branch config h l

  let of_tag config task tag =
    S.create config task >>= fun h ->
    L.of_tag config task tag >>= fun l ->
    let branch = ref (`Tag tag) in
    create_aux branch config h l

  let of_head config task head =
    S.create config task >>= fun h ->
    L.of_head config task head >>= fun l ->
    let branch = ref (`Head head) in
    create_aux branch config h l

  let empty config task =
    S.create config task >>= fun h ->
    L.empty config task  >>= fun l ->
    let branch = ref `Empty in
    create_aux branch config h l

  let read t key =
    get (uri t) ["read"; P.to_hum key] (module Tc.Option(C))

  let err_not_found n k =
    invalid_arg "Irmin_http.%s: %s not found" n (P.to_hum k)

  let err_no_head = invalid_arg "Irmin_http.%s: no head"
  let err_not_persistent = invalid_arg "Irmin_http.%s: not a persistent branch"

  let read_exn t key =
    read t key >>= function
    | None   -> err_not_found "read" key
    | Some v -> return v

  let mem t key =
    get (uri t) ["mem"; P.to_hum key] Tc.bool

  let iter t fn =
    let fn key = fn key (read_exn t key) in
    Lwt_stream.iter_p fn (get_stream (uri t) ["iter"] (module P))

  let watch t path =
    get_stream (uri t) ["watch"; P.to_hum path] (module Tc.Option(C))

  let watch_all t =
    get_stream (uri t) ["watch-all";] (module Tc.Pair(P)(Tc.Option(C)))

  let update t key value =
    post (uri t) ["update"; P.to_hum key] (C.to_json value) (module H)
    >>= fun h ->
    let () = match branch t with
      | `Empty
      | `Head _ -> set_head t (Some h)
      | `Tag  _ -> ()
    in
    return_unit

  let remove t key =
    delete (uri t) ["remove"; P.to_hum key] (module H) >>= fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h); return_unit
    | `Tag _  -> return_unit

  module CS = Tc.Pair(Tc.Option(C))(Tc.Option(C))

  let compare_and_set t key ~test ~set =
    post (uri t) ["compare-and-set"; P.to_hum key] (CS.to_json (test, set))
      Tc.bool

  let tag t = match branch t with
    | `Empty
    | `Head _ -> Lwt.return_none
    | `Tag t  -> Lwt.return (Some t)

  let tag_exn t = tag t >>= function
    | None   -> err_not_persistent "tag"
    | Some t -> Lwt.return t

  let tags t =
    get (uri t) ["tags"] (module Tc.List(T))

  let head t = match branch t with
    | `Empty  -> Lwt.return_none
    | `Head h -> return (Some h)
    | `Tag _  -> get (uri t) ["head"] (module Tc.Option(H))

  let head_exn t =
    head t >>= function
    | None   -> err_no_head "head"
    | Some h -> return h

  let update_tag t tag =
    get (uri t) ["update-tag"; T.to_hum tag] Tc.unit >>= fun () ->
    set_tag t tag;
    return_unit

  let remove_tag t = match branch t with
    | `Empty
    | `Head _  -> Lwt.return_unit
    | `Tag _   -> get (uri t) ["remove-tag"] Tc.unit

  let heads t =
    get (uri t) ["heads"] (module Tc.List(H))

  let update_head t head = match branch t with
    | `Empty
    | `Head _ -> set_head t (Some head); return_unit
    | `Tag _  -> get (uri t) ["update-head"; H.to_hum head] Tc.unit

  module CSH = Tc.Pair(Tc.Option(H))(Tc.Option(H))

  let compare_and_set_head_unsafe t ~test ~set = match branch t with
    | `Tag _  ->
      post (uri t) ["compare-and-set-head"] (CSH.to_json (test, set)) Tc.bool
    | `Empty  ->
      if None = test then (set_head t set; Lwt.return true) else Lwt.return false
    | `Head h ->
      if Some h = test then (set_head t set; Lwt.return true) else Lwt.return false

  let compare_and_set_head t ~test ~set =
    Lwt_mutex.with_lock t.lock (fun () ->
        compare_and_set_head_unsafe t ~test ~set
      )

  module M = Tc.App1 (Irmin.Merge.Result) (H)

  let mk_query ?max_depth ?n () =
    let max_depth = match max_depth with
      | None   -> []
      | Some i -> ["depth", [string_of_int i]]
    in
    let n = match n with
      | None   -> []
      | Some i -> ["n", [string_of_int i]]
    in
    match max_depth @ n with
    | [] -> None
    | q  -> Some q

  let fast_forward_head_unsafe t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    get (uri t) ?query ["fast-forward-head"; H.to_hum head] Tc.bool >>= fun b ->
    match branch t with
    | `Tag _  -> Lwt.return b
    | `Empty
    | `Head _ -> if b then set_head t (Some head); Lwt.return b

  let fast_forward_head t ?max_depth ?n head =
    Lwt_mutex.with_lock t.lock (fun () ->
        fast_forward_head_unsafe t ?max_depth ?n head
      )

  let merge_head t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    get (uri t) ?query ["merge-head"; H.to_hum head] (module M) >>| fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h); ok ()
    | `Tag _  -> ok ()

  let merge_head_exn t ?max_depth ?n head =
    merge_head t ?max_depth ?n head >>= Irmin.Merge.exn

  let watch_head t key =
    match branch t with
    | `Empty
    | `Head _ -> Lwt_stream.of_list []
    | `Tag _  ->
      let module W = Tc.Pair (P)(Tc.Option(H)) in
      Irmin.Private.Watch.lwt_stream_lift (
        let s = get_stream (uri t) ["watch-head"; P.to_hum key] (module W) in
        return s
      )

  let watch_tags t =
    let module W = Tc.Pair (T)(Tc.Option(H)) in
    Irmin.Private.Watch.lwt_stream_lift (
      let s = get_stream (uri t) ["watch-tags"] (module W) in
      return s
    )

  let clone task t tag =
    get (uri t) ["clone"; T.to_hum tag] Tc.string >>= function
    | "ok" ->
      of_tag t.config task tag >>= fun t ->
      return (`Ok t)
    | _    -> return `Duplicated_tag

  let clone_force task t tag =
    get (uri t) ["clone-force"; T.to_hum tag] Tc.unit >>= fun () ->
    of_tag t.config task tag

  let merge_tag t ?max_depth ?n tag =
    let query = mk_query ?max_depth ?n () in
    get (uri t) ?query ["merge-tag"; T.to_hum tag] (module M) >>| fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h); ok ()
    | `Tag _  -> ok ()

  let merge_tag_exn t ?max_depth ?n tag =
    merge_tag t ?max_depth ?n tag >>= Irmin.Merge.exn

  let merge a ?max_depth ?n t ~into =
    let t = t a and into = into a in
    match branch t with
    | `Tag tag -> merge_tag into ?max_depth ?n tag
    | `Head h  -> merge_head into ?max_depth ?n h
    | `Empty   -> ok ()

  let merge_exn a ?max_depth ?n t ~into =
    merge a ?max_depth ?n t ~into >>= Irmin.Merge.exn

  module LCA = struct
    module HL = Tc.List(H)
    type t = [`Ok of H.t list | `Max_depth_reached | `Too_many_lcas]
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)
    let of_json = function
      | `O [ "ok", j ] -> `Ok (HL.of_json j)
      | `A [`String "max-depth-reached" ] -> `Max_depth_reached
      | `A [`String "too-many-lcas"] -> `Too_many_lcas
      | j -> Ezjsonm.parse_error j "LCA.of_json"
    let to_json _ = failwith "TODO"
    let read _ = failwith "TODO"
    let write _ = failwith "TODO"
    let size_of _ = failwith "TODO"
  end

  let lcas_tag t ?max_depth ?n tag =
    let query = mk_query ?max_depth ?n () in
    get (uri t) ?query ["lcas-tag"; T.to_hum tag] (module LCA)

  let lcas_head t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    get (uri t) ?query ["lcas-head"; H.to_hum head] (module LCA)

  let lcas a ?max_depth ?n t1 t2 =
    match branch (t2 a) with
    | `Tag tag   -> lcas_tag  (t1 a) ?max_depth ?n tag
    | `Head head -> lcas_head (t1 a) ?max_depth ?n head
    | `Empty     -> Lwt.return (`Ok [])

  let task_of_head t head =
    LP.Commit.read_exn t.commit_t head >>= fun commit ->
    Lwt.return (LP.Commit.Val.task commit)

  module E = Tc.Pair (Tc.List(H)) (Tc.List(H))

  type slice = L.slice

  module Slice = L.Private.Slice

  let export ?full ?depth ?(min=[]) ?(max=[]) t =
    let query =
      let full = match full with
        | None   -> []
        | Some x -> ["full", [string_of_bool x]]
      in
      let depth = match depth with
        | None   -> []
        | Some x -> ["depth", [string_of_int x]]
      in
      match full @ depth with [] -> None | l -> Some l
    in
    post (uri t) ?query ["export"] (E.to_json (min, max))
      (module L.Private.Slice)

  module I = Tc.List(T)

  let import t slice =
    post (uri t) ["import"] (Slice.to_json slice) Tc.unit

  let remove_rec t dir =
    get (uri t) ["remove-rec"; P.to_hum dir] (module H) >>= fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h); return_unit
    | `Tag _  -> return_unit

  let list t dir =
    get (uri t) ["list"; P.to_hum dir] (module Tc.List(P))

  module History = Graph.Persistent.Digraph.ConcreteBidirectional(H)
  module G = Tc.Pair (Tc.List (H))(Tc.List (Tc.Pair(H)(H)))
  module Conv = struct
    type t = History.t
    let to_t (vertices, edges) =
      let t = History.empty in
      let t = List.fold_left History.add_vertex t vertices in
      List.fold_left (fun t (x, y) -> History.add_edge t x y) t edges
    let of_t t =
      let vertices = History.fold_vertex (fun v l -> v :: l) t [] in
      let edges = History.fold_edges (fun x y l -> (x, y) :: l) t [] in
      vertices, edges
  end
  module HTC = Tc.Biject (G)(Conv)
  module EO = Tc.Pair (Tc.Option(Tc.List(H))) (Tc.Option(Tc.List(H)))

  let history ?depth ?min ?max t =
    let query =
      let depth = match depth with
        | None   -> []
        | Some x -> ["depth", [string_of_int x]]
      in
      match depth with [] -> None | l -> Some l
    in
   post (uri t) ?query ["history"] (EO.to_json (min, max)) (module HTC)

  module Key = P
  module Val = C
  module Tag = T
  module Head = H
  module Private = struct
    include L.Private
    let config t = t.config
    let contents_t t = t.contents_t
    let node_t t = t.node_t
    let commit_t t = t.commit_t
    let tag_t t = t.tag_t
    let update_node t = t.update_node
    let read_node t = t.read_node
    let mem_node t = t.mem_node
  end
end

module AO (C: Cohttp_lwt.Client) = XAO(C)(struct let suffix = Some "contents" end)
module RW (C: Cohttp_lwt.Client) = XRW(C)(struct let suffix = None end)
