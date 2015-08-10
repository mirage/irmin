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

open Irmin.Merge.OP
open Irmin_http_common

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Log = Log.Make(struct let section = "HTTP" end)

(* ~uri *)
let uri =
  Irmin.Private.Conf.key
    ~docv:"URI"
    ~doc:"Location of the remote store."
    "uri" Irmin.Private.Conf.(some uri) None

let config x =
  Irmin.Private.Conf.singleton uri (Some x)

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

let err_no_uri () = invalid_arg "Irmin_http.create: No URI specified"

let get_uri config = match Irmin.Private.Conf.get config uri with
  | None   -> err_no_uri ()
  | Some u -> u

let add_uri_suffix suffix config =
  let v = uri_append (get_uri config) [suffix] in
  Irmin.Private.Conf.add config uri (Some v)

let invalid_arg fmt =
  Printf.ksprintf (fun str -> Lwt.fail (Invalid_argument str)) fmt

let some x = Some x

module Helper (Client: Cohttp_lwt.Client) = struct

  exception Error of string

  let raise_bad_version v =
    let v = match v with None -> "<none>" | Some v -> v in
    let err = Printf.sprintf
        "bad server version: expecting {version: %S}, but got %S"
        Irmin.version v
    in
    raise (Error err)

  let result_of_json ~version json =
    if version then (
      let version =
        try Ezjsonm.find json ["version"] |> Ezjsonm.decode_string
        with Not_found -> None
      in
      if version <> Some Irmin.version then raise_bad_version version;
    );
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
      |> result_of_json ~version:true
      |> M.of_json
      |> Lwt.return
    with Error e ->
      Lwt.fail (Error e)

  let err_empty_stream () = invalid_arg "the stream is empty!"
  let err_bad_start j =
    invalid_arg "bad opening stream: expecting %S, but got %S."
      start_stream (Ezjsonm.to_string (`A [j]))
  let err_bad_version v =
    invalid_arg "bad server version: expecting {\"version\": %S}, but got %S"
      Irmin.version Ezjsonm.(to_string (wrap v))

  let map_stream_response (type t) (module M: Tc.S0 with type t = t) (_, b) =
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let start stream =
      Lwt_stream.get stream >>= function
      | Some (`String s) when s = start_stream -> Lwt.return stream
      | None   -> err_empty_stream ()
      | Some j -> err_bad_start j
    in
    let version stream =
      Lwt_stream.get stream >>= function
      | Some (`O ["version", v]) ->
        if Ezjsonm.decode_string v = Some Irmin.version then Lwt.return stream
        else err_bad_version v
      | None   -> err_empty_stream ()
      | Some j -> err_bad_version j
    in
    start stream   >>= fun stream ->
    version stream >>= fun stream ->
    let stream = Lwt_stream.map (result_of_json ~version:false) stream in
    let stream =
      Lwt_stream.map (fun j ->
        Log.debug "stream: got %s" Ezjsonm.(to_string (wrap j));
        M.of_json j
      ) stream
    in
    Lwt.return stream

  let headers = Cohttp.Header.of_list [
      "Connection", "Keep-Alive";
      irmin_header, Irmin.version;
    ]

  let make_uri t path query =
    let uri = uri_append t path in
    match query with
    | None   -> uri
    | Some q -> Uri.with_query uri q

  let map_get t path ?query fn =
    let uri = make_uri t path query in
    Log.debug "get %s" (Uri.path uri);
    Client.get ~headers uri >>= fn

  let get t path ?query fn =
    map_get t path ?query (map_string_response fn)

  let get_stream t path ?query fn  =
    map_get t path ?query (map_stream_response fn)

  let make_body ?task body =
    let str l = Ezjsonm.to_string (`O l) in
    let str_t = Irmin.Task.to_json in
    let body = match body, task with
      | None  , None   -> None
      | None  , Some t -> Some (str ["task"  , str_t t])
      | Some b, None   -> Some (str ["params", b])
      | Some b, Some t -> Some (str ["task", str_t t; "params", b])
    in
    let short_body = match body with
      | None   -> "<none>"
      | Some b -> if String.length b > 80 then String.sub b 0 80 ^ ".." else b
    in
    let body = match body with
      | None   -> None
      | Some b -> Some (Cohttp_lwt_body.of_string b)
    in
    short_body, body

  let delete t ~task path fn =
    let uri = uri_append t path in
    let short_body, body = make_body ?task None in
    Log.debug "delete %s %s" (Uri.path uri) short_body;
    Client.delete ?body ~headers uri >>= map_string_response fn

  let map_post t ~task path ?query body fn =
    let uri = make_uri t path query in
    let short_body, body = make_body ?task body in
    Log.debug "post %s %s" (Uri.path uri) short_body;
    Client.post ?body ~headers uri >>= fn

  let post t ~task path ?query body fn =
    map_post t ~task path ?query body (map_string_response fn)

  let post_stream t ~task path ?query body fn  =
    map_post t ~task path ?query body (map_stream_response fn)

end

module RO (Client: Cohttp_lwt.Client) (K: Irmin.Hum.S) (V: Tc.S0) = struct

  include Helper (Client)

  type t = { mutable uri: Uri.t; task: Irmin.task; config: Irmin.config; }
  type key = K.t
  type value = V.t

  let get t = get t.uri
  let config t = t.config
  let task t = t.task
  let uri t = t.uri

  let create config task =
    let uri = get_uri config in
    Lwt.return (fun a -> { config; uri; task = task a})

  let read t key = get t ["read"; K.to_hum key] (module Tc.Option(V))

  let mem t key = get t ["mem"; K.to_hum key] Tc.bool

  let err_not_found n k =
    invalid_arg "Irmin_http.%s: %s not found" n (K.to_hum k)

  let read_exn t key =
    read t key >>= function
    | None   -> err_not_found "read" key
    | Some v -> Lwt.return v

  let iter t fn =
    let fn key = fn key (read_exn t key) in
    get_stream t.uri ["iter"] (module K) >>=
    Lwt_stream.iter_p fn

end

module AO (Client: Cohttp_lwt.Client) (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO (Client)(K)(V)

  let post t = post t.uri

  let add t value =
    post t ~task:None ["add"] (some @@ V.to_json value) (module K)

end

module RW (Client: Cohttp_lwt.Client) (K: Irmin.Hum.S) (V: Tc.S0) = struct

  module RO = RO (Client)(K)(V)
  module W  = Irmin.Private.Watch.Make(K)(V)

  type key = RO.key
  type value = RO.value
  type watch = W.watch

  (* cache the stream connections to the server: we open only one
     connection per stream kind. *)
  type cache = { mutable stop: unit -> unit; }

  let empty_cache () = { stop = fun () -> (); }

  type t = { t: RO.t; w: W.t; keys: cache; glob: cache }

  let post t = RO.post (RO.uri t.t)
  let delete t = RO.delete (RO.uri t.t)
  let post_stream t = RO.post_stream (RO.uri t.t)

  let create config task =
    RO.create config task >>= fun t ->
    let w = W.create () in
    let keys = empty_cache () in
    let glob = empty_cache () in
    Lwt.return (fun a -> { t = t a; w; keys; glob })

  let uri t = RO.uri t.t
  let config t = RO.config t.t
  let task t = RO.task t.t
  let read t = RO.read t.t
  let read_exn t = RO.read_exn t.t
  let mem t = RO.mem t.t
  let iter t = RO.iter t.t

  let update t key value =
    post t ~task:None ["update"; K.to_hum key] (some @@ V.to_json value) Tc.unit

  let remove t key = delete ~task:None t ["remove"; K.to_hum key] Tc.unit

  module CS = Tc.Pair(Tc.Option(V))(Tc.Option(V))

  let compare_and_set t key ~test ~set =
    post ~task:None t ["compare-and-set"; K.to_hum key]
      (some @@ CS.to_json (test, set)) Tc.bool

  let nb_keys t = fst (W.stats t.w)
  let nb_glob t = snd (W.stats t.w)

  (* run [t] and returns an handler to stop the task. *)
  let stoppable t =
    let s, u = Lwt.task () in
    Lwt.async (fun () -> Lwt.pick ([s; t ()]));
    function () -> Lwt.wakeup u ()

  let make_body (type t) (module V: Tc.S0 with type t=t) = function
    | None   -> None
    | Some v -> Some (V.to_json v)

  let watch_key t key ?init f =
    let init_stream () =
      if nb_keys t <> 0 then Lwt.return_unit
      else
        let task = Some (task t) in
        let body = make_body (module V) init in
        let v_option = Tc.option (module V) in
        post_stream t ~task ["watch-key"] body v_option >>= fun s ->
        let stop () = Lwt_stream.iter_s (W.notify t.w key) s in
        t.keys.stop <- stoppable stop;
        Lwt.return_unit
    in
    init_stream () >>= fun () ->
    W.watch_key t.w key ?init f

  module WI = Tc.List (Tc.Pair (K) (V))
  module WS = Tc.Pair (K) (Tc.Option (V))

  let watch t ?init f =
    let init_stream () =
      if nb_glob t <> 0 then Lwt.return_unit
      else
        let task = Some (task t) in
        let body = make_body (module WI) init in
        post_stream t ~task ["watch"] body (module WS) >>= fun s ->
        let stop () = Lwt_stream.iter_s (fun (k, v) -> W.notify t.w k v) s in
        t.glob.stop <- stoppable stop;
        Lwt.return_unit
    in
    init_stream () >>= fun () ->
    W.watch t.w ?init f

  let stop x =
    let () = try x.stop () with _e -> () in
    x.stop <- fun () -> ()

  let unwatch t id =
    W.unwatch t.w id >>= fun () ->
    if nb_keys t = 0 then stop t.keys;
    if nb_glob t = 0 then stop t.glob;
    Lwt.return_unit

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
        include AO(Client)(H)(C)
        let create config task = create (add_uri_suffix "contents" config) task
      end)
    module Node = struct
      module Key = H
      module Path = C.Path
      module Val = Irmin.Private.Node.Make(H)(H)(C.Path)
      include AO(Client)(Key)(Val)
      let create config task = create (add_uri_suffix "node" config) task
    end
    module Commit = struct
      module Key = H
      module Val = Irmin.Private.Commit.Make(H)(H)
      include AO(Client)(Key)(Val)
      let create config task = create (add_uri_suffix "commit" config) task
    end
    module Tag = struct
      module Key = T
      module Val = H
      include RW(Client)(Key)(Val)
      let create config task = create (add_uri_suffix "tag" config) task
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

  module Tag = struct
    include T
    let to_hum t = Uri.pct_encode (to_hum t)
    let of_hum t = of_hum (Uri.pct_decode t)
  end

  module Key = struct

    include C.Path

    let to_hum t =
      String.concat "/" (C.Path.map t (fun x -> Uri.pct_encode (Step.to_hum x)))

    let of_hum t =
      List.filter ((<>)"") (Stringext.split t ~on:'/')
      |> List.map (fun x -> Step.of_hum (Uri.pct_decode x))
      |> C.Path.create

  end

  module Head = H
  module Val = C

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
  module L = Low(Client)(Val)(Tag)(Head)
  module LP = L.Private
  module S  = RW(Client)(Key)(Val)

  (* [t.s.uri] always point to the right location:
       - `$uri/` if branch = `Tag T.master
       - `$uri/tree/$tag` if branch = `Tag tag
       - `$uri/tree/$key if key = `Key key *)
  type t = {
    branch: [`Tag of Tag.t | `Head of Head.t | `Empty] ref;
    h: S.t; l: L.t;
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
    let base = S.uri t.h in
    match branch t with
    | `Tag tag ->
      if Tag.equal tag Tag.master then base
      else uri_append base ["tree"; Tag.to_hum tag]
    | `Empty  -> uri_append base ["empty"]
    | `Head h -> uri_append base ["tree"; Head.to_hum h]

  let config t = S.config t.h
  let task t = S.task t.h

  let set_head t = function
    | None   -> t.branch := `Empty; Lwt.return_unit
    | Some h -> t.branch := `Head h; L.update_head t.l h

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
      { l; branch; h; contents_t; node_t; commit_t; tag_t;
        read_node; mem_node; update_node; config;
        lock; }
    in
    Lwt.return fn

  let create config task =
    S.create config task >>= fun h ->
    L.create config task >>= fun l ->
    let branch = ref (`Tag Tag.master) in
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

  let err_not_found n k =
    invalid_arg "Irmin_http.%s: %s not found" n (Key.to_hum k)

  let err_no_head = invalid_arg "Irmin_http.%s: no head"
  let err_not_persistent = invalid_arg "Irmin_http.%s: not a persistent branch"

  let get t = get (uri t)

  let delete t =
    let task = Some (task t) in
    delete (uri t) ~task

  let post t path ?query body =
    let task = Some (task t) in
    post (uri t) ~task path ?query body

  let read t key = get t ["read"; Key.to_hum key] (module Tc.Option(Val))
  let mem t key = get t ["mem"; Key.to_hum key] Tc.bool

  let read_exn t key =
    read t key >>= function
    | None   -> err_not_found "read" key
    | Some v -> Lwt.return v

  (* The server sends a stream of keys *)
  let iter t fn =
    let fn key = fn key (read_exn t key) in
    get_stream (uri t) ["iter"] (module Key) >>=
    Lwt_stream.iter_p fn

  let update t key value =
    post t ["update"; Key.to_hum key] (some @@ Val.to_json value) (module Head)
    >>= fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h)
    | `Tag  _ -> Lwt.return_unit

  let remove t key =
    delete t ["remove"; Key.to_hum key] (module Head) >>= fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h)
    | `Tag _  -> Lwt.return_unit

  module CS = Tc.Pair(Tc.Option(Val))(Tc.Option(Val))

  let compare_and_set t key ~test ~set =
    post t ["compare-and-set"; Key.to_hum key] (some @@ CS.to_json (test, set))
      Tc.bool

  let tag t = match branch t with
    | `Empty
    | `Head _ -> Lwt.return_none
    | `Tag t  -> Lwt.return (Some t)

  let tag_exn t = tag t >>= function
    | None   -> err_not_persistent "tag"
    | Some t -> Lwt.return t

  let tags t = get t ["tags"] (module Tc.List(Tag))

  let head t = match branch t with
    | `Empty  -> Lwt.return_none
    | `Head h -> Lwt.return (Some h)
    | `Tag _  -> get t ["head"] (module Tc.Option(Head))

  let head_exn t =
    head t >>= function
    | None   -> err_no_head "head"
    | Some h -> Lwt.return h

  let update_tag t tag =
    post t ["update-tag"; Tag.to_hum tag] None (module Head) >>= fun h ->
    match branch t with
    | `Head _ | `Empty -> set_head t (Some h)
    | `Tag _ -> Lwt.return_unit

  let remove_tag t tag = delete t ["remove-tag"; Tag.to_hum tag] Tc.unit

  let heads t = get t ["heads"] (module Tc.List(Head))

  let update_head t head = match branch t with
    | `Empty
    | `Head _ -> set_head t (Some head)
    | `Tag _  -> get t ["update-head"; Head.to_hum head] Tc.unit

  module CSH = Tc.Pair(Tc.Option(Head))(Tc.Option(Head))

  let compare_and_set_head_unsafe t ~test ~set =
    let true_ () = true in
    match branch t with
    | `Tag _  ->
      post t ["compare-and-set-head"] (some @@ CSH.to_json (test, set)) Tc.bool
    | `Empty ->
      if None = test then (set_head t set >|= true_) else Lwt.return false
    | `Head h ->
      if Some h = test then (set_head t set >|= true_) else Lwt.return false

  let compare_and_set_head t ~test ~set =
    Lwt_mutex.with_lock t.lock (fun () ->
        compare_and_set_head_unsafe t ~test ~set
      )

  module M = Tc.App1 (Irmin.Merge.Result) (Head)

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
    post t ?query ["fast-forward-head"; Head.to_hum head] None Tc.bool
    >>= fun b ->
    match branch t with
    | `Tag _  -> Lwt.return b
    | `Empty
    | `Head _ ->
      (if b then set_head t (Some head) else Lwt.return_unit) >|= fun () -> b

  let fast_forward_head t ?max_depth ?n head =
    Lwt_mutex.with_lock t.lock (fun () ->
        fast_forward_head_unsafe t ?max_depth ?n head
      )

  let merge_head t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    post t ?query ["merge-head"; Head.to_hum head] None (module M) >>| fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h) >>= ok
    | `Tag _  -> ok ()

  let merge_head_exn t ?max_depth ?n head =
    merge_head t ?max_depth ?n head >>= Irmin.Merge.exn

  let watch_head t = L.watch_head t.l
  let watch_tags t = L.watch_tags t.l

  (* FIXME: duplicated code from Ir_bc.lift *)
  let lift value_of_head fn = function
    | `Removed x -> begin
        value_of_head x >>= function
        | None   -> Lwt.return_unit
        | Some v -> fn @@ `Removed (x, v)
      end
    | `Added x -> begin
        value_of_head x >>= function
        | None   -> Lwt.return_unit
        | Some v -> fn @@ `Added (x, v)
      end
    | `Updated (x, y) ->
      assert (not (Head.equal x y));
      value_of_head x >>= fun vx ->
      value_of_head y >>= fun vy ->
      match vx, vy with
      | None   ,  None   -> Lwt.return_unit
      | Some vx, None    -> fn @@ `Removed (x, vx)
      | None   , Some vy -> fn @@ `Added (y, vy)
      | Some vx, Some vy ->
        if Val.equal vx vy then Lwt.return_unit
        else fn @@ `Updated ( (x, vx), (y, vy) )

  (* FIXME: duplicated code from Ir_bc.lift *)
  let watch_key t key ?init fn =
    let init_head = match init with
      | None        -> None
      | Some (h, _) -> Some h
    in
    let value_of_head h =
      of_head t.config (fun () -> task t) h >>= fun t ->
      read (t ()) key
    in
    watch_head t ?init:init_head (lift value_of_head fn)

  let clone task t tag =
    post t ["clone"; Tag.to_hum tag] None ok_or_duplicated_tag >>= function
    | `Ok -> of_tag t.config task tag >|= fun t -> `Ok t
    | `Duplicated_tag | `Empty_head as x -> Lwt.return x

  let clone_force task t tag =
    post t ["clone-force"; Tag.to_hum tag] None Tc.unit >>= fun () ->
    of_tag t.config task tag

  let merge_tag t ?max_depth ?n tag =
    let query = mk_query ?max_depth ?n () in
    post t ?query ["merge-tag"; Tag.to_hum tag] None (module M) >>| fun h ->
    match branch t with
    | `Empty
    | `Head _ -> set_head t (Some h) >>= ok
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

  let lca = lca (module Head)

  let lcas_tag t ?max_depth ?n tag =
    let query = mk_query ?max_depth ?n () in
    get t ?query ["lcas-tag"; Tag.to_hum tag] lca

  let lcas_head t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    get t ?query ["lcas-head"; Head.to_hum head] lca

  let lcas a ?max_depth ?n t1 t2 =
    match branch (t2 a) with
    | `Tag tag   -> lcas_tag  (t1 a) ?max_depth ?n tag
    | `Head head -> lcas_head (t1 a) ?max_depth ?n head
    | `Empty     -> Lwt.return (`Ok [])

  let task_of_head t head =
    LP.Commit.read_exn t.commit_t head >>= fun commit ->
    Lwt.return (LP.Commit.Val.task commit)

  module E = Tc.Pair (Tc.List(Head)) (Tc.Option(Tc.List(Head)))

  type slice = L.slice

  module Slice = L.Private.Slice

  let export ?full ?depth ?(min=[]) ?max t =
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
    (* FIXME: this should be a GET *)
    post t ?query ["export"] (some @@ E.to_json (min, max))
      (module L.Private.Slice)

  module I = Tc.List(Tag)

  let import t slice =
    post t ["import"] (some @@ Slice.to_json slice) ok_or_error

  let remove_rec t dir =
    delete t ["remove-rec"; Key.to_hum dir] (module Head) >>= fun h ->
    match branch t with
      | `Empty
      | `Head _ -> set_head t (Some h)
      | `Tag _  -> Lwt.return_unit

  let list t dir =
    get t ["list"; Key.to_hum dir] (module Tc.List(Key))

  module History = Graph.Persistent.Digraph.ConcreteBidirectional(Head)
  module G = Tc.Pair (Tc.List (Head))(Tc.List (Tc.Pair(Head)(Head)))
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
  module EO = Tc.Pair (Tc.Option(Tc.List(Head))) (Tc.Option(Tc.List(Head)))

  let history ?depth ?min ?max t =
    let query =
      let depth = match depth with
        | None   -> []
        | Some x -> ["depth", [string_of_int x]]
      in
      match depth with [] -> None | l -> Some l
    in
    (* FIXME: this should be a GET *)
    post t ?query ["history"] (some @@ EO.to_json (min, max)) (module HTC)

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
