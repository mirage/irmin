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

let content_type =
  Irmin.Private.Conf.key
    ~docv:"content-type"
    ~doc:"Set the HTTP client content-type (supported type are `raw` and \
          `json`)."
    "content-type" Irmin.Private.Conf.(some string) None

module Conf = Irmin.Private.Conf

let config ?(config=Irmin.Private.Conf.empty) ?content_type:y x =
  let y = match y with
    | None   -> None
    | Some s -> Some (string_of_ct s)
  in
  Conf.add config uri (Some x)
  |> fun config -> Conf.add config content_type y

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

let get_uri config =
  match Conf.get config uri with
  | None   -> err_no_uri ()
  | Some u -> u

let get_ct config =
  match Conf.get config content_type with
  | None   -> `Json
  | Some s -> match ct_of_string s with None -> `Raw | Some t -> t

let add_uri_suffix suffix config =
  let v = uri_append (get_uri config) [suffix] in
  Irmin.Private.Conf.add config uri (Some v)

let invalid_arg fmt =
  Printf.ksprintf (fun str -> Lwt.fail (Invalid_argument str)) fmt

module Helper (Client: Cohttp_lwt.Client) = struct

  let ct_of_response r = ct_of_header (Cohttp.Response.headers r)

  let map_string_response fn (r, b) =
    let ct = ct_of_response r in
    Response.of_body ct b >|= function
    | `Error exn -> raise exn
    | `Ok c      -> contents fn c

  let err_empty_stream () = invalid_arg "the stream is empty!"

  let err_bad_start j =
    invalid_arg "bad opening stream: expecting %S, but got %S."
      start_stream (Ezjsonm.to_string (`A [j]))

  let err_bad_version v =
    invalid_arg "bad server version: expecting {\"version\": %S}, but got %S"
      Irmin.version Ezjsonm.(to_string (wrap v))

  let map_stream_response fn (r, b) =
    let ct = ct_of_response r in
    if ct = `Raw then failwith "raw stream: TODO";
    let stream = Cohttp_lwt_body.to_stream b in
    let stream = Ezjsonm_lwt.from_stream stream in
    let start stream =
      Lwt_stream.get stream >>= function
      | Some (`O ["stream", `String s]) when s = start_stream -> Lwt.return stream
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
    let stream =
      let aux = function
        | `O ["stream", `String s] when s = stop_stream -> None
        | j ->
          match Response.of_json ~version:false j with
          | `Error e -> raise e
          | `Ok c    -> Some (contents fn c)
      in
      Lwt_stream.filter_map aux stream
    in
    Lwt.return stream

  let headers ct = Cohttp.Header.of_list [
      "Connection"       , "Keep-Alive";
      irmin_version      , Irmin.version;
      content_type_header, header_of_ct ct;
    ]

  let make_uri t path query =
    let uri = uri_append t path in
    match query with
    | None   -> uri
    | Some q -> Uri.with_query uri q

  let retry path ?(n=5) f =
    let rec aux n =
      if n <= 0 then f ()
      else
        Lwt.catch f (fun e ->
            Log.debug "Got %s while getting %s, retrying"
              (Printexc.to_string e) path;
            aux (n-1)
          )
    in aux n

  let map_get t ~ct path ?query fn =
    let uri = make_uri t path query in
    let headers = headers ct in
    Log.debug "get %s (%s)" (Uri.path uri) (string_of_ct ct);
    retry (Uri.path uri) (fun () -> Client.get ~headers uri >>= fn)

  let get t ~ct path ?query fn =
    map_get t ~ct path ?query (map_string_response fn)

  let get_stream t path ?query fn  =
    map_get t path ?query (map_stream_response fn)

  let make_body ct ?task body = Some (Request.to_body ct (task, body))

  let delete t ~ct ?task path fn =
    let uri = uri_append t path in
    let body = make_body ct ?task None in
    let headers = headers ct in
    Log.debug "delete %s" (Uri.path uri);
    retry (Uri.path uri) (fun () ->
        Client.delete ?body ~headers uri >>= map_string_response fn
      )

  let body_of_contents ct = function
    | None        -> None
    | Some (m, t) ->
      match ct with
      | `Json -> Some (json_contents m t)
      | `Raw  -> Some (raw_contents m t)

  let map_post t ~ct ?task path ?query ?body fn =
    let uri = make_uri t path query in
    let headers = headers ct in
    let body = make_body ct ?task (body_of_contents ct body) in
    Log.debug "post %s" (Uri.path uri);
    retry (Uri.path uri) (fun () -> Client.post ?body ~headers uri >>= fn)

  let post t ~ct ?task path ?query ?body fn =
    map_post t ~ct ?task path ?query ?body (map_string_response fn)

  let post_stream t ~ct ?task path ?query ?body fn  =
    map_post t ~ct ?task path ?query ?body (map_stream_response fn)

end

module RO (Client: Cohttp_lwt.Client) (K: Irmin.Hum.S) (V: Tc.S0) = struct

  include Helper (Client)

  type t = {
    mutable uri: Uri.t; ct: ct;
  }

  type key = K.t
  type value = V.t

  let uri t = t.uri
  let ct t = t.ct

  let get t = get t.uri ~ct:t.ct
  let get_stream t = get_stream t.uri ~ct:`Json

  let create config =
    let uri = get_uri config in
    let ct  = get_ct config in
    Lwt.return { uri; ct; }

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
    get_stream t ["iter"] (module K) >>= Lwt_stream.iter_p fn

end

module AO (Client: Cohttp_lwt.Client) (K: Irmin.Hash.S) (V: Tc.S0) = struct
  include RO (Client)(K)(V)
  let v: V.t Tc.t = (module V)
  let post t = post t.uri ~ct:t.ct ?task:None
  let add t value = post t ["add"] ~body:(v, value) (module K)
end

module RW (Client: Cohttp_lwt.Client) (K: Irmin.Hum.S) (V: Tc.S0) = struct

  module RO = RO (Client)(K)(V)
  module W  = Irmin.Private.Watch.Make(K)(V)

  let v: V.t Tc.t = (module V)
  type key = RO.key
  type value = RO.value
  type watch = W.watch

  (* cache the stream connections to the server: we open only one
     connection per stream kind. *)
  type cache = { mutable stop: unit -> unit; }

  let empty_cache () = { stop = fun () -> (); }

  type t = { t: RO.t; w: W.t; keys: cache; glob: cache }

  let post ?task t = RO.post (RO.uri t.t) ~ct:(RO.ct t.t) ?task
  let delete ?task t = RO.delete (RO.uri t.t) ~ct:(RO.ct t.t) ?task
  let post_stream ?task t =
    RO.post_stream (RO.uri t.t) ~ct:(RO.ct t.t) ?task

  let create config =
    RO.create config >>= fun t ->
    let w = W.create () in
    let keys = empty_cache () in
    let glob = empty_cache () in
    Lwt.return { t; w; keys; glob }

  let uri t = RO.uri t.t
  let read t = RO.read t.t
  let read_exn t = RO.read_exn t.t
  let mem t = RO.mem t.t
  let iter t = RO.iter t.t

  let update t key value =
    post t ["update"; K.to_hum key] ~body:(v, value) Tc.unit

  let remove t key = delete t ["remove"; K.to_hum key] Tc.unit

  module CS = Tc.Pair(Tc.Option(V))(Tc.Option(V))
  let cs: CS.t Tc.t = (module CS)

  let compare_and_set t key ~test ~set =
    post t ["compare-and-set"; K.to_hum key] ~body:(cs, (test, set)) Tc.bool

  let nb_keys t = fst (W.stats t.w)
  let nb_glob t = snd (W.stats t.w)

  (* run [t] and returns an handler to stop the task. *)
  let stoppable t =
    let s, u = Lwt.task () in
    Lwt.async (fun () -> Lwt.pick ([s; t ()]));
    function () -> Lwt.wakeup u ()

  let make_stream_body tc = function
    | None   -> None
    | Some v -> Some (tc, v)

  let watch_key t key ?init f =
    let init_stream () =
      if nb_keys t <> 0 then Lwt.return_unit
      else
        let body = make_stream_body v init in
        post_stream t ["watch-key"] ?body (Tc.option v) >>= fun s ->
        let stop () = Lwt_stream.iter_s (W.notify t.w key) s in
        t.keys.stop <- stoppable stop;
        Lwt.return_unit
    in
    init_stream () >>= fun () ->
    W.watch_key t.w key ?init f

  module WI = Tc.List (Tc.Pair (K) (V))
  module WS = Tc.Pair (K) (Tc.Option (V))

  let wi: WI.t Tc.t = (module WI)
  let ws: WS.t Tc.t = (module WS)

  let watch t ?init f =
    let init_stream () =
      if nb_glob t <> 0 then Lwt.return_unit
      else
        let body = make_stream_body wi init in
        post_stream t ["watch"] ?body ws >>= fun s ->
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
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S) =
struct
  module X = struct
    module Contents =
      Irmin.Contents.Make(struct
        module Key = H
        module Val = C
        include AO(Client)(H)(C)
        let create config =
          let config = Conf.add config content_type (Some "json") in
          create (add_uri_suffix "contents" config)
      end)
    module Node = struct
      module Key = H
      module Path = C.Path
      module Val = Irmin.Private.Node.Make(H)(H)(C.Path)
      include AO(Client)(Key)(Val)
      let create config =
        let config = Conf.add config content_type (Some "json") in
        create (add_uri_suffix "node" config)
    end
    module Commit = struct
      module Key = H
      module Val = Irmin.Private.Commit.Make(H)(H)
      include AO(Client)(Key)(Val)
      let create config =
        let config = Conf.add config content_type (Some "json") in
        create (add_uri_suffix "commit" config)
    end
    module Ref = struct
      module Key = R
      module Val = H
      include RW(Client)(Key)(Val)
      let create config = create (add_uri_suffix "tag" config)
    end
    module Slice = Irmin.Private.Slice.Make(Contents)(Node)(Commit)
    module Sync = Irmin.Private.Sync.None(H)(R)
  end
  include Irmin.Make_ext(X)
end

module Make (Client: Cohttp_lwt.Client)
    (C: Irmin.Contents.S)
    (R: Irmin.Ref.S)
    (H: Irmin.Hash.S) =
struct

  module Ref = struct
    include R
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
  module L = Low(Client)(Val)(Ref)(Head)
  module LP = L.Private
  module S  = RW(Client)(Key)(Val)

  module Repo = struct
    type t = {
      config : Irmin.config;
      h : S.t;
      l : L.Repo.t;
    }
    let create config =
      S.create config >>= fun h ->
      L.Repo.create config >>= fun l ->
      Lwt.return {config; h; l}

    let config t = t.config
  end

  (* [t.s.uri] always point to the right location:
       - `$uri/` if head_ref = `Branch R.master
       - `$uri/tree/$tag` if head_ref = `Branch tag
       - `$uri/tree/$key if key = `Key key *)
  type t = {
    head_ref: [`Branch of Ref.t | `Head of Head.t | `Empty] ref;
    l: L.t;
    repo: Repo.t;
    contents_t: LP.Contents.t;
    node_t: LP.Node.t;
    commit_t: LP.Commit.t;
    ref_t: LP.Ref.t;
    read_node: L.key -> LP.Node.key option Lwt.t;
    mem_node: L.key -> bool Lwt.t;
    update_node: L.key -> LP.Node.key -> unit Lwt.t;
    merge_node: L.key -> (L.head * LP.Node.key) -> unit Irmin.Merge.result Lwt.t;
    remove_node: L.key -> unit Lwt.t;
    iter_node: LP.Node.key -> (L.key -> L.value Lwt.t -> unit Lwt.t) -> unit Lwt.t;
    lock: Lwt_mutex.t;
  }

  let head_ref t = !(t.head_ref)

  let uri t =
    let base = S.uri t.repo.Repo.h in
    match head_ref t with
    | `Branch name ->
      if Ref.equal name Ref.master then base
      else uri_append base ["tree"; Ref.to_hum name]
    | `Empty  -> uri_append base ["empty"]
    | `Head h -> uri_append base ["tree"; Head.to_hum h]

  let repo t = t.repo
  let task t = L.task t.l
  let ct t = t.repo.Repo.h.S.t.S.RO.ct (* yiikes *)

  let set_head t = function
    | None   -> t.head_ref := `Empty; Lwt.return_unit
    | Some h -> t.head_ref := `Head h; L.update_head t.l h

  type key = S.key
  type value = S.value
  type head = L.head
  type branch_id = L.branch_id

  let v: Val.t Tc.t = (module Val)
  let head_tc: Head.t Tc.t = (module Head)

  let create_aux head_ref repo l =
    let fn a =
      let l = l a in
      let contents_t = LP.contents_t l in
      let node_t = LP.node_t l in
      let commit_t = LP.commit_t l in
      let ref_t = LP.ref_t l in
      let read_node = LP.read_node l in
      let mem_node = LP.mem_node l in
      let update_node = LP.update_node l in
      let remove_node = LP.remove_node l in
      let merge_node = LP.merge_node l in
      let iter_node = LP.iter_node l in
      let lock = Lwt_mutex.create () in
      { l; head_ref; contents_t; node_t; commit_t; ref_t;
        read_node; mem_node; update_node; remove_node; merge_node;
        repo; lock; iter_node; }
    in
    Lwt.return fn

  let master task repo =
    L.master task repo.Repo.l >>= fun l ->
    let head_ref = ref (`Branch Ref.master) in
    create_aux head_ref repo l

  let of_branch_id task branch_id repo =
    L.of_branch_id task branch_id repo.Repo.l >>= fun l ->
    let head_ref = ref (`Branch branch_id) in
    create_aux head_ref repo l

  let of_head task head repo =
    L.of_head task head repo.Repo.l >>= fun l ->
    let head_ref = ref (`Head head) in
    create_aux head_ref repo l

  let empty task repo =
    L.empty task repo.Repo.l >>= fun l ->
    let head_ref = ref `Empty in
    create_aux head_ref repo l

  let err_not_found n k =
    invalid_arg "Irmin_http.%s: %s not found" n (Key.to_hum k)

  let err_no_head = invalid_arg "Irmin_http.%s: no head"
  let err_not_persistent = invalid_arg "Irmin_http.%s: not a persistent branch"

  let get_json ?query t = get ?query (uri t) ~ct:`Json
  let get ?query t = get ?query (uri t) ~ct:(ct t)
  let delete t = delete (uri t) ~ct:(ct t) ~task:(task t)
  let get_stream t = get_stream (uri t) ~ct:(ct t)
  let post_json t = post (uri t) ~ct:`Json ~task:(task t)
  let post t = post (uri t) ~ct:(ct t) ~task:(task t)

  let read t key = get t ["read"; Key.to_hum key] (module Tc.Option(Val))
  let mem t key = get t ["mem"; Key.to_hum key] Tc.bool

  let read_exn t key =
    read t key >>= function
    | None   -> err_not_found "read" key
    | Some v -> Lwt.return v

  (* The server sends a stream of keys *)
  let iter t fn =
    let fn key = fn key (read_exn t key) in
    get_stream t ["iter"] (module Key) >>= Lwt_stream.iter_p fn

  let update t key value =
    post t ["update"; Key.to_hum key] ~body:(v, value) head_tc >>= fun h ->
    match head_ref t with
    | `Empty
    | `Head _ -> set_head t (Some h)
    | `Branch  _ -> Lwt.return_unit

  let remove t key =
    delete t ["remove"; Key.to_hum key] (module Head) >>= fun h ->
    match head_ref t with
    | `Empty
    | `Head _ -> set_head t (Some h)
    | `Branch _  -> Lwt.return_unit

  module CS = Tc.Pair(Tc.Option(Val))(Tc.Option(Val))
  let cs: CS.t Tc.t = (module CS)

  let compare_and_set t key ~test ~set =
    post t ["compare-and-set"; Key.to_hum key] ~body:(cs, (test, set)) Tc.bool

  let name t = match head_ref t with
    | `Empty
    | `Head _ -> Lwt.return_none
    | `Branch t  -> Lwt.return (Some t)

  let name_exn t = name t >>= function
    | None   -> err_not_persistent "name_exn"
    | Some t -> Lwt.return t

  let branches t = get t ["tags"] (module Tc.List(Ref))

  let head t = match head_ref t with
    | `Empty  -> Lwt.return_none
    | `Head h -> Lwt.return (Some h)
    | `Branch _  -> get t ["head"] (module Tc.Option(Head))

  let head_exn t =
    head t >>= function
    | None   -> err_no_head "head"
    | Some h -> Lwt.return h

  let update_branch t branch_id =
    post t ["update-tag"; Ref.to_hum branch_id] head_tc >>= fun h ->
    match head_ref t with
    | `Head _ | `Empty -> set_head t (Some h)
    | `Branch _ -> Lwt.return_unit

  let remove_branch t branch_id = delete t ["remove-tag"; Ref.to_hum branch_id] Tc.unit

  let heads t = get t ["heads"] (module Tc.List(Head))

  let update_head t head = match head_ref t with
    | `Empty
    | `Head _ -> set_head t (Some head)
    | `Branch _  -> get t ["update-head"; Head.to_hum head] Tc.unit

  module CSH = Tc.Pair(Tc.Option(Head))(Tc.Option(Head))
  let csh: CSH.t Tc.t = (module CSH)

  let compare_and_set_head_unsafe t ~test ~set =
    let true_ () = true in
    match head_ref t with
    | `Branch _  ->
      post t ["compare-and-set-head"] ~body:(csh, (test, set)) Tc.bool
    | `Empty ->
      if None = test then (set_head t set >|= true_) else Lwt.return false
    | `Head h ->
      if Some h = test then (set_head t set >|= true_) else Lwt.return false

  let compare_and_set_head t ~test ~set =
    Lwt_mutex.with_lock t.lock (fun () ->
        compare_and_set_head_unsafe t ~test ~set
      )

  module M = Tc.App1 (Irmin.Merge.Result) (Head)
  let m: M.t Tc.t = (module M)

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
    post t ?query ["fast-forward-head"; Head.to_hum head] Tc.bool >>= fun b ->
    match head_ref t with
    | `Branch _  -> Lwt.return b
    | `Empty
    | `Head _ ->
      (if b then set_head t (Some head) else Lwt.return_unit) >|= fun () -> b

  let fast_forward_head t ?max_depth ?n head =
    Lwt_mutex.with_lock t.lock (fun () ->
        fast_forward_head_unsafe t ?max_depth ?n head
      )

  let merge_head t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    post t ?query ["merge-head"; Head.to_hum head] m >>| fun h ->
    match head_ref t with
    | `Empty
    | `Head _ -> set_head t (Some h) >>= ok
    | `Branch _  -> ok ()

  let merge_head_exn t ?max_depth ?n head =
    merge_head t ?max_depth ?n head >>= Irmin.Merge.exn

  let watch_head t = L.watch_head t.l
  let watch_branches t = L.watch_branches t.l

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
      of_head (fun () -> task t) h t.repo >>= fun t ->
      read (t ()) key
    in
    watch_head t ?init:init_head (lift value_of_head fn)

  let clone task t branch_id =
    post t ["clone"; Ref.to_hum branch_id] ok_or_duplicated_branch_id >>= function
    | `Ok -> of_branch_id task branch_id t.repo >|= fun t -> `Ok t
    | `Duplicated_branch | `Empty_head as x -> Lwt.return x

  let clone_force task t branch_id =
    post t ["clone-force"; Ref.to_hum branch_id] Tc.unit >>= fun () ->
    of_branch_id task branch_id t.repo

  let merge_branch t ?max_depth ?n other =
    let query = mk_query ?max_depth ?n () in
    post t ?query ["merge-tag"; Ref.to_hum other] m >>| fun h ->
    match head_ref t with
    | `Empty
    | `Head _ -> set_head t (Some h) >>= ok
    | `Branch _  -> ok ()

  let merge_branch_exn t ?max_depth ?n other =
    merge_branch t ?max_depth ?n other >>= Irmin.Merge.exn

  let merge a ?max_depth ?n t ~into =
    let t = t a and into = into a in
    match head_ref t with
    | `Branch branch_id -> merge_branch into ?max_depth ?n branch_id
    | `Head h  -> merge_head into ?max_depth ?n h
    | `Empty   -> ok ()

  let merge_exn a ?max_depth ?n t ~into =
    merge a ?max_depth ?n t ~into >>= Irmin.Merge.exn

  let lca = lca (module Head)

  let lcas_branch t ?max_depth ?n other =
    let query = mk_query ?max_depth ?n () in
    get t ?query ["lcas-tag"; Ref.to_hum other] lca

  let lcas_head t ?max_depth ?n head =
    let query = mk_query ?max_depth ?n () in
    get t ?query ["lcas-head"; Head.to_hum head] lca

  let lcas a ?max_depth ?n t1 t2 =
    match head_ref (t2 a) with
    | `Branch name   -> lcas_branch  (t1 a) ?max_depth ?n name
    | `Head head -> lcas_head (t1 a) ?max_depth ?n head
    | `Empty     -> Lwt.return (`Ok [])

  let task_of_head t head =
    LP.Commit.read_exn t.commit_t head >>= fun commit ->
    Lwt.return (LP.Commit.Val.task commit)

  module E = Tc.Pair (Tc.List(Head)) (Tc.Option(Tc.List(Head)))

  type slice = L.slice

  module Slice = L.Private.Slice
  let slice_tc: Slice.t Tc.t = (module Slice)

  let head_query name = function
    | [] -> []
    | l  -> [name, List.map (Tc.write_string (module Head)) l]

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
      match full @ depth @ head_query "min" min @ head_query "max" max with
      | [] -> None
      | l  -> Some l
    in
    get_json t ?query ["export"] (module L.Private.Slice)

  module I = Tc.List(Ref)

  let import t slice =
    post_json t ["import"] ~body:(slice_tc, slice) ok_or_error

  let remove_rec t dir =
    delete t ["remove-rec"; Key.to_hum dir] (module Head) >>= fun h ->
    match head_ref t with
      | `Empty
      | `Head _ -> set_head t (Some h)
      | `Branch _  -> Lwt.return_unit

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

  let history ?depth ?(min=[]) ?(max=[]) t =
    let query =
      let depth = match depth with
        | None   -> []
        | Some x -> ["depth", [string_of_int x]]
      in
      match depth @ head_query "min" min @ head_query "max" max with
      | [] -> None
      | l  -> Some l
    in
    get t ?query ["history"] (module HTC)

  module Private = struct
    include L.Private
    module Repo = Repo
    let repo t = t.repo
    let contents_t t = t.contents_t
    let node_t t = t.node_t
    let commit_t t = t.commit_t
    let ref_t t = t.ref_t
    let update_node t = t.update_node
    let merge_node t = t.merge_node
    let remove_node t = t.remove_node
    let read_node t = t.read_node
    let mem_node t = t.mem_node
    let iter_node t = t.iter_node
  end
end
