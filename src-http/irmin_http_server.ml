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

open Lwt.Infix
open Irmin.Merge.OP
open Irmin_http_common
open Astring

let error fmt =
  Printf.ksprintf (fun msg ->
      failwith ("error: " ^ msg)
    ) fmt

let string_replace ~pattern subst str =
  let rex = Re_perl.compile_pat pattern in
  Re_pcre.substitute ~rex ~subst str

let src = Logs.Src.create "irmin.http-server" ~doc:"Irmin REST API server"
module Log = (val Logs.src_log src : Logs.LOG)

exception Invalid

type hooks = { update: unit -> unit Lwt.t }
let no_hooks = { update = fun () -> Lwt.return_unit }

let lwt_stream_lift s =
  let (stream: 'a Lwt_stream.t option ref) = ref None in
  let rec get () =
    match !stream with
    | Some s -> Lwt_stream.get s
    | None   ->
      s >>= fun s ->
      stream := Some s;
      get ()
  in
  Lwt_stream.from get

module type S = sig
  type t
  type spec
  val http_spec: ?strict:bool -> ?hooks:hooks -> t -> spec
end

module type DATE = sig
  val pretty: int64 -> string
  (** Pretty print a raw date format. *)
end

module Make (HTTP: Cohttp_lwt.Server) (D: DATE) (S: Irmin.S) = struct

  type spec = HTTP.t

  type request = {
    t: S.t;
    task: Irmin.task;
    ct: ct;
    path: string list;
    params: contents option;
    query: (string * string list) list;
  }

  type 'a callback = request -> 'a

  type dynamic_node = {
    list : string list Lwt.t;
    child: string -> dispatch Lwt.t;
  }

  and dispatch =
    | Fixed  of contents Lwt.t callback
    | Stream of Ezjsonm.value Lwt_stream.t callback
    | SNode  of (string * dispatch) list
    | DNode  of dynamic_node callback
    | Html   of string Lwt.t callback

  module Lock = Irmin.Private.Lock.Make(S.Ref)

  let read_exn file =
    match Irmin_http_static.read file with
    | None   -> error "%s: not found" file
    | Some s -> s

  let respond_error ct e =
    let headers, body = Response.to_body ct (`Error e) in
    Log.err (fun f -> f "server error %s" (Printexc.to_string e));
    HTTP.respond ~headers
      ~status:`Internal_server_error
      ~body ()

  let respond_contents contents =
    let ct = ct_of_contents (Some contents) in
    let headers, body = Response.to_body ct (`Ok contents) in
    HTTP.respond ~headers ~status:`OK ~body ()

  let respond_json body = respond_contents (json body)
  let respond_html body = HTTP.respond_string ~status:`OK ~body ()

  let respond_json_stream stream =
    let irmin_version = Ezjsonm.encode_string Irmin.version in
    let (++) = Lwt_stream.append in
    let elt ?(ends=", ") name j = Ezjsonm.to_string (`O [name, j]) ^ ends in
    let version = elt "version" in
    let result = elt "result" in
    let start = "[ " ^ elt "stream" (`String start_stream) in
    let stop  = elt "stream" ~ends:" ]" (`String stop_stream) in
    let stream =
      (Lwt_stream.of_list [start]) ++
      (Lwt_stream.of_list [version irmin_version]) ++
      (Lwt_stream.map result stream) ++
      (Lwt_stream.of_list [stop])
    in
    let body = Cohttp_lwt_body.of_stream stream in
    HTTP.respond ~headers:json_headers ~status:`OK ~body ()

  let json_of_response request r =
    let str = Ezjsonm.encode_string in
    let list x = `A x in
    match r with
    | Fixed   _
    | Stream _
    | Html _   -> Lwt.return (str "<data>")
    | SNode c  -> Lwt.return (list ((List.map (fun (n, _) -> str n) c)))
    | DNode d  ->
      let dyn = d request in
      dyn.list >>= fun l -> Lwt.return (list (List.map str l))

  let child request c r: dispatch Lwt.t =
    let error () = Lwt.fail (Failure ("Unknown action: " ^ c)) in
    match r with
    | Fixed _
    | Stream _ -> error ()
    | Html _   -> Lwt.return r
    | DNode d  ->
      let dyn = d request in
      dyn.child c
    | SNode l  ->
      try Lwt.return (List.assoc c l)
      with Not_found -> error ()

  let mk0p name = function
    | [] -> ()
    | p  -> error "%s: non-empty path (%s)" name (String.concat ~sep:":" p)

  let mk0b name = function
    | None   -> ()
    | Some _ -> error "%s: non-empty body" name

  let mk0q name = function
    | [] -> ()
    | _  -> error "%s: non-empty query" name

  let mk1p (type s) name (module S: Irmin.Hum.S with type t = s) path =
    match path with
    | [x] -> S.of_hum (Uri.pct_decode x)
    | [] -> error "%s: empty path" name
    | p  -> error "%s: %s is an invalid path" name (String.concat ~sep:":" p)

  let mknp (type s) (module S: Irmin.Hum.S with type t = s) path =
    List.map S.of_hum (List.map Uri.pct_decode path)

  let mk1b name i = function
    | None   -> error "%s: empty body" name
    | Some c -> contents i c

  let mk1bo _name i = function
    | None   -> None
    | Some c -> Some (contents i c)

  let id = let c = ref 0 in fun () -> incr c; !c

  (* global lock manager *)
  let lockm = Lock.create ()

  let with_lock lock t fn =
    match lock with
    | None     -> fn ()
    | Some tag ->
      tag t >>= function
      | None     -> fn ()
      | Some tag ->
        let id = id () in
        Log.debug (fun f -> f "Lock %d taken" id);
        Lock.with_lock lockm tag fn >>= fun r ->
        Log.debug (fun f -> f "Lock %d released" id);
        Lwt.return r

  let run_hooks = function
    | None   -> Lwt.return_unit
    | Some f -> f ()

  let mkct ct o r = match ct with
    | `Json -> json_contents o r
    | `Raw  -> raw_contents o r

  (* no arguments, fixed answer *)
  let mk0p0bf ?lock ?hooks name fn db o =
    name,
    Fixed (fun r ->
        mk0p name r.path;
        mk0b name r.params;
        mk0q name r.query;
        db r.t r.task >>= fun t ->
        with_lock lock t (fun () -> fn t) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  (* no arguments, fixed answer, query *)
  let mk0p0bfq ?lock ?hooks name fn db o =
    name,
    Fixed (fun r ->
        mk0p name r.path;
        mk0b name r.params;
        db r.t r.task >>= fun t ->
        with_lock lock t (fun () -> fn t r.query) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  (* 0 argument, return an html page. *)
  let mk0p0bh name fn db =
    name,
    Html (fun r ->
        mk0b name r.params;
        db r.t r.task >>= fun t ->
        fn t r.path r.query
      )

  (* 0 arguments, return a stream *)
  let mk0p0bs name fn db o =
    name,
    Stream (fun r ->
        mk0p name r.path;
        mk0b name r.params;
        mk0q name r.query;
        lwt_stream_lift
          (db r.t r.task >>= fun t ->
           let stream = fn t in
           let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
           Lwt.return stream)
      )

  (* 1 argument in the body, return a stream *)
  let mk0p1bs name fn db i1 o =
    name,
    Stream (fun r ->
        mk0p name r.path;
        mk0q name r.query;
        let x = mk1bo name i1 r.params in
        lwt_stream_lift
          (db r.t r.task >>= fun t ->
           let stream = fn t x in
           let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
           Lwt.return stream)
      )

  (* 1 argument in the path, fixed answer with locks *)
  let mk1p0bf name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun r ->
        let x = mk1p name i1 r.path in
        mk0b name r.params;
        mk0q name r.query;
        db r.t r.task >>= fun t ->
        with_lock lock (t, x) (fun () -> fn t x) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  (* 1 argument in the path, fixed answer and parameters in the query *)
  let mk1p0bfq name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun r ->
        let x = mk1p name i1 r.path in
        mk0b name r.params;
        db r.t r.task >>= fun t ->
        with_lock lock (t, x, r.query) (fun () -> fn t x r.query) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  (* n argument in the path, fixed answer *)
  let mknp0bf name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun r ->
        let x = mknp i1 r.path in
        mk0b name r.params;
        mk0q name r.query;
        db r.t r.task >>= fun t ->
        with_lock lock (t, x) (fun () -> fn t x) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  (* 1 argument in the body *)
  let mk0p1bf name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun r ->
        mk0p name r.path;
        mk0q name r.query;
        let x = mk1b name i1 r.params in
        db r.t r.task >>= fun t ->
        with_lock lock (t, x) (fun () -> fn t x) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  let mk1p1bf name ?lock fn db i1 i2 o =
    name,
    Fixed (fun r ->
        let x1 = mk1p name i1 r.path in
        let x2 = mk1b name i2 r.params in
        mk0q name r.query;
        db r.t r.task >>= fun t ->
        with_lock lock (t, x1, x2) (fun () -> fn t x1 x2) >|= fun x ->
        mkct r.ct o x
      )

  (* n arguments in the path, 1 argument in the body, fixed answer with locks *)
  let mknp1bf name ?lock ?hooks fn db i1 i2 o =
    name,
    Fixed (fun r ->
        let x1 = mknp i1 r.path in
        let x2 = mk1b name i2 r.params in
        mk0q name r.query;
        db r.t r.task >>= fun t ->
        with_lock lock (t, x1, x2) (fun () -> fn t x1 x2) >>= fun x ->
        run_hooks hooks >|= fun () ->
        mkct r.ct o x
      )

  (* 1 of arguments in the path, 1 body, streamed response *)
  let mk1p1bs name ?lock fn db i1 i2 o =
    name,
    Stream (fun r ->
        let x1 = mk1p name i1 r.path in
        mk0q name r.query;
        let x2 = mk1bo name i2 r.params in
        lwt_stream_lift (with_lock lock (r.t, x1, x2) (fun () ->
            db r.t r.task >>= fun t ->
            let stream = fn t x1 x2 in
            let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
            Lwt.return stream))
      )

  (* n of arguments in the path, 1 body, streamed response *)
  let mknp1bs name ?lock fn db i1 i2 o =
    name,
    Stream (fun r ->
        let x1 = mknp i1 r.path in
        mk0q name r.query;
        let x2 = mk1bo name i2 r.params in
        lwt_stream_lift (with_lock lock (r.t, x1, x2) (fun () ->
            db r.t r.task >>= fun t ->
            let stream = fn t x1 x2 in
            let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
            Lwt.return stream))
      )

  (* Wrapper for functions that only need a repository, not a branch.
   * todo: The REST API should be updated not to require a branch in the first place. *)
  let repo_op fn t =
    fn (S.repo t)

  let graph_index = read_exn "index.html"

  module Dot = Irmin.Dot(S)

  let graph_of_dump dump path query =
    match path with
    | [] -> Lwt.return graph_index
    | ["graph.dot"] ->
      let param n fn =
        try match List.assoc n query with
          | []   -> None
          | x::_ -> Some (fn x)
        with Not_found -> None
      in
      let depth = param "depth" int_of_string in
      let full = param "full" ((<>) "0") in
      let buffer = Buffer.create 1024 in
      Dot.output_buffer dump ~html:true ?depth ?full ~date:D.pretty buffer
      >>= fun () ->
      let str = Buffer.contents buffer in
      (* Fix the OCamlGraph output (XXX: open an issue upstream) *)
      let str = string_replace ~pattern:",[ \\n]*]" (fun _ -> "]") str in
      Lwt.return str
    | [file] -> mk0q "graph" query; Lwt.return (read_exn file)
    | l      -> error "%s: not found" (String.concat ~sep:"/" l)

  let ao_store (type key) (type value) (type l)
      (module M: Irmin.AO with type t = l and type key = key and type value = value)
      (module K: Irmin.Hum.S with type t = M.key)
      (module V: Tc.S0 with type t = M.value)
      (fn: S.t -> l Lwt.t) =
    let key': M.key Irmin.Hum.t = (module K) in
    let key: M.key Tc.t = (module K) in
    let value: M.value Tc.t = (module V) in
    let fn x _ = fn x in
    SNode [
      mk1p0bf "read" M.read fn key' (Tc.option value);
      mk1p0bf "mem" M.mem fn key' Tc.bool;
      mk0p1bf "add" M.add fn value key;
    ]

  let contents_store = ao_store
      (module S.Private.Contents)
      (module S.Private.Contents.Key)
      (module S.Private.Contents.Val)
      (fun t -> Lwt.return (S.Private.Repo.contents_t (S.repo t)))

  let node_store = ao_store
      (module S.Private.Node)
      (module S.Private.Node.Key)
      (module S.Private.Node.Val)
      (fun t -> Lwt.return (S.Private.Repo.node_t (S.repo t)))

  let commit_store = ao_store
      (module S.Private.Commit)
      (module S.Private.Commit.Key)
      (module S.Private.Commit.Val)
      (fun t -> Lwt.return (S.Private.Repo.commit_t (S.repo t)))

  let stream fn t =
    let stream, push = Lwt_stream.create () in
    lwt_stream_lift (
      fn t (fun k ->
          push (Some k);
          Lwt.return_unit
        ) >>= fun () ->
      push None;
      Lwt.return stream
    )

  let tag_store =
    let module T = S.Private.Ref in
    let ref_t t _ = Lwt.return (S.Private.Repo.ref_t (S.repo t)) in
    let lock3 (_, tag, _) = Lwt.return (Some tag) in
    let lock2 (_, tag) = Lwt.return (Some tag) in
    let tag': S.branch_id Irmin.Hum.t = (module S.Ref) in
    let tag: S.branch_id Tc.t = (module S.Ref) in
    let commit_id: S.commit_id Tc.t = (module S.Hash) in
    let t_cs t tag (test, set) = T.compare_and_set t tag ~test ~set in
    let tc_cs = Tc.pair (Tc.option commit_id) (Tc.option commit_id) in
    let t_iter t fn = T.iter t (fun k _ -> fn k) in
    let mk = function
      | `Updated (_, y)
      | `Added y   -> Some y
      | `Removed _ -> None
    in
    let t_watch t init =
      let stream, push = Lwt_stream.create () in
      lwt_stream_lift (
        let close = ref (fun () -> Lwt.return_unit) in
        T.watch t ?init (fun k v ->
            try push (Some (k, mk v)); Lwt.return_unit
            with Lwt_stream.Closed -> !close ())
        >>= fun c ->
        close := (fun () -> T.unwatch t c);
        Lwt.return stream
      )
    in
    let t_watch_key t tag init =
      let stream, push = Lwt_stream.create () in
      lwt_stream_lift (
        let close = ref (fun () -> Lwt.return_unit) in
        T.watch_key t ?init tag (fun v ->
            try push (Some (mk v)); Lwt.return_unit
            with Lwt_stream.Closed -> !close ())
        >>= fun c ->
        close := (fun () -> T.unwatch t c);
        Lwt.return stream
      )
    in
    let tc_ho = Tc.option commit_id in
    let tc_watch_i = Tc.list (Tc.pair tag commit_id) in
    let tc_watch_s = Tc.pair tag (Tc.option commit_id) in
    let tc_watch_k = tc_ho in
    SNode [
      mk1p0bf "read" T.read ref_t tag' tc_ho;
      mk1p0bf "mem" T.mem ref_t tag' Tc.bool;
      mk0p0bs "iter" (stream t_iter) ref_t tag;
      mk1p1bf "update" ~lock:lock3 T.update ref_t tag' commit_id Tc.unit;
      mk1p0bf "remove" ~lock:lock2 T.remove ref_t tag' Tc.unit;
      mk1p1bf "compare-and-set" ~lock:lock3 t_cs ref_t tag' tc_cs Tc.bool;
      mk0p1bs "watch" t_watch ref_t tc_watch_i tc_watch_s;
      mk1p1bs "watch-key" t_watch_key ref_t tag' commit_id tc_watch_k;
    ]

  let list f t list = f t (S.Key.create list)

  let step_h: S.Key.step Irmin.Hum.t = (module S.Key.Step)
  let tag_h: S.branch_id Irmin.Hum.t = (module S.Ref)
  let commit_id_h: S.commit_id Irmin.Hum.t = (module S.Hash)

  let node_t: S.Private.Node.key Tc.t = (module S.Private.Node.Key)
  let commit_id: S.commit_id Tc.t = (module S.Hash)
  let value: S.value Tc.t = (module S.Val)
  let slice: S.slice Tc.t = (module S.Private.Slice)
  let key: S.key Tc.t = (module S.Key)

  module View = struct
    type t = S.commit_id * S.Private.Node.key
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash

    let of_string str =
      match String.cut str ~sep:"-" with
      | None        -> failwith "invalid view"
      | Some (h, n) -> S.Hash.of_hum h, S.Private.Node.Key.of_hum n
    let to_string (h, n) = S.Hash.to_hum h ^ "-" ^ S.Private.Node.Key.to_hum n

    let to_json t = `String (to_string t)

    let of_json = function
      | `String s -> of_string s
      | j -> Ezjsonm.parse_error j "ok_or_duplicated_tag"

    let read buf = of_string (Tc.String.read buf)
    let size_of t = Tc.String.size_of (to_string t)
    let write t = Tc.String.write (to_string t)
  end
  let view: View.t Tc.t = (module View)

  let dyn_node list child =
    DNode (fun request -> { list = list request; child = child request })

  let static_node nodes = Lwt.return (SNode nodes)

  let merge (type x) (x:x Tc.t): x Irmin.Merge.result Tc.t =
    let module X = (val x: Tc.S0 with type t = x) in
    let module M = Tc.App1(Irmin.Merge.Result)(X) in
    (module M)

  let view_store =
    let module Contents = S.Private.Contents in
    let module Node = S.Private.Node in
    let module Graph = Irmin.Private.Node.Graph(Node) in
    let t x _ = Lwt.return x in
    let g x _ =
      let repo = S.repo x in
      let c = S.Private.Repo.contents_t repo in
      let n = S.Private.Repo.node_t repo in
      Lwt.return (c, n)
    in
    dyn_node
      (fun _ -> Lwt.return ["create"])
      (fun _ -> function
         | "create" ->
           let create =
             let aux t path =
               S.head_exn t >>= fun commit_id ->
               S.of_commit_id (fun () -> S.task t) commit_id (S.repo t) >>= fun t ->
               S.Private.read_node (t ()) path >>= function
               | None   -> Lwt.fail_invalid_arg "view"
               | Some n -> Lwt.return (commit_id, n)
             in
             list aux
           in
           static_node [
             mknp0bf "create" create t step_h view;
           ]
         | node ->
           let node = Node.Key.of_hum node in
           let read =
             let aux (c, n) path =
               Graph.read_contents_exn n node path >>= fun (k, _meta) ->
               Contents.read_exn c k
             in
             list aux
           in
           let update =
             let aux (c, n) path value =
               Contents.add c value >>= fun k ->
               Graph.add_contents n node path (k, Node.Val.Metadata.default)
             in
             list aux
           in
           let update_path =
             let aux t path = S.Private.update_node t path node in
             list aux
           in
           let merge_path =
             let aux t path parent =
               S.Private.merge_node t path (parent, node)
             in
             list aux
           in
           let iter t fn = S.Private.iter_node t node (fun k _ -> fn k) in
           static_node [
             mknp0bf "read"        read g step_h value;
             mknp1bf "update"      update g step_h value node_t;
             mk0p0bs "iter"        (stream iter) t key;
             mknp0bf "update-path" update_path t step_h Tc.unit;
             mknp1bf "merge-path"  merge_path t step_h commit_id (merge Tc.unit);
           ]
      )

  let get_query query fn n =
    try match List.assoc n query with
      | [i] -> (try Some (fn i) with Failure _ -> None)
      | _ -> None
    with Not_found ->
      None

  let get_query_all query fn n =
    try List.assoc n query |> List.map fn
    with Not_found -> []

  let mk_merge_query query =
    let max_depth = get_query query int_of_string "depth" in
    let n = get_query query int_of_string "n" in
    max_depth, n

  let mk_export_query query =
    let full = get_query query bool_of_string "full" in
    let depth = get_query query int_of_string "depth" in
    let min = get_query_all query (Tc.read_string (module S.Hash)) "min" in
    let max = get_query_all query (Tc.read_string (module S.Hash)) "max" in
    full, depth, min, max

  let mk_history_query query =
    let depth = get_query query int_of_string "depth" in
    let min = get_query_all query (Tc.read_string (module S.Hash)) "min" in
    let max = get_query_all query (Tc.read_string (module S.Hash)) "max" in
    depth, min, max

  module G = Tc.Pair (Tc.List (S.Hash)) (Tc.List (Tc.Pair (S.Hash)(S.Hash)))
  module Conv = struct
    type t = S.History.t
    let to_t (vertices, edges) =
      let t = S.History.empty in
      let t = List.fold_left S.History.add_vertex t vertices in
      List.fold_left (fun t (x, y) -> S.History.add_edge t x y) t edges
    let of_t t =
      let vertices = S.History.fold_vertex (fun v l -> v :: l) t [] in
      let edges = S.History.fold_edges (fun x y l -> (x, y) :: l) t [] in
      vertices, edges
  end
  module HTC = Tc.Biject (G)(Conv)

  let lca = lca (module S.Hash)

  module V = Irmin.View(S)

  let dispatch hooks =
    let lock3 (t, _, _) = S.name t in
    let lock2 (t, _) = S.name t in
    let s_export t query =
      let full, depth, min, max = mk_export_query query in
      S.Repo.export ?full ?depth ~min ~max (S.repo t)

    in
    let s_history t query =
      let depth, min, max = mk_history_query query in
      S.history ?depth ~min ~max t
    in
    let s_graph t = graph_of_dump t in
    let s_clone t tag =
      S.clone (fun () -> S.task t) t tag >|= function
      | `Ok _ -> `Ok
      | `Duplicated_branch | `Empty_head as x -> x
    in
    let s_clone_force t tag =
      S.clone_force (fun () -> S.task t) t tag >>= fun _ ->
      Lwt.return_unit
    in
    let s_update t k v =
      S.update t k v >>= fun () ->
      S.head_exn t
    in
    let s_remove t k =
      S.remove t k >>= fun () ->
      S.head_exn t
    in
    let s_remove_rec t k =
      S.remove_rec t k >>= fun () ->
      S.head_exn t
    in
    let s_merge_head t k query =
      let max_depth, n = mk_merge_query query in
      S.merge_head t ?max_depth ?n k >>| fun () ->
      S.head_exn t >>=
      ok
    in
    let s_merge_branch t k query =
      let max_depth, n = mk_merge_query query in
      S.merge_branch t ?max_depth ?n k >>| fun () ->
      S.head_exn t >>=
      ok
    in
    let s_watch_rec t key init =
      let init = match init with
        | None   -> Lwt.return_none
        | Some c ->
          S.of_commit_id Irmin.Task.none c (S.repo t) >>= fun t' ->
          V.of_path (t' ()) key >|= fun v ->
          Some (c, v)
      in
      let mk (k, v) =
        (match v with `Updated _ -> "*" | `Added _ -> "+" | `Removed _ -> "-"),
        k
      in
      let stream, push = Lwt_stream.create () in
      lwt_stream_lift (
        let close = ref (fun () -> Lwt.return_unit) in
        init >>= fun init ->
        V.watch_path t ?init key (fun diff ->
            let head = match diff with
              | `Added y | `Updated (_, y) -> Some (fst y)
              | `Removed _ -> None
            in
            match head with
            | None      -> Lwt.return_unit (* skip deleted branch events *)
            | Some head ->
              let x, y = match diff with
                | `Updated (x, y) -> Lwt.return (snd x), Lwt.return (snd y)
                | `Added x        -> V.empty (), Lwt.return (snd x)
                | `Removed x      -> Lwt.return (snd x), V.empty ()
              in
              x >>= fun x -> y >>= fun y ->
              V.diff x y >>= fun diff ->
              let diff = head, List.map mk diff in
              try push (Some diff); Lwt.return_unit
              with Lwt_stream.Closed -> !close ())
        >|= fun f ->
        close := f;
        stream
      ) in
    let s_watch t key init =
      let mk = function
        | `Updated (_, y) | `Added y -> Some y
        | `Removed _ -> None
      in
      let stream, push = Lwt_stream.create () in
      lwt_stream_lift (
        let close = ref (fun () -> Lwt.return_unit) in
        S.watch_key t ?init key (fun v ->
            try push (Some (mk v)); Lwt.return_unit
            with Lwt_stream.Closed -> !close ())
        >|= fun f ->
        close := f;
        stream
      ) in
    let tc_watch_v = Tc.pair commit_id value in
    let tc_watch_k = Tc.pair commit_id Tc.(list @@ pair string key) in
    let s_lcas_branch t tag query =
      let max_depth, n = mk_merge_query query in
      S.lcas_branch t ?max_depth ?n tag
    in
    let s_lcas_head t head query =
      let max_depth, n = mk_merge_query query in
      S.lcas_head t ?max_depth ?n head
    in
    let s_compare_and_set t key (test, set) =
      S.compare_and_set t key ~test ~set
    in
    let s_compare_and_set_head t (test, set) =
      S.compare_and_set_head t ~test ~set
    in
    let s_fast_forward_head t head query =
      let max_depth, n = mk_merge_query query in
      S.fast_forward_head t ?max_depth ?n head
    in
    let s_update_branch t tag = S.update_branch t tag >>= fun () -> S.head_exn t in
    let s_iter t fn = S.iter t (fun k _ -> fn k) in
    let hooks = hooks.update in
    let bc t = [
      (* rw *)
      mknp0bf "read" (list S.read) t step_h (Tc.option value);
      mknp0bf "mem" (list S.mem) t step_h Tc.bool;
      mk0p0bs "iter" (stream s_iter) t key;
      mknp1bf "update" ~lock:lock3 ~hooks (list s_update) t step_h value commit_id;
      mknp0bf "remove" ~lock:lock2 ~hooks (list s_remove) t step_h commit_id;
      mknp1bf "compare-and-set" ~lock:lock3 ~hooks (list s_compare_and_set) t step_h
        (Tc.pair (Tc.option value) (Tc.option value)) Tc.bool;

      (* hrw *)
      mknp0bf "list" (list S.list) t step_h (Tc.list key);
      mknp0bf "remove-rec" ~lock:lock2 (list s_remove_rec) t step_h commit_id;

      (* watches *)
      mknp1bs "watch" (list s_watch) t step_h tc_watch_v (Tc.option tc_watch_v);
      mknp1bs "watch-rec" (list s_watch_rec) t step_h commit_id tc_watch_k;

      (* more *)
      mk1p0bf "remove-tag" ~lock:lock2 ~hooks (repo_op S.Repo.remove_branch)  t tag_h Tc.unit;
      mk1p0bf "update-tag" ~lock:lock2 ~hooks s_update_branch t tag_h commit_id;
      mk1p0bfq "merge-tag" ~lock:lock3 ~hooks s_merge_branch t tag_h (merge commit_id);
      mk0p0bf "head" S.head t (Tc.option commit_id);
      mk0p0bf "heads" (repo_op S.Repo.heads) t (Tc.list commit_id);
      mk1p0bf "update-head" ~lock:lock2 ~hooks S.update_head t commit_id_h Tc.unit;
      mk1p0bfq "fast-forward-head" ~lock:lock3 ~hooks s_fast_forward_head t
        commit_id_h Tc.bool;
      mk0p1bf "compare-and-set-head" ~lock:lock2 ~hooks s_compare_and_set_head t
        (Tc.pair (Tc.option commit_id) (Tc.option commit_id)) Tc.bool;
      mk1p0bfq "merge-head" ~lock:lock3 ~hooks s_merge_head t commit_id_h (merge commit_id);
      mk1p0bf "clone" s_clone t tag_h ok_or_duplicated_branch_id;
      mk1p0bf "clone-force" s_clone_force t tag_h Tc.unit;
      mk0p0bfq "export" s_export t slice;
      mk0p1bf "import" (repo_op S.Repo.import) t slice ok_or_error;
      mk0p0bfq "history" s_history t (module HTC);

      (* lca *)
      mk1p0bfq "lcas-tag" s_lcas_branch  t tag_h lca;
      mk1p0bfq "lcas-head" s_lcas_head t commit_id_h lca;

      (* extra *)
      mk0p0bh "graph" s_graph t;

      (* views *)
      "view", view_store;

    ] in

    SNode (
      bc (fun x task ->
          S.master (fun () -> task) (S.repo x) >>= fun x ->
          Lwt.return (x ()))
      @ [
        (* subdirs *)
        "contents", contents_store;
        "node"    , node_store;
        "commit"  , commit_store;
        "tag"     , tag_store;
      ] @ [
        "empty"   , SNode (bc (fun t task ->
            S.empty (fun () -> task) (S.repo t) >>= fun t ->
            Lwt.return (t ())))
      ] @ [
        "tree"    , dyn_node
          (fun req ->
             let repo = S.repo req.t in
             S.Repo.branches repo  >>= fun branches ->
             S.Repo.heads repo >|= fun heads ->
             List.map S.Ref.to_hum branches @ List.map S.Hash.to_hum heads)
          (fun _req n ->
             let app fn t x task =
               fn (fun () -> task) x (S.repo t) >>= fun t ->
               Lwt.return (t ())
             in
             try
               let n = S.Hash.of_hum n in
               Lwt.return (SNode (bc (fun t -> app S.of_commit_id t n)))
             with Irmin.Hash.Invalid _ ->
               let node = bc (fun t -> app S.of_branch_id t (S.Ref.of_hum n)) in
               Lwt.return (SNode node))
      ])

  let err_bad_version v =
    error "bad client version: expecting %s, but got %s" Irmin.version v

  let err_no_task () = error "the client didn't send any task information."

  let read_body meth ct body = Request.of_body ~meth ct body

  let path uri =
    let path = Uri.path uri in
    let path = String.cuts path ~sep:"/" in
    let path = List.filter ((<>) "") path in
    path

  let process ?(strict=false) t dispatch ~meth ~version ~ct ~uri ~body =
    let () = match version with
      | None   ->
        if strict then err_bad_version "<none>"
        else Log.info (fun f -> f "No Irmin header set, skipping the version check")
      | Some v -> if v <> Irmin.version then err_bad_version v
    in
    let query = Uri.query uri in
    begin match meth with
      | `GET    -> Lwt.return_none
      | `DELETE -> read_body "DELETE" ct body
      | `POST   -> read_body "POST" ct body
      | _       -> Lwt.fail Invalid
    end >>= fun body ->
    let task, params = match meth, body with
      | `GET, None   -> Irmin.Task.empty, None
      | _   , None   -> err_no_task ()    (* Wrong error message? Params missing too! *)
      | _   , Some (Some t, params) -> t, params
      | _   , Some (None, params) -> Irmin.Task.empty, params  (* Should be an error, but reads arrive here too! *)
    in
    (* FIXME: validate the client task *)
    let request path = { t; task; ct; path; params; query } in
    let rec aux dispatch = function
      | []      -> json_of_response (request []) dispatch >>= respond_json
      | h::path ->
        let request = request path in
        let f = function
          | Fixed fn  -> fn request >>= respond_contents
          | Stream fn -> fn request |>  respond_json_stream
          | Html fn   -> fn request >>= respond_html
          | child     -> aux child path
        in
        Lwt.catch
          (fun () -> child request h dispatch >>= f)
          (fun e ->
             let query =
               List.map (fun (k, v) -> k ^ "=" ^ String.concat ~sep:"." v) query
               |> String.concat ~sep:","
             in
             Log.debug (fun f -> f "uri=%s path=%s query=%s error=%s"
               (Uri.to_string uri)
               (String.concat ~sep:"/" path)
               query
               (Printexc.to_string e));
             Lwt.fail e)
    in
    aux dispatch (path uri)

  type t = S.t

  let http_spec ?strict ?(hooks=no_hooks) t =
    let dispatch = dispatch hooks in
    let callback (_, conn_id) req body =
      let uri = Cohttp.Request.uri req in
      let path = Uri.path (Cohttp.Request.uri req) in
      let meth = Cohttp.Request.meth req in
      let headers = Cohttp.Request.headers req in
      let ct = ct_of_header headers in
      let version = Cohttp.Header.get headers irmin_version in
      Log.info (fun f -> f "Connection %s: %s %s (%s)"
        (Cohttp.Connection.to_string conn_id)
        (Cohttp.Code.string_of_method meth)
        path
        (string_of_ct ct));
      Lwt.catch (fun () ->
          try process ?strict t dispatch ~meth ~version ~ct ~uri ~body
          with e -> Lwt.fail e
        ) (fun e -> respond_error ct e)
    in
    let conn_closed (_, conn_id) =
      Log.debug (fun f -> f "Connection %s: closed!" (Cohttp.Connection.to_string conn_id))
    in
    HTTP.make ~callback ~conn_closed ()

end
