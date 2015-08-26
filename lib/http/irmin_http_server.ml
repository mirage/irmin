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

let error fmt =
  Printf.ksprintf (fun msg ->
      failwith ("error: " ^ msg)
    ) fmt

let string_replace ~pattern subst str =
  let rex = Re_perl.compile_pat pattern in
  Re_pcre.substitute ~rex ~subst str

module Log = Log.Make(struct let section = "HTTP.server" end)

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
  val listen:
    ?timeout:int -> ?strict:bool -> ?hooks:hooks -> t -> Uri.t -> unit Lwt.t
end

module type SERVER = sig
  include Cohttp_lwt.Server
  val listen: t -> ?timeout:int -> Uri.t -> unit Lwt.t
end

module type DATE = sig
  val pretty: int64 -> string
  (** Pretty print a raw date format. *)
end

module Make (HTTP: SERVER) (D: DATE) (S: Irmin.S) = struct

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
    list : S.t -> string list Lwt.t;
    child: S.t -> string -> dispatch Lwt.t;
  }

  and dispatch =
    | Fixed  of contents Lwt.t callback
    | Stream of Ezjsonm.value Lwt_stream.t callback
    | SNode  of (string * dispatch) list
    | DNode  of dynamic_node
    | Html   of string Lwt.t callback

  module Lock = Irmin.Private.Lock.Make(S.Tag)

  let read_exn file =
    match Irmin_http_static.read file with
    | None   -> error "%s: not found" file
    | Some s -> s

  let respond_error ct e =
    let headers, body = Response.to_body ct (`Error e) in
    Log.error "server error %s" (Printexc.to_string e);
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

  let json_of_response t r =
    let str = Ezjsonm.encode_string in
    let list x = `A x in
    match r with
    | Fixed   _
    | Stream _
    | Html _   -> Lwt.return (str "<data>")
    | SNode c  -> Lwt.return (list ((List.map (fun (n, _) -> str n) c)))
    | DNode d  -> d.list t >>= fun l -> Lwt.return (list (List.map str l))

  let child t c r: dispatch Lwt.t =
    let error () = Lwt.fail (Failure ("Unknown action: " ^ c)) in
    match r with
    | Fixed _
    | Stream _ -> error ()
    | Html _   -> Lwt.return r
    | DNode d  -> d.child t c
    | SNode l  ->
      try Lwt.return (List.assoc c l)
      with Not_found -> error ()

  let mk0p name = function
    | [] -> ()
    | p  -> error "%s: non-empty path (%s)" name (String.concat ":" p)

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
    | p  -> error "%s: %s is an invalid path" name (String.concat ":" p)

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
        Log.debug "Lock %d taken" id;
        Lock.with_lock lockm tag fn >>= fun r ->
        Log.debug "Lock %d released" id;
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
    | l      -> error "%s: not found" (String.concat "/" l)

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
      (fun t -> Lwt.return (S.Private.contents_t t))

  let node_store = ao_store
      (module S.Private.Node)
      (module S.Private.Node.Key)
      (module S.Private.Node.Val)
      (fun t -> Lwt.return (S.Private.node_t t))

  let commit_store = ao_store
      (module S.Private.Commit)
      (module S.Private.Commit.Key)
      (module S.Private.Commit.Val)
      (fun t -> Lwt.return (S.Private.commit_t t))

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
    let module T = S.Private.Tag in
    let tag_t t _ = Lwt.return (S.Private.tag_t t) in
    let lock3 (_, tag, _) = Lwt.return (Some tag) in
    let lock2 (_, tag) = Lwt.return (Some tag) in
    let tag': S.tag Irmin.Hum.t = (module S.Tag) in
    let tag: S.tag Tc.t = (module S.Tag) in
    let head: S.head Tc.t = (module S.Head) in
    let t_cs t tag (test, set) = T.compare_and_set t tag ~test ~set in
    let tc_cs = Tc.pair (Tc.option head) (Tc.option head) in
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
    let tc_ho = Tc.option head in
    let tc_watch_i = Tc.list (Tc.pair tag head) in
    let tc_watch_s = Tc.pair tag (Tc.option head) in
    let tc_watch_k = tc_ho in
    SNode [
      mk1p0bf "read" T.read tag_t tag' tc_ho;
      mk1p0bf "mem" T.mem tag_t tag' Tc.bool;
      mk0p0bs "iter" (stream t_iter) tag_t tag;
      mk1p1bf "update" ~lock:lock3 T.update tag_t tag' head Tc.unit;
      mk1p0bf "remove" ~lock:lock2 T.remove tag_t tag' Tc.unit;
      mk1p1bf "compare-and-set" ~lock:lock3 t_cs tag_t tag' tc_cs Tc.bool;
      mk0p1bs "watch" t_watch tag_t tc_watch_i tc_watch_s;
      mk1p1bs "watch-key" ~lock:lock3 t_watch_key tag_t tag' head tc_watch_k;
    ]

  let key: S.key Tc.t = (module S.Key)
  let head: S.head Tc.t = (module S.Head)

  let merge (type x) (x:x Tc.t): x Irmin.Merge.result Tc.t =
    let module X = (val x: Tc.S0 with type t = x) in
    let module M = Tc.App1(Irmin.Merge.Result)(X) in
    (module M)

  let dyn_node list child = DNode { list; child }

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
    let min = get_query_all query (Tc.read_string (module S.Head)) "min" in
    let max = get_query_all query (Tc.read_string (module S.Head)) "max" in
    full, depth, min, max

  let mk_history_query query =
    let depth = get_query query int_of_string "depth" in
    let min = get_query_all query (Tc.read_string (module S.Head)) "min" in
    let max = get_query_all query (Tc.read_string (module S.Head)) "max" in
    depth, min, max

  module G = Tc.Pair (Tc.List (S.Head)) (Tc.List (Tc.Pair (S.Head)(S.Head)))
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

  let lca = lca (module S.Head)

  let dispatch hooks =
    let step': S.Key.step Irmin.Hum.t = (module S.Key.Step) in
    let tag': S.tag Irmin.Hum.t = (module S.Tag) in
    let lock3 (t, _, _) = S.tag t in
    let lock2 (t, _) = S.tag t in
    let head': S.head Irmin.Hum.t = (module S.Head) in
    let value: S.value Tc.t = (module S.Val) in
    let slice: S.slice Tc.t = (module S.Private.Slice) in
    let s_export t query =
      let full, depth, min, max = mk_export_query query in
      S.export ?full ?depth ~min ~max t

    in
    let s_history t query =
      let depth, min, max = mk_history_query query in
      S.history ?depth ~min ~max t
    in
    let s_graph t = graph_of_dump t in
    let s_clone t tag =
      S.clone (fun () -> S.task t) t tag >|= function
      | `Ok _ -> `Ok
      | `Duplicated_tag | `Empty_head as x -> x
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
    let s_merge_tag t k query =
      let max_depth, n = mk_merge_query query in
      S.merge_tag t ?max_depth ?n k >>| fun () ->
      S.head_exn t >>=
      ok
    in
    let s_lcas_tag t tag query =
      let max_depth, n = mk_merge_query query in
      S.lcas_tag t ?max_depth ?n tag
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
    let s_update_tag t tag = S.update_tag t tag >>= fun () -> S.head_exn t in
    let s_iter t fn = S.iter t (fun k _ -> fn k) in
    let l f t list = f t (S.Key.create list) in
    let hooks = hooks.update in
    let bc t = [
      (* rw *)
      mknp0bf "read" (l S.read) t step' (Tc.option value);
      mknp0bf "mem" (l S.mem) t step' Tc.bool;
      mk0p0bs "iter" (stream s_iter) t key;
      mknp1bf "update" ~lock:lock3 ~hooks (l s_update) t step' value head;
      mknp0bf "remove" ~lock:lock2 ~hooks (l s_remove) t step' head;
      mknp1bf "compare-and-set" ~lock:lock3 ~hooks (l s_compare_and_set) t step'
        (Tc.pair (Tc.option value) (Tc.option value)) Tc.bool;

      (* hrw *)
      mknp0bf "list" (l S.list) t step' (Tc.list key);
      mknp0bf "remove-rec" ~lock:lock2 (l s_remove_rec) t step' head;

      (* more *)
      mk1p0bf "remove-tag" ~lock:lock2 ~hooks S.remove_tag t tag' Tc.unit;
      mk1p0bf "update-tag" ~lock:lock2 ~hooks s_update_tag t tag' head;
      mk1p0bfq "merge-tag" ~lock:lock3 ~hooks s_merge_tag t tag' (merge head);
      mk0p0bf "head" S.head t (Tc.option head);
      mk0p0bf "heads" S.heads t (Tc.list head);
      mk1p0bf "update-head" ~lock:lock2 ~hooks S.update_head t head' Tc.unit;
      mk1p0bfq "fast-forward-head" ~lock:lock3 ~hooks s_fast_forward_head t head' Tc.bool;
      mk0p1bf "compare-and-set-head" ~lock:lock2 ~hooks s_compare_and_set_head t
        (Tc.pair (Tc.option head) (Tc.option head)) Tc.bool;
      mk1p0bfq "merge-head" ~lock:lock3 ~hooks s_merge_head t head' (merge head);
      mk1p0bf "clone" s_clone t tag' ok_or_duplicated_tag;
      mk1p0bf "clone-force" s_clone_force t tag' Tc.unit;
      mk0p0bfq "export" s_export t slice;
      mk0p1bf "import" S.import t slice ok_or_error;
      mk0p0bfq "history" s_history t (module HTC);

      (* lca *)
      mk1p0bfq "lcas-tag" s_lcas_tag  t tag' lca;
      mk1p0bfq "lcas-head" s_lcas_head t head' lca;

      (* extra *)
      mk0p0bh "graph" s_graph t;
    ] in

    SNode (
      bc (fun x task ->
          S.create (S.Private.config x) (fun () -> task) >>= fun x ->
          Lwt.return (x ()))
      @ [
        (* subdirs *)
        "contents", contents_store;
        "node"    , node_store;
        "commit"  , commit_store;
        "tag"     , tag_store;
      ] @ [
        "empty"   , SNode (bc (fun t task ->
            S.empty (S.Private.config t) (fun () -> task) >>= fun t ->
            Lwt.return (t ())))
      ] @ [
        "tree"    , dyn_node
        (fun t ->
          S.tags t  >>= fun tags ->
          S.heads t >>= fun heads ->
          Lwt.return (List.map S.Tag.to_hum tags @ List.map S.Head.to_hum heads)
        )
        (fun _ n ->
           let app fn t x task =
             fn (S.Private.config t) (fun () -> task) x >>= fun t ->
             Lwt.return (t ())
           in
           try
             let n = S.Head.of_hum n in
             Lwt.return (SNode (bc (fun t -> app S.of_head t n)))
           with Irmin.Hash.Invalid _ ->
             Lwt.return (SNode (bc (fun t -> app S.of_tag t (S.Tag.of_hum n)))))
      ])

  let err_bad_version v =
    error "bad client version: expecting %s, but got %s" Irmin.version v

  let err_no_task () = error "the client didn't send any task information."

  let read_body meth ct body = Request.of_body ~meth ct body

  let path uri =
    let path = Uri.path uri in
    let path = Stringext.split path ~on:'/' in
    let path = List.filter ((<>) "") path in
    path

  let process ?(strict=false) t dispatch ~meth ~version ~ct ~uri ~body =
    let () = match version with
      | None   ->
        if strict then err_bad_version "<none>"
        else Log.info "No Irmin header set, skipping the version check"
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
      (* FIXME: we currently don't have the client's task for GET
         queries. See similar comment in [Irmin_http]. *)
      | `GET, None   -> Irmin.Task.empty, None
      | _   , None   -> err_no_task ()
      | _   , Some t -> t
    in
    (* FIXME: validate the client task *)
    let rec aux dispatch = function
      | []      -> json_of_response t dispatch >>= respond_json
      | h::path ->
        let request = { t; task; ct; path; params; query } in
        let f = function
          | Fixed fn  -> fn request >>= respond_contents
          | Stream fn -> fn request |>  respond_json_stream
          | Html fn   -> fn request >>= respond_html
          | child     -> aux child path
        in
        Lwt.catch
          (fun () -> child t h dispatch >>= f)
          (fun e ->
             let query =
               List.map (fun (k, v) -> k ^ "=" ^ String.concat "." v) query
               |> String.concat ","
             in
             Log.debug "uri=%s path=%s query=%s error=%s"
               (Uri.to_string uri)
               (String.concat "/" path)
               query
               (Printexc.to_string e);
             Lwt.fail e)
    in
    aux dispatch (path uri)

  type t = S.t

  let listen ?timeout ?strict ?(hooks=no_hooks) t uri =
    let uri = match Uri.host uri with
      | None   -> Uri.with_host uri (Some "localhost")
      | Some _ -> uri in
    let port, uri = match Uri.port uri with
      | None   -> 8080, Uri.with_port uri (Some 8080)
      | Some p -> p, uri in
    Printf.printf "Server started on port %d.\n%!" port;
    let dispatch = dispatch hooks in
    let callback (_, conn_id) req body =
      let uri = Cohttp.Request.uri req in
      let path = Uri.path (Cohttp.Request.uri req) in
      let meth = Cohttp.Request.meth req in
      let headers = Cohttp.Request.headers req in
      let ct = ct_of_header headers in
      let version = Cohttp.Header.get headers irmin_version in
      Log.info "Connection %s: %s %s (%s)"
        (Cohttp.Connection.to_string conn_id)
        (Cohttp.Code.string_of_method meth)
        path
        (string_of_ct ct);
      Lwt.catch (fun () ->
          try process ?strict t dispatch ~meth ~version ~ct ~uri ~body
          with e -> Lwt.fail e
        ) (fun e -> respond_error ct e)
    in
    let conn_closed (_, conn_id) =
      Log.debug "Connection %s: closed!" (Cohttp.Connection.to_string conn_id)
    in
    let config = HTTP.make ~callback ~conn_closed () in
    HTTP.listen config ?timeout uri

end
