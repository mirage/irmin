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

let error fmt =
  Printf.ksprintf (fun msg ->
      failwith ("error: " ^ msg)
    ) fmt

let string_replace ~pattern subst str =
  let rex = Re_perl.compile_pat pattern in
  Re_pcre.substitute ~rex ~subst str

module Log = Log.Make(struct let section = "HTTP.server" end)

let json_headers = Cohttp.Header.of_list [
    "Content-type", "application/json"
  ]

let http_headers = Cohttp.Header.of_list []

exception Invalid

type hooks = { update: unit -> unit Lwt.t }
let no_hooks = { update = fun () -> Lwt.return_unit }

module type S = sig
  type t
  val listen: t -> ?timeout:int -> ?hooks:hooks -> Uri.t -> unit Lwt.t
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

  let read_exn file =
    match Irmin_http_static.read file with
    | None   -> error "%s: not found" file
    | Some s -> s

  let respond_error e =
    let json = `O [ "error", Ezjsonm.encode_string (Printexc.to_string e) ] in
    let body = Ezjsonm.to_string json in
    Log.error "server error: %s" body;
    HTTP.respond_string
      ~headers:json_headers
      ~status:`Internal_server_error
      ~body ()

  let respond ?headers body =
    HTTP.respond_string ?headers ~status:`OK ~body ()

  let respond_json json =
    let json = `O [ "result", json ] in
    let body = Ezjsonm.to_string json in
    respond ~headers:json_headers body

  let respond_json_stream stream =
    let (++) = Lwt_stream.append in
    let stream =
      (Lwt_stream.of_list ["["])
      ++ (Lwt_stream.map (fun j -> Ezjsonm.to_string (`O ["result", j]) ^ ",") stream)
      ++ (Lwt_stream.of_list [" ]"])
    in
    let body = Cohttp_lwt_body.of_stream stream in
    HTTP.respond ~headers:json_headers ~status:`OK ~body ()

  type 'a leaf = S.t -> string list -> Ezjsonm.value option -> (string * string list) list -> 'a

  type dynamic_node = {
    list: S.t -> string list Lwt.t;
    child: S.t -> string -> response Lwt.t;
  }

  and response =
    | Fixed  of Ezjsonm.value Lwt.t leaf
    | Stream of Ezjsonm.value Lwt_stream.t leaf
    | SNode  of (string * response) list
    | DNode  of dynamic_node
    | Html   of string Lwt.t leaf

  let json_of_response t r =
    let str = Ezjsonm.encode_string in
    let list x = `A x in
    match r with
    | Fixed   _
    | Stream _
    | Html _   -> return (str "<data>")
    | SNode c  -> return (list ((List.map (fun (n, _) -> str n) c)))
    | DNode d  -> d.list t >>= fun l -> return (list (List.map str l))

  let child t c r: response Lwt.t =
    let error () = fail (Failure ("Unknown action: " ^ c)) in
    match r with
    | Fixed _
    | Stream _ -> error ()
    | Html _   -> return r
    | DNode d  -> d.child t c
    | SNode l  ->
      try return (List.assoc c l)
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
    | Some b  -> Tc.of_json i b

  let id = let c = ref 0 in fun () -> incr c; !c

  let with_lock lock fn = match lock with
    | None   -> fn ()
    | Some l ->
      let id = id () in
      Log.debug "Lock %d taken" id;
      Lwt_mutex.with_lock l fn >>= fun r ->
      Log.debug "Lock %d released" id;
      Lwt.return r

  let run_hooks lock = function
    | None   -> Lwt.return_unit
    | Some f -> with_lock lock f

  (* no arguments, fixed answer *)
  let mk0p0bf ?lock ?hooks name fn db o =
    name,
    Fixed (fun t path params query ->
        mk0p name path;
        mk0b name params;
        mk0q name query;
        db t >>= fun t ->
        with_lock lock (fun () -> fn t) >>= fun r ->
        run_hooks lock hooks >>= fun () ->
        return (Tc.to_json o r)
      )

  (* 0 argument, return an html page. *)
  let mk0p0bh name fn db =
    name,
    Html (fun t path params query ->
        mk0b name params;
        db t >>= fun t ->
        fn t path query
      )

  (* 0 arguments, return a stream *)
  let mk0p0bs name fn db o =
    name,
    Stream (fun t path params query ->
        mk0p name path;
        mk0b name params;
        mk0q name query;
        Irmin.Private.Watch.lwt_stream_lift
          (db t >>= fun t ->
           let stream = fn t in
           let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
           return stream)
      )

  (* 1 argument in the path, fixed answer *)
  let mk1p0bf name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mk1p name i1 path in
        mk0b name params;
        mk0q name query;
        db t >>= fun t ->
        with_lock lock (fun () -> fn t x) >>= fun r ->
        run_hooks lock hooks >>= fun () ->
        return (Tc.to_json o r)
      )

  (* 1 argument in the path, fixed answer and parameters in the query *)
  let mk1p0bfq name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mk1p name i1 path in
        mk0b name params;
        db t >>= fun t ->
        with_lock lock (fun () -> fn t x query) >>= fun r ->
        run_hooks lock hooks >>= fun () ->
        return (Tc.to_json o r)
      )

  (* n argument in the path, fixed answer *)
  let mknp0bf name ?lock ?hooks fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mknp i1 path in
        mk0b name params;
        mk0q name query;
        db t >>= fun t ->
        with_lock lock (fun () -> fn t x) >>= fun r ->
        run_hooks lock hooks >>= fun () ->
        return (Tc.to_json o r)
      )

  (* 1 argument in the body *)
  let mk0p1bf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        mk0p name path;
        mk0q name query;
        let x = mk1b name i1 params in
        db t >>= fun t ->
        fn t x >>= fun r ->
        return (Tc.to_json o r)
      )

  (* 1 argument in the body + query *)
  let mk0p1bfq name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        mk0p name path;
        let x = mk1b name i1 params in
        db t >>= fun t ->
        fn t x query >>= fun r ->
        return (Tc.to_json o r)
      )

  (* 1 argument in the path, 1 argument in the body, fixed answer *)
  let mk1p1bf name ?lock ?hooks fn db i1 i2 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mk1p name i1 path in
        let x2 = mk1b name i2 params in
        mk0q name query;
        db t >>= fun t ->
        with_lock lock (fun () -> fn t x1 x2) >>= fun r ->
        run_hooks lock hooks >>= fun () ->
        return (Tc.to_json o r)
      )

  (* n arguments in the path, 1 argument in the body, fixed answer *)
  let mknp1bf name ?lock ?hooks fn db i1 i2 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mknp i1 path in
        let x2 = mk1b name i2 params in
        mk0q name query;
        db t >>= fun t ->
        with_lock lock (fun () -> fn t x1 x2) >>= fun r ->
        run_hooks lock hooks >>= fun () ->
        return (Tc.to_json o r)
      )

  (* 1 of arguments in the path, no body, streamed response *)
  let mk1p0bs name fn db i1 o =
    name,
    Stream (fun t path _params query ->
        let x1 = mk1p name i1 path in
        mk0q name query;
        (* mk0b name params; *)
        Irmin.Private.Watch.lwt_stream_lift
          (db t >>= fun t ->
           let stream = fn t x1 in
           let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
           return stream)
      )

  (* list of arguments in the path, no body, streamed response *)
  let mknp0bs name fn db i1 o =
    name,
    Stream (fun t path _params query ->
        let x1 = mknp i1 path in
        mk0q name query;
        (* mk0b name params; *)
        Irmin.Private.Watch.lwt_stream_lift
          (db t >>= fun t ->
           let stream = fn t x1 in
           let stream = Lwt_stream.map (fun r -> Tc.to_json o r) stream in
           return stream)
      )

  let graph_index = read_exn "index.html"

  module Dot = Irmin.Dot(S)

  let graph_of_dump dump path query =
    match path with
    | [] -> return graph_index
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
      return str
    | [file] -> mk0q "graph" query; return (read_exn file)
    | l      -> error "%s: not found" (String.concat "/" l)

  let ao_store (type key) (type value) (type l)
      (module M: Irmin.AO with type t = l and type key = key and type value = value)
      (module K: Irmin.Hum.S with type t = M.key)
      (module V: Tc.S0 with type t = M.value)
      (fn: S.t -> l Lwt.t) =
    let key': M.key Irmin.Hum.t = (module K) in
    let key: M.key Tc.t = (module K) in
    let value: M.value Tc.t = (module V) in
    SNode [
      mk1p0bf "read" M.read fn key' (Tc.option value);
      mk1p0bf "mem"  M.mem  fn key' Tc.bool;
      mk0p1bf "add"  M.add  fn value key;
    ]

  let contents_store = ao_store
      (module S.Private.Contents)
      (module S.Private.Contents.Key)
      (module S.Private.Contents.Val)
      (fun t -> return (S.Private.contents_t t))

  let node_store = ao_store
      (module S.Private.Node)
      (module S.Private.Node.Key)
      (module S.Private.Node.Val)
      (fun t -> return (S.Private.node_t t))

  let commit_store = ao_store
      (module S.Private.Commit)
      (module S.Private.Commit.Key)
      (module S.Private.Commit.Val)
      (fun t -> return (S.Private.commit_t t))

  let stream m fn t =
    let stream, push = Lwt_stream.create () in
    Irmin.Private.Watch.lwt_stream_lift (
      fn t (fun k ->
          Log.debug "stream push %s" (Tc.show m k);
          push (Some k);
          return_unit
        ) >>= fun () ->
      push None;
      return stream
    )

  let tag_store =
    let open S.Private.Tag in
    let tag_t t = return (S.Private.tag_t t) in
    let tag': S.tag Irmin.Hum.t = (module S.Tag) in
    let tag: S.tag Tc.t = (module S.Tag) in
    let head: S.head Tc.t = (module S.Head) in
    SNode [
      mk1p0bf "read"   read   tag_t tag' (Tc.option head);
      mk1p0bf "mem"    mem    tag_t tag' Tc.bool;
      mk0p0bs "iter"   (stream tag iter) tag_t tag;
      mk1p1bf "update" update tag_t tag' head Tc.unit;
      mk1p0bf "remove" remove tag_t tag' Tc.unit;
      mk1p0bs "watch"  watch  tag_t tag' (Tc.option head);
    ]

  let ok_or_duplicated_tag =
    let module M = struct
      type t = [ `Ok | `Duplicated_tag ]
      let to_string = function
        | `Ok -> "ok"
        | `Duplicated_tag -> "duplicated-tag"
      let to_json t = `String (to_string t)
      let compare = Pervasives.compare
      let equal = (=)
      let hash = Hashtbl.hash
      let of_json _ = failwith "TODO"
      let write _ = failwith "TODO"
      let read _ = failwith "TODO"
      let size_of _ = failwith "TODO"
    end in
    (module M: Tc.S0 with type t = M.t)

  let key: S.key Tc.t = (module S.Key)
  let head: S.head Tc.t = (module S.Head)

  let export = Tc.pair (Tc.list head) (Tc.list head)

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

  let mk_merge_query query =
    let max_depth = get_query query int_of_string "depth" in
    let n = get_query query int_of_string "n" in
    max_depth, n

  let mk_export_query query =
    let full = get_query query bool_of_string "full" in
    let depth = get_query query int_of_string "depth" in
    full, depth

  let mk_history_query query =
    let depth = get_query query int_of_string "depth" in
    depth

  module LCA = struct
    module HL = Tc.List(S.Head)
    type t = [`Ok of S.Head.t list | `Max_depth_reached | `Too_many_lcas]
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
    let equal = (=)
    let to_json = function
      | `Ok x -> `O ["ok", HL.to_json x]
      | `Max_depth_reached -> `A [`String "max-depth-reached" ]
      | `Too_many_lcas -> `A [`String "too-many-lcas"]
    let of_json _ = failwith "TODO"
    let read _ = failwith "TODO"
    let write _ = failwith "TODO"
    let size_of _ = failwith "TODO"
  end

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

  let store lock hooks =
    let step': S.Key.step Irmin.Hum.t = (module S.Key.Step) in
    let tag': S.tag Irmin.Hum.t = (module S.Tag) in
    let tag: S.tag Tc.t = (module S.Tag) in
    let head': S.head Irmin.Hum.t = (module S.Head) in
    let value: S.value Tc.t = (module S.Val) in
    let slice: S.slice Tc.t = (module S.Private.Slice) in
    let min_max: (S.head list option * S.head list option) Tc.t =
      let module M = Tc.Option(Tc.List(S.Head)) in
      (module Tc.Pair(M)(M))
    in
    let s_export t (min, max) query =
      let full, depth = mk_export_query query in
      S.export ?full ?depth ~min ~max t
    in
    let s_history t (min, max) query =
      let depth = mk_history_query query in
      S.history ?depth ?min ?max t
    in
    let s_graph t = graph_of_dump t in
    let s_clone t tag =
      S.clone (fun () -> S.task t) t tag >>= function
      | `Ok _ -> return `Ok
      | `Duplicated_tag -> return `Duplicated_tag
    in
    let s_clone_force t tag =
      S.clone_force (fun () -> S.task t) t tag >>= fun _ ->
      return_unit
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
    let l f t list = f t (S.Key.create list) in
    let hooks = hooks.update in
    let bc t = [
      (* rw *)
      mknp0bf "read"   (l S.read)     t step' (Tc.option value);
      mknp0bf "mem"    (l S.mem)      t step' Tc.bool;
      mk0p0bs "iter"   (stream key S.iter) t key;
      mknp1bf "update" ~lock ~hooks (l s_update) t step' value head;
      mknp0bf "remove" ~lock ~hooks (l s_remove) t step' head;
      mknp0bs "watch"  (l S.watch)    t step' (Tc.option value);

      (* hrw *)
      mknp0bf "list"       (l S.list)       t step' (Tc.list key);
      mknp0bf "remove-rec" ~lock (l s_remove_rec) t step' head;

      (* more *)
      mk0p0bf "remove-tag"  ~lock ~hooks S.remove_tag t Tc.unit;
      mk1p0bf "rename-tag"  ~lock ~hooks S.rename_tag t tag' ok_or_duplicated_tag;
      mk1p0bf "update-tag"  ~lock ~hooks S.update_tag t tag' Tc.unit;
      mk1p0bfq "merge-tag"  ~lock ~hooks s_merge_tag t tag' (merge head);
      mk0p0bf "head"        S.head t (Tc.option head);
      mk0p0bf "heads"       S.heads t (Tc.list head);
      mk1p0bf "update-head" ~lock ~hooks S.update_head t head' Tc.unit;
      mk1p0bfq "merge-head" ~lock ~hooks s_merge_head t head' (merge head);
      mknp0bs "watch-head"  (l S.watch_head) t step' (Tc.pair key (Tc.option head));
      mk0p0bs "watch-tags"  S.watch_tags t (Tc.pair tag (Tc.option head));
      mk1p0bf "clone"       s_clone t tag' ok_or_duplicated_tag;
      mk1p0bf "clone-force" s_clone_force t tag' Tc.unit;
      mk0p1bfq "export"     s_export t export slice;
      mk0p1bf "import"      S.import t slice Tc.unit;
      mk0p1bfq "history"    s_history t min_max (module HTC);

      (* lca *)
      mk1p0bfq "lcas-tag"  s_lcas_tag  t tag'  (module LCA);
      mk1p0bfq "lcas-head" s_lcas_head t head' (module LCA);

      (* extra *)
      mk0p0bh "graph" s_graph t;
    ] in

    SNode (
      bc (fun x -> return x)
      @ [
        (* subdirs *)
        "contents", contents_store;
        "node"    , node_store;
        "commit"  , commit_store;
        "tag"     , tag_store;
      ] @ [
        "tree"    , dyn_node
        (fun t ->
          S.tags t  >>= fun tags ->
          S.heads t >>= fun heads ->
          return (List.map S.Tag.to_hum tags @ List.map S.Head.to_hum heads))
        (fun _ n ->
           let app fn t x =
             fn (S.Private.config t) (fun () -> S.task t) x >>= fun t ->
             return (t ())
           in
           try
             let n = S.Head.of_hum n in
             return (SNode (bc (fun t -> app S.of_head t n)))
           with Irmin.Hash.Invalid _ ->
             return (SNode (bc (fun t -> app S.of_tag t (S.Tag.of_hum n)))))
      ])

  let process t store req body path =
    begin match Cohttp.Request.meth req with
      | `DELETE
      | `GET       -> return_none
      | `POST ->
        Cohttp_lwt_body.length body >>= fun (len, body) ->
        if len = 0L then
          return_none
        else begin
          Cohttp_lwt_body.to_string body >>= fun b ->
          let short_body =
            if String.length b > 80 then String.sub b 0 80 ^ ".." else b
          in
          Log.debug "process: length=%Ld body=%S" len short_body;
          try match Ezjsonm.from_string b with
            | `O l ->
              if List.mem_assoc "params" l then (
                try return (Some (List.assoc "params" l))
                with Not_found -> return_none
              ) else
                error "process: wrong request"
            | _    ->
              error "process: wrong parameters"
          with e ->
            error "process: not a valid JSON body %S [%s]" b (Printexc.to_string e)
        end
      | _ -> fail Invalid
    end >>= fun params ->
    let query = Uri.query (Cohttp.Request.uri req) in
    let rec aux actions path =
      Log.debug "aux %s" (String.concat "/" path);
      match path with
      | []      -> json_of_response t actions >>= respond_json
      | h::path ->
        child t h actions >>= function
        | Fixed fn  -> fn t path params query >>= respond_json
        | Stream fn -> respond_json_stream (fn t path params query)
        | Html fn   -> fn t path params query >>= respond ~headers:http_headers
        | actions   -> aux actions path in
    aux store path

  type t = S.t

  let listen t ?timeout ?(hooks=no_hooks) uri =
    let uri = match Uri.host uri with
      | None   -> Uri.with_host uri (Some "localhost")
      | Some _ -> uri in
    let port, uri = match Uri.port uri with
      | None   -> 8080, Uri.with_port uri (Some 8080)
      | Some p -> p   , uri in
    Printf.printf "Server started on port %d.\n%!" port;
    let lock = Lwt_mutex.create () in
    let store = store lock hooks in
    let callback (_, conn_id) req body =
      let path = Uri.path (Cohttp.Request.uri req) in
      Log.info "Connection %s: %s %s"
        (Cohttp.Connection.to_string conn_id)
        (Cohttp.(Code.string_of_method (Request.meth req)))
        path;
      let path = Stringext.split path ~on:'/' in
      let path = List.filter ((<>) "") path in
      catch
        (fun () -> try process t store req body path with e -> fail e)
        (fun e  -> respond_error e)
    in
    let conn_closed (_, conn_id) =
      Log.debug "Connection %s: closed!" (Cohttp.Connection.to_string conn_id)
    in
    let config = HTTP.make ~callback ~conn_closed () in
    HTTP.listen config ?timeout uri

end
