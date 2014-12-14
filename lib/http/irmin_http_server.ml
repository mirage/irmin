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

exception Invalid

module type S = sig
  type t
  val listen: t -> ?timeout:int -> Uri.t -> unit Lwt.t
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
    HTTP.respond_string
      ~headers:json_headers
      ~status:`Internal_server_error
      ~body ()

  let respond ?headers body =
    Log.debug
      (lazy (String.escaped @@
             if String.length body > 140 then
               String.sub body 0 100 ^ ".."
             else body));
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

  type 'a leaf = S.t -> string list -> Ezjsonm.t option -> (string * string list) list -> 'a

  type dynamic_node = {
    list: S.t -> string list Lwt.t;
    child: S.t -> string -> response Lwt.t;
  }

  and response =
    | Fixed  of Ezjsonm.t Lwt.t leaf
    | Stream of Ezjsonm.t Lwt_stream.t leaf
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

  (* no arguments, fixed answer *)
  let mk0p0bf name fn db o =
    name,
    Fixed (fun t path params query ->
        mk0p name path;
        mk0b name params;
        mk0q name query;
        db t >>= fun t ->
        fn t >>= fun r ->
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
  let mk1p0bf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mk1p name i1 path in
        mk0b name params;
        mk0q name query;
        db t >>= fun t ->
        fn t x >>= fun r ->
        return (Tc.to_json o r)
      )

  (* n argument in the path, fixed answer *)
  let mknp0bf name fn db i1 o =
    name,
    Fixed (fun t path params query ->
        let x = mknp i1 path in
        mk0b name params;
        mk0q name query;
        db t >>= fun t ->
        fn t x >>= fun r ->
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

  (* 1 argument in the path, 1 argument in the body, fixed answer *)
  let mk1p1bf name fn db i1 i2 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mk1p name i1 path in
        let x2 = mk1b name i2 params in
        mk0q name query;
        db t >>= fun t ->
        fn t x1 x2 >>= fun r ->
        return (Tc.to_json o r)
      )

  (* n arguments in the path, 1 argument in the body, fixed answer *)
  let mknp1bf name fn db i1 i2 o =
    name,
    Fixed (fun t path params query ->
        let x1 = mknp i1 path in
        let x2 = mk1b name i2 params in
        mk0q name query;
        db t >>= fun t ->
        fn t x1 x2 >>= fun r ->
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
      Dot.output_buffer dump ?depth ?full ~date:D.pretty buffer
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
          Log.debugf "stream push %s" (Tc.show m k);
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
      let to_sexp t = Sexplib.Type.Atom (to_string t)
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

  let export =
    Tc.pair
      (Tc.pair (Tc.option Tc.bool) (Tc.option Tc.int))
      (Tc.pair (Tc.list head) (Tc.list head))

  let merge (type x) (x:x Tc.t): x Irmin.Merge.result Tc.t =
    let module X = (val x: Tc.S0 with type t = x) in
    let module M = Tc.App1(Irmin.Merge.Result)(X) in
    (module M)

  let dyn_node list child = DNode { list; child }

  let store =
    let step': S.step Irmin.Hum.t = (module S.Key.Step) in
    let tag': S.tag Irmin.Hum.t = (module S.Tag) in
    let head': S.head Irmin.Hum.t = (module S.Head) in
    let value: S.value Tc.t = (module S.Val) in
    let slice: S.slice Tc.t = (module S.Private.Slice) in
    let s_export t ((full, depth), (min, max)) =
      S.export ?full ?depth ~min ~max t
    in
    let s_graph t = graph_of_dump t in
    let s_clone t tag =
      S.clone t (fun () -> S.task t) tag >>= function
      | `Ok _ -> return `Ok
      | `Duplicated_tag -> return `Duplicated_tag
    in
    let s_clone_force t tag =
      S.clone_force t (fun () -> S.task t) tag >>= fun _ ->
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
    let s_merge_head t k =
      S.merge_head t k >>| fun () ->
      S.head_exn t >>=
      ok
    in
    let s_merge t k =
      S.merge t k >>| fun () ->
      S.head_exn t >>=
      ok
    in
    let bc t = [
      (* rw *)
      mknp0bf "read"     S.read     t step' (Tc.option value);
      mknp0bf "mem"      S.mem      t step' Tc.bool;
      mk0p0bs "iter"     (stream key S.iter) t key;
      mknp1bf "update"   s_update   t step' value head;
      mknp0bf "remove"   s_remove   t step' head;
      mknp0bs "watch"    S.watch    t step' (Tc.option value);

      (* hrw *)
      mknp0bf "list"       S.list       t step' (Tc.list key);
      mknp0bf "remove-rec" s_remove_rec t step' head;

      (* more *)
      mk1p0bf "update-tag"       S.update_tag t tag' ok_or_duplicated_tag;
      mk1p0bf "update-tag-force" S.update_tag_force t tag' Tc.unit;
      mk1p0bf "switch"           S.switch t tag' Tc.unit;
      mk0p0bf "head"             S.head t (Tc.option head);
      mk0p0bf "heads"            S.heads t (Tc.list head);
      mk1p0bf "update-head"      S.update_head t head' Tc.unit;
      mk1p0bf "merge-head"       s_merge_head t head' (merge head);
      mknp0bs "watch-head"       S.watch_head t step' (Tc.pair key head);
      mk1p0bf "clone"            s_clone t tag' ok_or_duplicated_tag;
      mk1p0bf "clone-force"      s_clone_force t tag' Tc.unit;
      mk1p0bf "merge"            s_merge t tag' (merge head);
      mk0p1bf "export"           s_export t export slice;
      mk0p1bf "import"           S.import t slice Tc.unit;

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
             fn (S.config t) (fun () -> S.task t) x >>= fun t ->
             return (t ())
           in
           try
             let n = S.Head.of_hum n in
             return (SNode (bc (fun t -> app S.of_head t n)))
           with Irmin.Hash.Invalid _ ->
             return (SNode (bc (fun t -> app S.of_tag t (S.Tag.of_hum n)))))
      ])

  let process t req body path =
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
          Log.debugf "process: length=%Ld body=%S" len short_body;
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
      Log.debugf "aux %s" (String.concat "/" path);
      match path with
      | []      -> json_of_response t actions >>= respond_json
      | h::path ->
        child t h actions >>= function
        | Fixed fn  -> fn t path params query >>= respond_json
        | Stream fn -> respond_json_stream (fn t path params query)
        | Html fn   -> fn t path params query >>= respond
        | actions   -> aux actions path in
    aux store path

  type t = S.t

  let listen t ?timeout uri =
    let uri = match Uri.host uri with
      | None   -> Uri.with_host uri (Some "localhost")
      | Some _ -> uri in
    let port, uri = match Uri.port uri with
      | None   -> 8080, Uri.with_port uri (Some 8080)
      | Some p -> p   , uri in
    Printf.printf "Server started on port %d.\n%!" port;
    let callback (_, conn_id) req body =
      let path = Uri.path (Cohttp.Request.uri req) in
      Log.infof "Connection %s: %s %s"
        (Cohttp.Connection.to_string conn_id)
        (Cohttp.(Code.string_of_method (Request.meth req)))
        path;
      let path = Stringext.split path ~on:'/' in
      let path = List.filter ((<>) "") path in
      catch
        (fun () -> process t req body path)
        (fun e  -> respond_error e) in
    let conn_closed (_, conn_id) () =
      Log.debugf "Connection %s: closed!" (Cohttp.Connection.to_string conn_id)
    in
    let config = { HTTP.callback; conn_closed } in
    HTTP.listen config ?timeout uri

end
