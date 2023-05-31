(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
open Common
module T = Irmin.Type

let to_json = Irmin.Type.to_json_string
let of_json = Irmin.Type.of_json_string
let src = Logs.Src.create "irmin.http-srv" ~doc:"Irmin REST API server"

module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig
  type repo
  type t

  val v : ?strict:bool -> repo -> t
end

let ok = { status = "ok" }
let bool b = { status = string_of_bool b }

module Make (HTTP : Cohttp_lwt.S.Server) (S : Irmin.S) = struct
  module Wm = struct
    module Rd = Webmachine.Rd

    module Clock = struct
      let now () = int_of_float (Unix.gettimeofday ())
    end

    include Webmachine.Make (HTTP.IO) (Clock)
  end

  module B = S.Backend

  class virtual resource =
    object
      inherit [Cohttp_lwt.Body.t] Wm.resource

      method! finish_request rd =
        Wm.Rd.with_resp_headers
          (fun h -> Cohttp.Header.add h irmin_version Irmin.version)
          rd
        |> Wm.continue ()
    end

  let parse_error rd str (`Msg e) =
    let err = Fmt.str "Parse error %S: %s" str e in
    Wm.respond ~body:(`String err) 400 rd

  module Content_addressable (S : sig
    include Irmin.Content_addressable.S

    val batch : B.Repo.t -> (read_write t -> 'a Lwt.t) -> 'a Lwt.t
  end)
  (K : Irmin.Type.S with type t = S.key)
  (V : Irmin.Type.S with type t = S.value) =
  struct
    open Lwt.Syntax

    let with_key rd f =
      match Irmin.Type.of_string K.t (Wm.Rd.lookup_path_info_exn "id" rd) with
      | Ok key -> f key
      | Error _ -> Wm.respond 404 rd

    let add rd repo =
      let* body = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
      match Irmin.Type.of_string V.t body with
      | Error e -> parse_error rd body e
      | Ok body ->
          S.batch repo @@ fun db ->
          let* new_id = S.add db body in
          let resp_body = `String (Irmin.Type.to_string K.t new_id) in
          Wm.continue true { rd with Wm.Rd.resp_body }

    let unsafe_add rd repo key =
      let* body = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
      match Irmin.Type.of_string V.t body with
      | Error e -> parse_error rd body e
      | Ok body ->
          S.batch repo @@ fun db ->
          S.unsafe_add db key body >>= fun () ->
          let resp_body = `String "" in
          Wm.continue true { rd with Wm.Rd.resp_body }

    class items repo =
      object
        inherit resource
        method! allowed_methods rd = Wm.continue [ `POST ] rd

        method content_types_provided rd =
          Wm.continue [ ("application/json", fun _ -> assert false) ] rd

        method content_types_accepted rd = Wm.continue [] rd
        method! process_post rd = add rd repo
      end

    class unsafe_items repo =
      object
        inherit resource
        method! allowed_methods rd = Wm.continue [ `POST ] rd

        method content_types_provided rd =
          Wm.continue [ ("application/json", fun _ -> assert false) ] rd

        method content_types_accepted rd = Wm.continue [] rd
        method! process_post rd = with_key rd (unsafe_add rd repo)
      end

    class merge merge repo =
      object
        inherit resource
        method! allowed_methods rd = Wm.continue [ `POST ] rd

        method content_types_provided rd =
          Wm.continue [ ("application/json", fun _ -> assert false) ] rd

        method content_types_accepted rd = Wm.continue [] rd

        method! process_post rd =
          let* body = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
          match Irmin.Type.(of_string (merge_t (option K.t))) body with
          | Error e -> parse_error rd body e
          | Ok { old; left; right } ->
              S.batch repo @@ fun db ->
              let old = Irmin.Merge.promise old in
              let* m = Irmin.Merge.f (merge db) ~old left right in
              let result = merge_result_t K.t in
              let resp_body = `String Irmin.(Type.to_string result m) in
              Wm.continue true { rd with Wm.Rd.resp_body }
      end

    class item db =
      object (self)
        inherit resource

        method private to_json rd =
          with_key rd (fun key ->
              let str = Irmin.Type.to_string V.t in
              S.find db key >>= function
              | Some value -> Wm.continue (`String (str value)) rd
              | None -> Wm.respond 404 rd)

        method! allowed_methods rd = Wm.continue [ `GET; `HEAD ] rd
        method content_types_accepted rd = Wm.continue [] rd

        method! resource_exists rd =
          with_key rd (fun key ->
              let* mem = S.mem db key in
              Wm.continue mem rd)

        method content_types_provided rd =
          Wm.continue [ ("application/json", self#to_json) ] rd
      end
  end

  module Atomic_write
      (S : Irmin.Atomic_write.S)
      (K : Irmin.Type.S with type t = S.key)
      (V : Irmin.Type.S with type t = S.value) =
  struct
    class items db =
      object (self)
        inherit resource
        method! allowed_methods rd = Wm.continue [ `GET; `HEAD ] rd
        method content_types_accepted rd = Wm.continue [] rd

        method private to_json rd =
          let* keys = S.list db in
          let json = to_json T.(list K.t) keys in
          Wm.continue (`String json) rd

        method content_types_provided rd =
          Wm.continue [ ("application/json", self#to_json) ] rd
      end

    let with_key rd f =
      match Irmin.Type.of_string K.t rd.Wm.Rd.dispatch_path with
      | Ok x -> f x
      | Error _ -> Wm.respond 404 rd

    class item db =
      object (self)
        inherit resource

        method private of_json rd =
          let* body = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
          match of_json (set_t V.t) body with
          | Error e -> parse_error rd body e
          | Ok v ->
              with_key rd (fun key ->
                  match v.v with
                  | Some v ->
                      S.set db key v >>= fun () ->
                      let resp_body = `String (to_json status_t ok) in
                      let rd = { rd with Wm.Rd.resp_body } in
                      Wm.continue true rd
                  | None ->
                      let* b = S.test_and_set db key ~test:v.test ~set:v.set in
                      let resp_body = `String (to_json status_t (bool b)) in
                      let rd = { rd with Wm.Rd.resp_body } in
                      Wm.continue b rd)

        method private to_json rd =
          with_key rd (fun key ->
              let str = Irmin.Type.to_string V.t in
              S.find db key >>= function
              | Some value -> Wm.continue (`String (str value)) rd
              | None -> Wm.respond 404 rd)

        method! resource_exists rd =
          with_key rd (fun key ->
              let* mem = S.mem db key in
              Wm.continue mem rd)

        method! allowed_methods rd =
          Wm.continue [ `GET; `HEAD; `PUT; `DELETE ] rd

        method content_types_provided rd =
          Wm.continue [ ("application/json", self#to_json) ] rd

        method content_types_accepted rd =
          Wm.continue [ ("application/json", self#of_json) ] rd

        method! delete_resource rd =
          with_key rd (fun key ->
              S.remove db key >>= fun () ->
              let resp_body = `String (to_json status_t ok) in
              Wm.continue true { rd with Wm.Rd.resp_body })
      end

    class watches db =
      object (self)
        inherit resource
        method! allowed_methods rd = Wm.continue [ `GET; `HEAD; `POST ] rd
        method content_types_accepted rd = Wm.continue [] rd

        method private stream ?init () =
          let stream, push = Lwt_stream.create () in
          let init =
            Option.map (List.map (fun i -> (i.branch, i.commit))) init
          in
          let+ w =
            S.watch ?init db (fun branch diff ->
                let v = to_json (event_t K.t V.t) { branch; diff } in
                push (Some v);
                push (Some ",");
                Lwt.return_unit)
          in
          Lwt.async (fun () ->
              Lwt_stream.closed stream >>= fun () -> S.unwatch db w);
          push (Some "[");
          `Stream stream

        method! process_post rd =
          let* body = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
          match of_json T.(list (init_t K.t V.t)) body with
          | Error e -> parse_error rd body e
          | Ok init ->
              let* resp_body = self#stream ~init () in
              Wm.continue true { rd with Wm.Rd.resp_body }

        method private of_json rd =
          let* body = self#stream () in
          Wm.continue body rd

        method content_types_provided rd =
          Wm.continue [ ("application/json", self#of_json) ] rd
      end

    class watch db =
      object (self)
        inherit resource
        method! allowed_methods rd = Wm.continue [ `GET; `HEAD; `POST ] rd
        method content_types_accepted rd = Wm.continue [] rd

        method private stream ?init key =
          let stream, push = Lwt_stream.create () in
          let+ w =
            S.watch_key ?init db key (fun diff ->
                let v = to_json (event_t K.t V.t) { branch = key; diff } in
                push (Some v);
                push (Some ",");
                Lwt.return_unit)
          in
          Lwt.async (fun () ->
              Lwt_stream.closed stream >>= fun () -> S.unwatch db w);
          push (Some "[");
          `Stream stream

        method! process_post rd =
          let* body = Cohttp_lwt.Body.to_string rd.Wm.Rd.req_body in
          match of_json V.t body with
          | Error e -> parse_error rd body e
          | Ok init ->
              with_key rd (fun key ->
                  let* resp_body = self#stream ~init key in
                  Wm.continue true { rd with Wm.Rd.resp_body })

        method private of_json rd =
          with_key rd (fun key ->
              let* body = self#stream key in
              Wm.continue body rd)

        method content_types_provided rd =
          Wm.continue [ ("application/json", self#of_json) ] rd
      end
  end

  module Blob =
    Content_addressable
      (struct
        include B.Contents

        let unsafe_add t k v = unsafe_add t k v >|= fun _ -> ()
        let batch t f = B.Repo.batch t @@ fun x _ _ -> f x
      end)
      (B.Contents.Key)
      (B.Contents.Val)

  module Tree =
    Content_addressable
      (struct
        include B.Node

        let unsafe_add t k v = unsafe_add t k v >|= fun _ -> ()
        let batch t f = B.Repo.batch t @@ fun _ x _ -> f x
      end)
      (B.Node.Key)
      (B.Node.Val)

  module Commit =
    Content_addressable
      (struct
        include B.Commit

        let unsafe_add t k v = unsafe_add t k v >|= fun _ -> ()
        let batch t f = B.Repo.batch t @@ fun _ _ x -> f x
      end)
      (B.Commit.Key)
      (B.Commit.Val)

  module Branch = Atomic_write (B.Branch) (B.Branch.Key) (B.Branch.Val)

  type repo = S.Repo.t
  type t = HTTP.t

  let v ?strict:_ db =
    let blob = B.Repo.contents_t db in
    let tree = B.Repo.node_t db in
    let commit = B.Repo.commit_t db in
    let branch = B.Repo.branch_t db in
    let routes =
      [
        ("/blobs", fun () -> new Blob.items db);
        ("/blob/:id", fun () -> new Blob.item blob);
        ("/trees", fun () -> new Tree.items db);
        ("/trees/merge", fun () -> new Tree.merge S.Backend.Node.merge db);
        ("/tree/:id", fun () -> new Tree.item tree);
        ("/commits", fun () -> new Commit.items db);
        ("/commit/:id", fun () -> new Commit.item commit);
        ("/unsafe/blobs/:id", fun () -> new Blob.unsafe_items db);
        ("/unsafe/trees/:id", fun () -> new Tree.unsafe_items db);
        ("/unsafe/commits/:id", fun () -> new Commit.unsafe_items db);
        ("/branches", fun () -> new Branch.items branch);
        ("/branch/*", fun () -> new Branch.item branch);
        ("/watches", fun () -> new Branch.watches branch);
        ("/watch/*", fun () -> new Branch.watch branch);
      ]
    in
    let pp_con = Fmt.of_to_string Cohttp.Connection.to_string in
    let callback (_ch, conn) request body =
      let open Cohttp in
      [%log.debug "new connection %a" pp_con conn];
      let* status, headers, body, _path =
        Wm.dispatch' routes ~body ~request >|= function
        | None -> (`Not_found, Header.init (), `String "Not found", [])
        | Some result -> result
      in
      [%log.info
        "[%a] %d - %s %s" pp_con conn
          (Code.code_of_status status)
          (Code.string_of_method (Request.meth request))
          (Uri.path (Request.uri request))];

      (* Finally, send the response to the client *)
      HTTP.respond ~headers ~body ~status ()
    in
    (* create the server and handle requests with the function defined above *)
    let conn_closed (_, conn) =
      [%log.debug "connection %a closed" pp_con conn]
    in
    HTTP.make ~callback ~conn_closed ()
end
