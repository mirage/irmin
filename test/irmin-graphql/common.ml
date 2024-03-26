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
open Lwt.Syntax
open Lwt.Infix
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)

let ( / ) = Filename.concat
let http_graphql_dir = "test-graphql"
let socket = http_graphql_dir / "irmin.sock"
let host = "irmin"

let ctx =
  let resolver =
    let h = Hashtbl.create 1 in
    Hashtbl.add h host (`Unix_domain_socket socket);
    Resolver_lwt_unix.static h
  in
  Cohttp_lwt_unix.Client.custom_ctx ~resolver ()

module Server =
  Irmin_graphql_unix.Server.Make
    (Store)
    (struct
      let remote = None
    end)

let mkdir d =
  Lwt.catch
    (fun () -> Lwt_unix.mkdir d 0o755)
    (function
      | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit | e -> Lwt.fail e)

(** Create a GraphQL server over the supplied Irmin repository, returning the
    event loop thread. *)
let server_of_repo : type a. Store.repo -> a Lwt.t =
 fun repo ->
  let server = Server.v repo in
  let* () =
    Lwt.catch
      (fun () -> Lwt_unix.unlink socket)
      (function Unix.Unix_error _ -> Lwt.return_unit | e -> Lwt.fail e)
  in
  mkdir http_graphql_dir >>= fun () ->
  let on_exn = raise and mode = `Unix_domain_socket (`File socket) in
  Cohttp_lwt_unix.Server.create ~on_exn ~mode server >>= fun () ->
  Lwt.fail_with "GraphQL server terminated unexpectedly"

type server = { event_loop : 'a. 'a Lwt.t; store : Store.t }

let spawn_graphql_server ~sw =
  let config = Irmin_mem.config () in
  let repo = Store.Repo.v ~sw config in
  let main = Store.main repo in
  let event_loop = server_of_repo repo in
  { event_loop; store = main }

let rec retry n f =
  let retries = ref 0 in
  Lwt.catch f (fun e ->
      if n = 0 then Lwt.fail e
      else
        Lwt_unix.sleep (0.1 *. float !retries) >>= fun () ->
        incr retries;
        retry (n + 1) f)

type param =
  | Var of string
  | Raw of string
  | String of string
  | Int of int
  | Float of float

type query =
  | Mutation of query
  | Query of query
  | List of query list
  | Func of string * (string * param) list * query
  | Field of string

let rec string_of_query = function
  | Query q -> Printf.sprintf "query { %s }" (string_of_query q)
  | Mutation q -> Printf.sprintf "mutation { %s }" (string_of_query q)
  | List ql -> String.concat "\n" (List.map string_of_query ql)
  | Func (name, params, query) -> (
      match params with
      | [] -> Printf.sprintf "%s { %s }" name (string_of_query query)
      | args ->
          Printf.sprintf "%s (%s) { %s }" name (string_of_args args)
            (string_of_query query))
  | Field s -> s

and string_of_args args =
  List.map
    (fun (k, v) ->
      let v =
        match v with
        | Var s -> "$" ^ s
        | String s -> "\"" ^ s ^ "\""
        | Raw s -> s
        | Int i -> string_of_int i
        | Float f -> string_of_float f
      in
      k ^ ": " ^ v)
    args
  |> String.concat ", "

let query q = Query q
let mutation q = Mutation q
let list l = List l
let func name ?(params = []) q = Func (name, params, q)
let field s = Field s
let string s = String s
let raw s = Raw s
let var s = Var s
let int i = Int i
let float f = Float f

let find_result res x =
  let rec aux (res : Yojson.Safe.t) x =
    match res with
    | `List l -> `List (List.map (fun res -> aux res x) l)
    | `Assoc _ -> (
        match x with
        | Query q | Mutation q -> aux (Yojson.Safe.Util.member "data" res) q
        | Func (name, _, q) -> aux (Yojson.Safe.Util.member name res) q
        | Field name -> Yojson.Safe.Util.member name res
        | List l ->
            `Assoc
              (List.map
                 (function
                   | Field name as item -> (name, aux res item)
                   | Func (name, _, _) as item -> (name, aux res item)
                   | _ -> assert false)
                 l))
    | x -> x
  in
  aux res x

(** Issue a query to the localhost server and return the body of the response
    message *)
let send_query :
    ?vars:(string * Yojson.Safe.t) list ->
    string ->
    (string, [ `Msg of string ]) result Lwt.t =
 fun ?(vars = []) query ->
  let headers = Cohttp.Header.init_with "Content-Type" "application/json"
  and body =
    Yojson.Safe.to_string
      (`Assoc [ ("query", `String query); ("variables", `Assoc vars) ])
    |> Cohttp_lwt.Body.of_string
  in
  let* response, body =
    retry 10 (fun () ->
        Cohttp_lwt_unix.Client.post ~headers ~body ~ctx
          (Uri.make ~scheme:"http" ~host ~path:"graphql" ()))
  in
  let status = Cohttp.Response.status response in
  let+ body = Cohttp_lwt.Body.to_string body in
  match Cohttp.Code.(status |> code_of_status |> is_success) with
  | true -> Ok body
  | false ->
      let msg =
        Format.sprintf "Response: %s\nBody:\n%s"
          (Cohttp.Code.string_of_status status)
          body
      in
      Error (`Msg msg)

let members keys json =
  List.fold_left (fun key json -> Yojson.Safe.Util.member json key) json keys

let parse_result k f res = f (members k res)

(** Issue a query to the localhost server, parse the response object and convert
    it using [f] *)
let exec ?vars query f =
  let res =
    Lwt_eio.run_lwt @@ fun () -> send_query ?vars (string_of_query query)
  in
  match res with
  | Error (`Msg e) -> Alcotest.fail e
  | Ok res ->
      let res = Yojson.Safe.from_string res in
      let value = find_result res query in
      print_endline (Yojson.Safe.to_string value);
      f value
