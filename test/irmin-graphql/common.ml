open Lwt.Infix
module Store = Irmin_mem.KV (Irmin.Contents.String)

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
  Irmin_graphql.Server.Make
    (Cohttp_lwt_unix.Server)
    (struct
      let remote = None

      let info ?(author = "graphql-test-author") =
        Format.kasprintf (fun msg () -> Irmin.Info.v ~date:0L ~author msg)
    end)
    (Store)

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
  Lwt.catch
    (fun () -> Lwt_unix.unlink socket)
    (function Unix.Unix_error _ -> Lwt.return_unit | e -> Lwt.fail e)
  >>= fun () ->
  mkdir http_graphql_dir >>= fun () ->
  let on_exn = raise and mode = `Unix_domain_socket (`File socket) in
  Cohttp_lwt_unix.Server.create ~on_exn ~mode server >>= fun () ->
  Lwt.fail_with "GraphQL server terminated unexpectedly"

type server = {
  event_loop : 'a. 'a Lwt.t;
  set_tree : Store.Tree.concrete -> unit Lwt.t;
}

let spawn_graphql_server () =
  let config = Irmin_mem.config () in
  Store.Repo.v config >>= fun repo ->
  Store.master repo >|= fun master ->
  let event_loop = server_of_repo repo
  and set_tree tree =
    Store.Tree.of_concrete tree
    |> Store.set_tree_exn ~info:Irmin.Info.none master []
  in
  { event_loop; set_tree }

let rec retry n f =
  let retries = ref 0 in
  Lwt.catch f (fun e ->
      if n = 0 then Lwt.fail e
      else
        Lwt_unix.sleep (0.1 *. float !retries) >>= fun () ->
        incr retries;
        retry (n + 1) f)

(** Issue a query to the localhost server and return the body of the response
    message *)
let send_query : string -> (string, [ `Msg of string ]) result Lwt.t =
 fun query ->
  let headers = Cohttp.Header.init_with "Content-Type" "application/json"
  and body =
    Yojson.Safe.to_string (`Assoc [ ("query", `String query) ])
    |> Cohttp_lwt.Body.of_string
  in
  retry 10 (fun () ->
      Cohttp_lwt_unix.Client.post ~headers ~body ~ctx
        (Uri.make ~scheme:"http" ~host ~path:"graphql" ()))
  >>= fun (response, body) ->
  let status = Cohttp_lwt.Response.status response in
  Cohttp_lwt.Body.to_string body >|= fun body ->
  match Cohttp.Code.(status |> code_of_status |> is_success) with
  | true -> Ok body
  | false ->
      let msg =
        Format.sprintf "Response: %s\nBody:\n%s"
          (Cohttp.Code.string_of_status status)
          body
      in
      Error (`Msg msg)
