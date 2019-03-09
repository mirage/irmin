open Lwt.Infix

exception Graphql of string

module Make(Client: Cohttp_lwt.S.Client)(Branch: Irmin.Branch.S)(Hash: Irmin.Hash.S) = struct
  module Client = Client

  type t = {
    uri: Uri.t;
    headers: Cohttp.Header.t option;
    ctx: Client.ctx option;
    branch: Branch.t option;
  }

  let unwrap = function
    | Ok x -> x
    | Error (`Msg msg) -> raise (Graphql msg)

  let unwrap_option name = function
    | Ok (Some x) -> Ok x
    | Ok None -> Error (`Msg ("unwrap_option: " ^ name))
    | Error e -> Error e

  let opt_branch x =
    let b = match x with
      | Some b -> b
      | None -> "master"
    in `String b

  let error msg = Error (`Msg msg)
  let error_msg msg = Error msg
  let invalid_response name = print_endline ("INVALID RESPONSE: " ^ name); error ("invalid response: " ^ name)

  let v ?headers ?ctx ?branch uri = {uri; headers; ctx; branch}
  let with_branch t branch = {t with branch = branch}

  let execute client ?vars ?operation body =
    let query = [("query", `String body)] in
    let query =
      match operation with
      | Some o ->
        ("operationName", `String o) :: query
      | None ->
        query
    in
    let query =
      match vars with
      | Some v ->
        ("variables", `O v) :: query
      | None ->
        query
    in
    let body = Cohttp_lwt.Body.of_string @@ Json.to_string (`O query) in
    Client.post ?ctx:client.ctx ?headers:client.headers ~body client.uri >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body

  let execute_json client ?vars ?operation body response =
    execute client ?vars ?operation body >|= fun res ->
    Printf.printf "RESPONSE: %s\n" res;
    match Json.of_string res with
    | Ok j ->
      (match Json.find j ["errors"] with
       | Some (`A (h::_)) ->
         let s = Json.find_exn h ["message"] |> Json.get_string_exn in
         raise (Graphql s)
       | _ -> Json.find j response)
    | Error (`Msg msg) -> raise (Graphql msg)

  (*let decode_hash name ?(suffix = ["hash"]) key = function
    | Ok `Null -> Lwt.return_ok None
    | Ok j ->
      (match Json.find j ("data" :: key @ suffix) with
       | Some (`String hash) ->
           (match Irmin.Type.of_string Hash.t hash with
           | Ok x -> Lwt.return_ok (Some x)
           | Error e -> Lwt.return_error e)
       | _ -> Lwt.return @@ invalid_response name)
    | Error msg -> Lwt.return @@ error_msg msg*)
end

