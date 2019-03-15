open Lwt.Infix

exception Graphql of string

module type S = sig
  module Client: Cohttp_lwt.S.Client
  module Branch: Irmin.Branch.S

  type t
  val v: ?headers:Cohttp.Header.t -> ?ctx:Client.ctx -> ?branch:Branch.t -> Uri.t -> t
  val with_branch: t -> Branch.t option -> t
  val execute: t -> ?vars:(string * Json.t) list -> ?operation:string -> string -> string Lwt.t
  val execute_json: t -> ?vars:(string * Json.t) list -> ?operation:string -> string -> string list -> Json.t option Lwt.t
end

module Make(Client: Cohttp_lwt.S.Client)(Branch: Irmin.Branch.S)(Hash: Irmin.Hash.S) = struct
  module Client = Client
  module Branch = Branch

  type t = {
    uri: Uri.t;
    headers: Cohttp.Header.t option;
    ctx: Client.ctx option;
    branch: Branch.t option;
  }

  let unwrap ?prefix = function
    | Ok x -> x
    | Error (`Msg msg) ->
        let msg = (match prefix with
          | Some x -> x ^ ": " ^ msg
          | None -> "client: " ^ msg)
        in
        Printexc.record_backtrace true;
        raise (Graphql msg)

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
  let invalid_response name = error ("invalid response: " ^ name)

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
    let j = Json.to_string (`O query) in
    let body = Cohttp_lwt.Body.of_string j in
    Client.post ?ctx:client.ctx ?headers:client.headers ~body client.uri >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body

  let execute_json client ?vars ?operation body response =
    execute client ?vars ?operation body >|= fun res ->
    match Json.of_string res with
    | Ok j ->
      (match Json.find j ["errors"] with
       | Some (`A (h::_)) ->
         let s = Json.find_exn h ["message"] |> Json.get_string_exn in
         raise (Graphql s)
       | _ -> Json.find j response)
    | Error (`Msg msg) -> raise (Graphql msg)
end

