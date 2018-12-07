open Lwt.Infix

module Query = Query

module type CLIENT = sig
  type t

  val post : t -> string -> string Lwt.t
end

type error = [`Msg of string]

module type S = sig
  type t

  module Store : Irmin.S

  type commit = {
    hash: Store.Hash.t;
    info: Irmin.Info.t;
    parents: Store.Hash.t list;
  }

  val execute :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> string Lwt.t

  val execute_json :
    t
    -> ?vars:(string * Irmin.Contents.json) list
    -> ?operation:string
    -> string
    -> (Irmin.Contents.json, error) result Lwt.t

  val branches : t -> (string list, error) result Lwt.t

  val set :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.Contents.t
    -> (Store.Hash.t, error) result Lwt.t

  val set_all :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.Contents.t
    -> Store.Metadata.t
    -> (Store.Hash.t, error) result Lwt.t

  val set_tree :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.tree
    -> (Store.Hash.t, error) result Lwt.t

  val update_tree :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> Store.tree
    -> (Store.Hash.t, error) result Lwt.t

  val remove :
    t
    -> ?author:string
    -> ?message:string
    -> ?branch:Store.branch
    -> Store.Key.t
    -> (Store.Hash.t, error) result Lwt.t

  val merge :
    t
    -> ?author:string
    -> ?message:string
    -> ?into:Store.branch
    -> from:Store.branch
    -> (Store.Hash.t, error) result Lwt.t

  val get :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t, error) result Lwt.t

  val get_all :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.Contents.t * Store.Metadata.t, error) result Lwt.t

  val get_tree :
    t -> ?branch:Store.branch -> Store.Key.t -> (Store.tree, error) result Lwt.t

  val push :
    t -> ?branch:Store.branch -> string -> (bool, error) result Lwt.t

  val pull :
    t -> ?author:string -> ?message:string -> ?branch:Store.branch -> string -> (Store.Hash.t, error) result Lwt.t

  val clone :
    t -> ?branch:Store.branch -> string -> (Store.Hash.t, error) result Lwt.t

  val revert :
    t -> ?branch:Store.branch -> Store.Hash.t -> (bool, error) result Lwt.t

  val lca:
    t -> ?branch:Store.branch -> Store.Hash.t -> (commit, error) result Lwt.t

  val commit_info :
    t -> Store.Hash.t -> (commit, error) result Lwt.t

  val branch_info :
    t -> Store.branch -> (commit, error) result Lwt.t
end

let opt f = function
  | None ->
    `Null
  | Some s ->
    f s

let opt_string x = opt (fun s -> `String s) x

module Make
    (Client : CLIENT)
    (Store: Irmin.S) =
struct
  module Store = Store

  type t = Client.t

  type commit = {
    hash: Store.Hash.t;
    info: Irmin.Info.t;
    parents: Store.Hash.t list;
  }

  let unwrap = function
    | Ok x -> x
    | Error (`Msg msg) -> failwith msg

  let opt_branch x =
    let b = match x with
      | Some b -> b
      | None -> Store.Branch.master
    in`String (Irmin.Type.to_string Store.branch_t b)

  let error msg = Error (`Msg msg)
  let error_msg msg = Error msg
  let invalid_response = error "invalid response"

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
    Client.post client (Json.to_string (`O query))

  let execute_json client ?vars ?operation body =
    execute client ?vars ?operation body >|= fun res ->
    match Json.of_string res with
    | Ok j ->
      (match Json.find j ["errors"] with
       | Some (`A (h::_)) ->
         let s = Json.find_exn h ["message"] |> Json.get_string_exn in
         Error (`Msg s)
       | _ -> Ok j)
    | Error msg -> error_msg msg

  let branches client =
    execute_json client Query.branches
    >|= function
    | Ok j ->
      (match Json.find j ["data"; "branches"] with
       | Some (`A branches) ->
         (try
            Ok (List.map (function
                | `String s -> s
                | _ -> failwith "invalid branch value") branches)
          with Failure msg -> Error (`Msg msg))
       | _ -> invalid_response)
    | Error msg -> error_msg msg

  let get client ?branch key =
    let branch = opt_branch branch in
    let s = Irmin.Type.to_string Store.key_t key in
    let vars = [("branch", branch); ("key", `String s)] in
    execute client ~vars Query.get >|= fun res ->
    Irmin.Type.of_string Store.contents_t res

  let get_all client ?branch key =
    let branch = opt_branch branch in
    let s = Irmin.Type.to_string Store.key_t key in
    let vars = [("branch", branch); ("key", `String s)] in
    execute_json client ~vars Query.get_all >|= function
    | Ok j ->
        (try
          let j = Json.find_exn j ["data"; "branch"; "get_all"] in
          let value =
            Json.find_exn j ["value"]
            |> Json.get_string_exn
            |> Irmin.Type.of_string Store.contents_t
            |> unwrap
          in
          let metadata =
            Json.find_exn j ["metadata"]
            |> Json.get_string_exn
            |> Irmin.Type.of_string Store.metadata_t
            |> unwrap
          in
          Ok (value, metadata)
        with Failure msg -> error msg)
    | Error msg -> error_msg msg

  let get_tree client ?branch key =
    let branch = opt_branch branch in
    let s = Irmin.Type.to_string Store.key_t key in
    let vars = [("branch", branch); ("key", `String s)] in
    execute_json client ~vars Query.get_tree >>= function
    | Ok j ->
      (match Json.find j ["branch"; "get_tree"] with
       | Some (`A arr) ->
         let tree = Store.Tree.empty in
         Lwt_list.fold_left_s (fun tree obj ->
             let key = Json.find_exn obj ["key"] |> Json.get_string_exn |> Irmin.Type.of_string Store.key_t |> unwrap in
             let metadata = Json.find_exn obj ["metadata"] |> Json.get_string_exn |> Irmin.Type.of_string Store.metadata_t |> unwrap in
             let value = Json.find_exn obj ["value"] |> Json.get_string_exn |> Irmin.Type.of_string Store.contents_t |> unwrap in
             Store.Tree.add tree key ~metadata value) tree arr
         >>= Lwt.return_ok
       | _ -> Lwt.return invalid_response)
    | Error msg -> Lwt.return (error_msg msg)

  let mk_info ?author ?message () =
    `O [("author", opt_string author); ("message", opt_string message)]

  let decode_hash key = function
    | Ok j ->
      (match Json.find j ("data" :: key @ ["hash"]) with
       | Some (`String hash) -> Irmin.Type.of_string Store.Hash.t hash
       | _ -> invalid_response)
    | Error msg -> error_msg msg

  let set client ?author ?message ?branch key value =
    let branch = opt_branch branch in
    let key = Irmin.Type.to_string Store.key_t key in
    let value = Irmin.Type.to_string Store.contents_t value in
    let vars =
      [ ("key", `String key)
      ; ("value", `String value)
      ; ("branch", branch)
      ; ("info", mk_info ?author ?message ()) ]
    in
    execute_json client ~vars Query.set
    >|= decode_hash ["set"]

  let set_all client ?author ?message ?branch key value metadata =
    let branch = opt_branch branch in
    let key = Irmin.Type.to_string Store.key_t key in
    let value = Irmin.Type.to_string Store.contents_t value in
    let metadata = Irmin.Type.to_string Store.metadata_t metadata in
    let vars =
      [ ("key", `String key)
      ; ("value", `String value)
      ; ("metadata", `String metadata)
      ; ("branch", branch)
      ; ("info", mk_info ?author ?message ()) ]
    in
    execute_json client ~vars Query.set
    >|= decode_hash ["set"]

  let rec tree_list key = function
    | `Contents (c, m) ->
      let k = Irmin.Type.to_string Store.key_t key in
      let c = Irmin.Type.to_string Store.contents_t c in
      let m = Irmin.Type.to_string Store.metadata_t m in
      `O ["key", `String k; "value", `String c; "metadata", `String m]
    | `Tree l ->
      let l = List.fold_left (fun acc (step, i) ->
          let key = Store.Key.rcons key step in
          tree_list key i :: acc) [] l
      in `A l

  let set_or_update_tree client ?author ?message ?branch key tree query =
    let branch = opt_branch branch in
    let key' = Irmin.Type.to_string Store.key_t key in
    Store.Tree.to_concrete tree >>= fun tree ->
    let arr = tree_list key tree in
    let vars =
      [ "key", `String key'
      ; "branch", branch
      ; "info", mk_info ?author ?message ()
      ; "tree", arr ]
    in
    execute_json client ~vars query

  let set_tree client ?author ?message ?branch key tree =
    set_or_update_tree client ?author ?message ?branch key tree Query.set_tree
    >|= decode_hash ["set_tree"]

  let update_tree client ?author ?message ?branch key tree =
    set_or_update_tree client ?author ?message ?branch key tree Query.update_tree
    >|= decode_hash ["update_tree"]

  let remove client ?author ?message ?branch key =
    let branch = opt_branch branch in
    let key = Irmin.Type.to_string Store.key_t key in
    let vars =
      [ "key", `String key
      ; "branch", branch
      ; "info", mk_info ?author ?message () ]
    in
    execute_json client ~vars Query.remove >|= decode_hash ["remove"]

  let merge client ?author ?message ?into ~from =
    let into = opt_branch into in
    let from = opt_branch (Some from) in
    let vars =
      [ "branch", into
      ; "from", from
      ; "info", mk_info ?author ?message () ]
    in
    execute_json client ~vars Query.merge >|= decode_hash ["merge"]

  let push client ?branch remote =
    let branch = opt_branch branch in
    let vars =
      [ "branch", branch
      ; "remote", `String remote ]
    in
    execute_json client ~vars Query.push >|= function
    | Ok j ->
      (match Json.find j ["data"; "push"] with
       | Some (`Bool b) -> Ok b
       | _ -> invalid_response)
    | Error msg -> error_msg msg

  let pull client ?author ?message ?branch remote =
    let branch = opt_branch branch in
    let vars =
      [ "branch", branch
      ; "info", mk_info ?author ?message ()
      ; "remote", `String remote ]
    in
    execute_json client ~vars Query.pull >|= decode_hash ["pull"]

  let clone client ?branch remote =
    let branch = opt_branch branch in
    let vars =
      [ "branch", branch
      ; "remote", `String remote ]
    in
    execute_json client ~vars Query.clone >|= decode_hash ["clone"]

  let revert client ?branch commit =
    let branch = opt_branch branch in
    let commit' = Irmin.Type.to_string Store.Hash.t commit in
    let vars =
      [ "branch", branch
      ; "commit", `String commit' ]
    in
    execute_json client ~vars Query.revert >|= fun x ->
    (match decode_hash ["revert"] x with
     | Ok hash ->
       Ok (Irmin.Type.equal Store.Hash.t commit hash)
     | Error msg -> error_msg msg)

  let make_commit_info commit =
    let hash =
      Json.find_exn commit ["hash"]
      |> Json.get_string_exn
      |> Irmin.Type.of_string Store.Hash.t
      |> unwrap
    in
    let date =
      Json.find_exn commit ["info"; "date"]
      |> Json.get_string_exn
      |> Int64.of_string
    in
    let message =
      Json.find_exn commit ["info"; "message"]
      |> Json.get_string_exn
    in
    let author =
      Json.find_exn commit ["info"; "author"]
      |> Json.get_string_exn
    in
    let parents =
      match Json.find_exn commit ["info"; "parents"] with
      | `A a -> List.map (fun x -> Irmin.Type.of_string Store.Hash.t (Json.get_string_exn x) |> unwrap) a
      | _ -> failwith "Invalid parents field"
    in
    let info = Irmin.Info.v ~date ~author message in
    Ok {hash; info; parents}

  let lca client ?branch commit =
    let branch = opt_branch branch in
    let commit = Irmin.Type.to_string Store.Hash.t commit in
    let vars =
      [ "branch", branch
      ; "commit", `String commit ]
    in
    execute_json client ~vars Query.lca >|= function
    | Ok j ->
        (match Json.find j ["data"; "branch"; "lca"] with
        | Some commit ->
            (try make_commit_info commit
            with Failure msg -> error msg)
        | None -> invalid_response)
    | Error msg -> error_msg msg

  let branch_info client branch =
    let branch = opt_branch (Some branch) in
    let vars =
      [ "branch", branch ]
    in
    execute_json client ~vars Query.branch_info >|= function
    | Ok j ->
      (match Json.find j ["data"; "branch"; "head"] with
       | Some commit ->
         (try make_commit_info commit
          with Failure msg -> error msg)
       | None -> invalid_response)
    | Error msg -> error_msg msg


  let commit_info client hash =
    let commit = Irmin.Type.to_string Store.Hash.t hash in
    let vars =
      [ "commit", `String commit ]
    in
    execute_json client ~vars Query.commit_info >|= function
    | Ok j ->
      (match Json.find j ["data"; "commit"] with
       | Some commit ->
         (try make_commit_info commit
          with Failure msg -> error msg)
       | None -> invalid_response)
    | Error msg -> error_msg msg

end
