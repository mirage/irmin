open Lwt.Infix
open Graphql_lwt

module type S = sig
  type store

  val schema : store -> unit Schema.schema
  val start_server : ?port:int -> store -> unit Lwt.t
end

let of_irmin_result = function
  | Ok _ as ok -> ok
  | Error (`Msg msg) -> Error msg

type commit_input = {
  author: string option;
  message: string option;
}

let mk_info input =
  let default_message = "" in
  match input with
  | Some input ->
    let message = match input.message with None -> default_message | Some m -> m in
    let author = input.author in
    Irmin_unix.info ?author "%s" message
  | None ->
    Irmin_unix.info "%s" default_message

module Make(Store : Irmin.S) : S with type store = Store.t = struct
  module Sync = Irmin.Sync (Store)

  type store = Store.t

  let from_string f = function
    | `String s -> Ok (f s)
    | _ -> Error "Invalid string value"

  let from_string_err f = function
    | `String s -> f s |> of_irmin_result
    | _ -> Error "Invalid string value"



  module Input = struct
    let coerce x = Ok x

    let key = Schema.Arg.(
        scalar "Key" ~coerce
      )


    let step = Schema.Arg.(
        scalar "Step" ~coerce
      )

    let value = Schema.Arg.(
        scalar "Value" ~coerce
      )

    let commit_hash = Schema.Arg.(
        scalar "CommitHash" ~coerce
      )

    let branch = Schema.Arg.(
        scalar "BranchName" ~coerce
      )

    let remote = Schema.Arg.(
        scalar "Remote" ~coerce
      )

    let info = Schema.Arg.(
        obj "InfoInput"
          ~fields:[
            arg "author" string;
            arg "message" string;
          ]
          ~coerce:(fun author message -> {author; message})
      )

    let item = Schema.Arg.(
        obj "ItemInput"
          ~fields:[
            arg "key" (non_null string);
            arg "value" (non_null value);
          ]
          ~coerce:(fun key value -> (key, value))
      )
  end

  let rec commit = lazy Schema.(
      obj "Commit"
        ~fields:(fun commit -> [
              io_field "node"
                ~typ:(non_null (Lazy.force node))
                ~args:[]
                ~resolve:(fun _ c ->
                    Store.Commit.tree c >|= fun tree ->
                    Ok (tree, Store.Key.empty)
                  )
              ;
              io_field "parents"
                ~typ:(non_null (list (non_null commit)))
                ~args:[]
                ~resolve:(fun _ c -> Store.Commit.parents c >|= fun parents ->
                           Ok parents
                         )
              ;
              field "info"
                ~typ:(non_null Lazy.(force info))
                ~args:[]
                ~resolve:(fun _ c -> Store.Commit.info c)
              ;
              field "hash"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ c -> Fmt.to_to_string Store.Commit.Hash.pp (Store.Commit.hash c))
              ;
            ])
    )

  and info : ('ctx, Irmin.Info.t option) Schema.typ Lazy.t = lazy Schema.(
      obj "Info"
        ~fields:(fun info -> [
              field "date"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ i -> Irmin.Info.date i |> Int64.to_string)
              ;
              field "author"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ i -> Irmin.Info.author i)
              ;
              field "message"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ i -> Irmin.Info.message i)
            ])
    )

  and node : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(
      obj "Node"
        ~fields:(fun node -> [
              field "key"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ (_, key) -> Fmt.to_to_string Store.Key.pp key)
              ;
              io_field "get"
                ~args:Arg.[arg "key" ~typ:(non_null Input.step)]
                ~typ:(node)
                ~resolve:(fun _ (tree, key) step ->
                    match from_string_err Store.Key.step_of_string step with
                    | Ok step ->
                      let key = Store.Key.(rcons empty step) in
                      Lwt.return_ok (Some (tree, key))
                    | Error msg -> Lwt.return_error msg
                  )
              ;
              io_field "tree"
                ~typ:(non_null (list (non_null tree)))
                ~args:[]
                ~resolve:(fun _ (tree, key) ->
                    Store.Tree.list tree key >>= Lwt_list.map_p (fun (step, kind) ->
                        let key' = Store.Key.rcons key step in
                        match kind with
                        | `Contents -> Lwt.return (Lazy.(force contents_as_tree) (tree, key'))
                        | `Node -> Lwt.return (Lazy.(force node_as_tree) (tree, key'))
                    ) >>= Lwt.return_ok
                  )
              ;
            ])
    )

  and branch :  ('ctx, (Store.t * Store.Branch.t) option) Schema.typ Lazy.t = lazy Schema.(
      obj "Branch"
        ~fields:(fun branch -> [
              field "name"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ (_, b) ->
                    Fmt.to_to_string Store.Branch.pp b
                  )
              ;
              io_field "head"
                ~args:[]
                ~typ:(Lazy.force (commit))
                ~resolve:(fun _  (s, _) ->
                    Store.Head.find s >>= Lwt.return_ok
                  )
              ;
              io_field "get"
                ~args:Arg.[arg "key" ~typ:(non_null Input.key)]
                ~typ:(string)
                ~resolve:(fun _ (s, _) key ->
                    match from_string_err Store.Key.of_string key with
                    | Ok key ->
                      (Store.find s key >>= function
                      | Some v -> Lwt.return_ok (Some (Fmt.to_to_string Store.Contents.pp v))
                      | None -> Lwt.return_ok None)
                    | Error msg -> Lwt.return_error msg
                  )
              ;
              io_field "lca"
                ~typ:(non_null (list (non_null (Lazy.force commit))))
                ~args:Arg.[arg "commit" (non_null Input.commit_hash)]
                ~resolve:(fun _ (s, _) commit ->
                    match from_string_err Store.Commit.Hash.of_string commit with
                    | Ok commit ->
                      (Store.Commit.of_hash (Store.repo s) commit >>= function
                      | Some commit ->
                        (Store.lcas_with_commit s commit >>= function
                          | Ok lcas -> Lwt.return_ok lcas
                          | Error e ->
                            let msg = Fmt.to_to_string (Irmin.Type.pp_json Store.lca_error_t) e in
                            Lwt.return_error msg)
                      | None -> Lwt.return_error "Commit not found")
                    | Error msg -> Lwt.return_error msg
                  )
              ;
            ])
    )

  and contents : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(
      obj "Contents"
        ~fields:(fun contents -> [
              field "key"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ (_, key) -> Fmt.to_to_string Store.Key.pp key)
              ;
              io_field "value"
                ~typ:string
                ~args:[]
                ~resolve:(fun _ (tree, key) ->
                    Store.Tree.find tree key >|= function
                    | None -> Ok None
                    | Some contents ->
                      Ok (Some (Fmt.to_to_string Store.Contents.pp contents))
                  )
              ;
            ])
    )

  and tree = Schema.union "Tree"
  and node_as_tree = lazy (Schema.add_type tree (Lazy.force node))
  and contents_as_tree = lazy (Schema.add_type tree (Lazy.force contents))

  let _ = Lazy.force node_as_tree
  let _ = Lazy.force contents_as_tree


  let mk_branch repo = function
    | Some b -> Store.of_branch repo b
    | None -> Store.master repo

  let unwrap_branch branch =
    match branch with
    | Some branch ->
        (match from_string_err Store.Branch.of_string branch with
        | Ok b -> Ok (Some b)
        | Error msg -> Error msg)
    | None -> Ok None

  let mutations s = Schema.[
      io_field "clone"
        ~typ:(non_null Lazy.(force (commit)))
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "remote" ~typ:(non_null Input.remote)
          ]
        ~resolve:(fun _ _src branch remote ->
            let branch = unwrap_branch branch in
            match from_string Irmin.remote_uri remote, branch with
            | Ok remote, Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              Sync.fetch t remote >>= function
              | Ok d ->
                Store.Head.set s d >|= fun () ->
                Ok d
              | Error e ->
                let err = Fmt.to_to_string Sync.pp_fetch_error e in
                Lwt_result.fail err)
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
          )
      ;
      io_field "set"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "items" ~typ:(non_null (list (non_null Input.item)));
            arg "info" ~typ:Input.info;
          ]
        ~resolve:(fun _ _src branch items i ->
            match unwrap_branch branch with
            | Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              let items = List.map (fun (k, v) ->
                let k = of_irmin_result @@ Store.Key.of_string k in
                match k, from_string_err Store.Contents.of_string v with
                | Ok k, Ok v -> Ok (k, v)
                | Error msg, _ | _, Error msg -> Error msg) items
              in
              List.fold_right (fun x tree ->
                  match x with
                  | Ok (k, v) -> (tree >>= fun tree -> Store.Tree.add tree k v)
                  | _ -> tree
                ) items (Store.tree t) >>= fun tree ->
              Store.set_tree t Store.Key.empty tree ~info >>= fun _ ->
              Store.Head.find t >>=
              Lwt.return_ok)
            | Error msg -> Lwt.return_error msg
          )
      ;
      io_field "remove"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "keys" ~typ:(non_null (list (non_null Input.key)));
            arg "info" ~typ:Input.info
          ]
        ~resolve:(fun _ _src branch keys i ->
            match unwrap_branch branch with
            | Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              (try
                List.fold_right (fun k tree ->
                  match from_string_err Store.Key.of_string k with
                  | Ok k ->
                    tree >>= fun tree -> Store.Tree.remove tree k
                  | Error msg -> failwith msg
                ) keys (Store.tree t) >>= Lwt.return_ok
              with Failure msg -> Lwt.return_error msg) >>= function
                | Ok tree ->
                  Store.set_tree t Store.Key.empty tree ~info >>= fun _ ->
                  Store.Head.find t >>=
                  Lwt.return_ok
                | Error msg -> Lwt.return_error msg)
            | Error msg -> Lwt.return_error msg
          )
      ;
      io_field "merge"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "from" ~typ:(non_null Input.branch);
            arg "info" ~typ:Input.info;
          ]
        ~resolve:(fun _ _src into from i ->
            match unwrap_branch (Some from), unwrap_branch into with
            | Ok from, Ok into ->
              let from = match from with Some x -> x | None -> Store.Branch.master in
              mk_branch (Store.repo s) into >>= fun t ->
              let info = mk_info i in
              Store.merge_with_branch t from ~info >>= fun _ ->
              Store.Head.find t >>=
              Lwt.return_ok
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
          )
      ;
      io_field "push"
        ~typ:(string)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "remote" ~typ:(non_null Input.remote);
          ]
        ~resolve:(fun _ _src branch remote ->
            match from_string Irmin.remote_uri remote, unwrap_branch branch with
            | Ok remote, Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              Sync.push t remote >>= function
              | Ok _ -> Lwt.return_ok None
              | Error e -> Lwt.return_ok (Some (Fmt.to_to_string Sync.pp_push_error e)))
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
          )
      ;
      io_field "pull"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "remote" ~typ:(non_null Input.remote);
          ]
        ~resolve:(fun _ _src branch remote ->
            match from_string Irmin.remote_uri remote, unwrap_branch branch with
            | Ok remote, Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              Sync.pull t remote `Set >>= function
              | Ok _ ->
                (Store.Head.find t >>=
                 Lwt.return_ok)
              | Error e -> Lwt.return_ok None)
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
          )
      ;
      io_field "revert"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "commit" ~typ:(non_null Input.commit_hash);
          ]
        ~resolve:(fun _ _src branch commit->
            match from_string_err Store.Commit.Hash.of_string commit, unwrap_branch branch with
            | Ok commit, Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              Store.Commit.of_hash (Store.repo s) commit >>= function
              | Some commit ->
                Store.Head.set t commit >>= fun () ->
                Lwt.return_ok (Some commit)
              | None -> Lwt.return_ok None)
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
          )
    ]

  let schema s =
    let mutations = mutations s in
    Schema.(schema ~mutations [
        io_field "master"
          ~typ:(Lazy.force (branch))
          ~args:[]
          ~resolve:(fun _ _ ->
              Store.master (Store.repo s) >>= fun s ->
              Lwt.return_ok (Some (s, Store.Branch.master))
            );
        io_field "branch"
          ~typ:(Lazy.force (branch))
          ~args:Arg.[arg "name" (non_null Input.branch)]
          ~resolve:(fun _ _ branch ->
              let branch = from_string_err Store.Branch.of_string branch in
              match branch with
              | Ok branch ->
                Store.of_branch (Store.repo s) branch >>= fun t ->
                Lwt.return_ok (Some (t, branch))
              | Error msg -> Lwt.return_error msg
            )
      ])

  let start_server ?port s =
    Server.start ?port ~ctx:(fun () -> ()) (schema s)
end

