open Lwt.Infix
open Graphql_lwt

module type S = sig
  type store

  val schema : store -> unit Schema.schema
  val start_server : store -> unit Lwt.t
end

let of_irmin_result = function
  | Ok _ as ok -> ok
  | Error (`Msg msg) -> Error msg

type commit_input = {
  author: string option;
  message: string option;
}

module Make(Store : Irmin.S) : S with type store = Store.t = struct
  module Sync = Irmin.Sync (Store)

  type store = Store.t

  module Input = struct
    let key = Schema.Arg.(scalar "Key"
      ~coerce:(function
        | `String s -> Store.Key.of_string s |> of_irmin_result
        | _ -> Error "Key only accepts strings"
      )
    )

    let step = Schema.Arg.(scalar "Step"
      ~coerce:(function
        | `String s -> Store.Key.step_of_string s |> of_irmin_result
        | _ -> Error "Step only accepts strings"
      )
    )

    let value = Schema.Arg.(scalar "Value"
      ~coerce:(function
        | `String s -> Store.Contents.of_string s |> of_irmin_result
        | _ -> Error "Invalid Value type"
      )
    )

    let commit_hash = Schema.Arg.(scalar "CommitHash"
      ~coerce:(function
        | `String s -> Store.Commit.Hash.of_string s |> of_irmin_result
        | _ -> Error "Invalid Value type"
      )
    )

    let branch = Schema.Arg.(scalar "BranchName"
      ~coerce:(function
        | `String s -> Store.Branch.of_string s |> of_irmin_result
        | _ -> Error "BranchName only accepts strings"
      )
    )

    let remote = Schema.Arg.(scalar "Remote"
      ~coerce:(function
        | `String s -> Ok (Irmin.remote_uri s)
        | _ -> Error "Remote only accepts strings"
      )
    )
    let info = Schema.Arg.(obj "InfoInput"
      ~fields:[
        arg "author" string;
        arg "message" string;
      ]
      ~coerce:(fun author message -> {author; message}
      )
    )
  end

  let rec commit = lazy Schema.(obj "Commit"
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

  and info : ('ctx, Irmin.Info.t option) Schema.typ Lazy.t = lazy Schema.(obj "Info"
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

  and node : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(obj "Node"
    ~fields:(fun node -> [
      field "name"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _ (_, key) -> Fmt.to_to_string Store.Key.pp key)
      ;
      io_field "contents"
        ~typ:(Lazy.force contents)
        ~args:[]
        ~resolve:(fun _ (tree, key) ->
          Store.Tree.kind tree key >>= function
          | Some `Contents -> Lwt.return_ok (Some (tree, key))
          | Some `Node -> Lwt.return_ok None
          | None -> Lwt.return_error "Invalid key"
        )
      ;
      io_field "get"
        ~args:Arg.[arg "key" ~typ:(non_null Input.step)]
        ~typ:(node)
        ~resolve:(fun _ (tree, key) step ->
          let key = Store.Key.(rcons empty step) in
          Lwt.return_ok (Some (tree, key))
        )
       ;
      io_field "tree"
        ~typ:(non_null (list (non_null node)))
        ~args:[]
        ~resolve:(fun _ (tree, key) ->
              Store.Tree.list tree key >>= Lwt_list.map_s (fun (step, kind) ->
                let key' = Store.Key.rcons key step in
                Lwt.return (tree, key')
              ) >>= fun l ->
              Lwt.return_ok l
          )
        ;
    ])
  )

  and branch :  ('ctx, (Store.t * Store.Branch.t) option) Schema.typ Lazy.t = lazy Schema.(obj "Branch"
    ~fields:(fun branch -> [
      io_field "name"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _ (_, b) ->
          Lwt.return_ok @@ Fmt.to_to_string Store.Branch.pp b
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
          Store.find s key >>= function
            | Some v -> Lwt.return_ok (Some (Fmt.to_to_string Store.Contents.pp v))
            | None -> Lwt.return_ok None
        )
       ;
       io_field "lcas"
        ~typ:(non_null (list (non_null (Lazy.force commit))))
        ~args:Arg.[arg "commit" (non_null Input.commit_hash)]
        ~resolve:(fun _ (s, _) commit ->
            Store.Commit.of_hash (Store.repo s) commit >>= function
              | Some commit ->
                (Store.lcas_with_commit s commit >>= function
                  | Ok lcas -> Lwt.return_ok lcas
                  | Error e ->
                      let msg = Fmt.to_to_string (Irmin.Type.pp_json Store.lca_error_t) e in
                      Lwt.return_error msg)
              | None -> Lwt.return_error "Invalid commit"
        )
      ;
    ])
  )

  and tree : ('ctx, [`tree] option) Schema.abstract_typ = Schema.union "Tree"
  and node_as_tree = lazy (Schema.add_type tree Lazy.(force node))
  and contents_as_tree = lazy (Schema.add_type tree Lazy.(force contents))

  and contents : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(obj "Contents"
    ~fields:(fun tree -> [
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
    ])
  )

  let _ = Lazy.force node_as_tree
  let _ = Lazy.force contents_as_tree


  let schema s =
    Schema.(schema [
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
          Store.of_branch (Store.repo s)  branch >>= fun s ->
          Lwt.return_ok (Some (s, branch))
        )
    ]
    ~mutations:[
      io_field "clone"
        ~typ:(non_null Lazy.(force (commit)))
        ~args:Arg.[
          arg "remote" ~typ:(non_null Input.remote)
        ]
        ~resolve:(fun _ src remote ->
            Sync.fetch s remote >>= function
            | Ok d ->
                Store.Head.set s d >|= fun () ->
                Ok d
            | Error e ->
                let err = Fmt.to_to_string Sync.pp_fetch_error e in
                Lwt_result.fail err
        )
      ])

  let start_server s =
    Server.start ~ctx:(fun () -> ()) (schema s)
end

module Make_old(Store : Irmin.S) : S with type store = Store.t = struct
  module Sync = Irmin.Sync (Store)

  type store = Store.t

  type repo_branch = Store.repo * Store.branch

  module Input = struct
    let of_irmin_result = function
      | Ok _ as ok -> ok
      | Error (`Msg msg) -> Error msg

    let key = Schema.Arg.(scalar "KeyName"
      ~coerce:(function
        | `String s -> Store.Key.of_string s |> of_irmin_result
        | _ -> Error "Key only accepts strings"
      )
    )

    let branch = Schema.Arg.(scalar "BranchName"
      ~coerce:(function
        | `String s -> Store.Branch.of_string s |> of_irmin_result
        | _ -> Error "Branch only accepts strings"
      )
    )
  end

  let rec commit : ('ctx, Store.commit option) Schema.typ Lazy.t = lazy Schema.(obj "Commit"
    ~fields:(fun commit -> [
      io_field "tree"
        ~typ:(non_null Lazy.(force node))
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
    ])
  )

  and info : ('ctx, Irmin.Info.t option) Schema.typ Lazy.t = lazy Schema.(obj "Info"
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

  and branch : ('ctx, repo_branch option) Schema.typ Lazy.t = lazy Schema.(obj "Branch"
    ~fields:(fun branch -> [
      field "name"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _ (_, b) -> Fmt.to_to_string Store.Branch.pp b)
      ;
      io_field "tree"
        ~typ:(non_null Lazy.(force node))
        ~args:[]
        ~resolve:(fun _ (r, b) ->
            Store.of_branch r b >>= fun store ->
            Store.tree store >|= fun tree ->
            Ok (tree, Store.Key.empty)
        )
      ;
      io_field "commit"
        ~typ:(non_null Lazy.(force commit))
        ~args:[]
        ~resolve:(fun _ (r, b) -> Store.Branch.get r b >|= fun branch ->
          Ok branch
        )
      ;
    ])
  )

  and store : ('ctx, Store.t option) Schema.typ Lazy.t = lazy Schema.(obj "Store"
    ~fields:(fun store -> [
      field "repo"
        ~typ:(non_null Lazy.(force repo))
        ~args:[]
        ~resolve:(fun _ s -> Store.repo s)
      ;
      io_field "tree"
        ~typ:(non_null Lazy.(force node))
        ~args:[]
        ~resolve:(fun _ s ->
          Store.tree s >|= fun tree ->
          Ok (tree, Store.Key.empty)
        )
    ])
  )

  and repo : ('ctx, Store.repo option) Schema.typ Lazy.t = lazy Schema.(obj "Repo"
    ~fields:(fun repo -> [
      io_field "heads"
        ~typ:(non_null (list (non_null Lazy.(force commit))))
        ~args:[]
        ~resolve:(fun _ r -> Store.Repo.heads r >>= Lwt.return_ok)
      ;
      io_field "branches"
        ~typ:(non_null (list (non_null Lazy.(force branch))))
        ~args:[]
        ~resolve:(fun _ r ->
          Store.Repo.branches r >|= fun bs ->
          Ok (List.map (fun b -> r, b) bs)
        )
      ;
      io_field "of_branch"
        ~typ:(non_null Lazy.(force store))
        ~args:Arg.[arg "name" ~typ:(non_null Input.branch)]
        ~resolve:(fun _ r branch ->
            Store.of_branch r branch >>= Lwt.return_ok
        )
      ;
      io_field "of_commit"
        ~typ:Lazy.(force store)
        ~args:Arg.[arg "name" ~typ:(non_null string)]
        ~resolve:(fun _ r name ->
          match Store.Commit.of_string r name with
          | Ok commit ->
              Store.of_commit commit >|= fun s ->
              Ok (Some s)
          | Error _ -> Lwt.return (Ok None)
        )
    ])
  )

  and tree : ('ctx, [`tree] option) Schema.abstract_typ = Schema.union "Tree"
  and node_as_tree = lazy (Schema.add_type tree Lazy.(force node))
  and contents_as_tree = lazy (Schema.add_type tree Lazy.(force contents))

  and node : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(obj "Node"
    ~fields:(fun node -> [
      field "name"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _ (_, key) -> Fmt.to_to_string Store.Key.pp key)
      ;
      io_field "subnodes"
        ~typ:(list (non_null tree))
        ~args:[]
        ~resolve:(fun _ (tree, key) ->
        Store.Tree.kind tree key >>= fun kind ->
          match kind with
          | Some `Node ->
            Store.Tree.list tree key >|= fun children ->
            List.map (fun (step, kind) ->
              let key' = Store.Key.rcons key step in
              match kind with
              | `Contents -> Lazy.(force contents_as_tree) (tree, key')
              | `Node -> Lazy.(force node_as_tree) (tree, key')
            ) children
            |> fun c -> Ok (Some c)
          | _ -> Lwt.return_ok None
        )
      ;
      io_field "contents"
        ~typ:(Lazy.force contents)
        ~args:[]
        ~resolve:(fun _ (tree, key) ->
          Store.Tree.kind tree key >>= fun kind ->
          match kind with
          | Some `Contents ->
              Lwt.return_ok (Some (tree, key))
          |_ ->
              Lwt.return_ok None
        )
    ])
  )

  and contents : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(obj "Contents"
    ~fields:(fun tree -> [
      field "name"
        ~typ:(non_null string)
        ~args:[]
        ~resolve:(fun _ (_, key) -> Fmt.to_to_string Store.Key.pp key)
      ;
      io_field "contents"
        ~typ:string
        ~args:[]
        ~resolve:(fun _ (tree, key) ->
          Store.Tree.find tree key >|= function
          | None -> Ok None
          | Some contents ->
              Ok (Some (Fmt.to_to_string Store.Contents.pp contents))
        )
    ])
  )

  let _ = Lazy.force node_as_tree
  let _ = Lazy.force contents_as_tree

  let schema s =
    Schema.(schema [
      field "store"
        ~typ:(non_null Lazy.(force store))
        ~args:[]
        ~resolve:(fun _ src -> s)
    ]
    ~mutations:[
      io_field "clone"
        ~typ:(non_null Lazy.(force commit))
        ~args:Arg.[
          arg "remote" ~typ:(non_null string)
        ]
        ~resolve:(fun _ src remote ->
            let uri = Irmin.remote_uri remote in
            Sync.fetch s uri >>= function
            | Ok d ->
                Store.Head.set s d >|= fun () ->
                Ok d
            | Error e ->
                let err = Fmt.to_to_string Sync.pp_fetch_error e in
                Lwt_result.fail err
        )
    ])

  let start_server s =
    Server.start ~ctx:(fun () -> ()) (schema s)
end

