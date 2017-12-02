open Lwt.Infix
open Graphql_lwt

module type S = sig
  type store 

  val schema : store -> unit Schema.schema
  val start_server : store -> unit Lwt.t
end

module Make(Store : Irmin.S) : S with type store = Store.t = struct
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

  type tree_kind = [`Contents | `Node]
  let tree_kind_values = Schema.[
    enum_value "CONTENTS" ~value:`Contents;
    enum_value "NODE" ~value:`Node;
  ]

  let tree_kind : ('ctx, tree_kind option) Schema.typ = Schema.enum "TreeKind" ~values:tree_kind_values

  let rec commit : ('ctx, Store.commit option) Schema.typ Lazy.t = lazy Schema.(obj "Commit"
    ~fields:(fun commit -> [
      io_field "tree"
        ~typ:(non_null Lazy.(force tree))
        ~args:[]
        ~resolve:(fun _ c -> Store.Commit.tree c)
      ;
      io_field "parents"
        ~typ:(non_null (list (non_null commit)))
        ~args:[]
        ~resolve:(fun _ c -> Store.Commit.parents c)
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
      io_field "commit"
        ~typ:(non_null Lazy.(force commit))
        ~args:[]
        ~resolve:(fun _ (r, b) -> Store.Branch.get r b)
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
        ~typ:(non_null Lazy.(force tree))
        ~args:[]
        ~resolve:(fun _ s -> Store.tree s)
    ])
  )

  and repo : ('ctx, Store.repo option) Schema.typ Lazy.t = lazy Schema.(obj "Repo"
    ~fields:(fun repo -> [
      io_field "heads"
        ~typ:(non_null (list (non_null Lazy.(force commit))))
        ~args:[]
        ~resolve:(fun _ r -> Store.Repo.heads r)
      ;
      io_field "branches"
        ~typ:(non_null (list (non_null Lazy.(force branch))))
        ~args:[]
        ~resolve:(fun _ r ->
          Store.Repo.branches r >|= fun bs ->
          List.map (fun b -> r, b) bs
        )
      ;
      io_field "of_branch"
        ~typ:(non_null Lazy.(force store))
        ~args:Arg.[arg "name" ~typ:(non_null Input.branch)]
        ~resolve:(fun _ r branch ->
            Store.of_branch r branch
        )
      ;
      io_field "of_commit"
        ~typ:Lazy.(force store)
        ~args:Arg.[arg "name" ~typ:(non_null string)]
        ~resolve:(fun _ r name ->
          match Store.Commit.of_string r name with
          | Ok commit ->
              Store.of_commit commit >|= fun s ->
              Some s
          | Error _ -> Lwt.return None
        )
    ])
  )

  and tree : ('ctx, Store.tree option) Schema.typ Lazy.t = lazy Schema.(obj "Tree"
    ~fields:(fun tree -> [
      io_field "kind"
        ~typ:tree_kind
        ~args:Arg.[arg' "key" ~typ:Input.key ~default:Store.Key.empty]
        ~resolve:(fun _ t key -> Store.Tree.kind t key)
      ;
      io_field "contents"
        ~typ:(non_null (list (non_null Lazy.(force step))))
        ~args:Arg.[arg' "key" ~typ:Input.key ~default:Store.Key.empty]
        ~resolve:(fun _ t key -> Store.Tree.list t key)
    ])
  )
  
  and step : ('ctx, (Store.step * tree_kind) option) Schema.typ Lazy.t = lazy Schema.(obj"Step"
    ~fields:(fun step -> [
      field "kind"
        ~typ:(non_null tree_kind)
        ~args:[]
        ~resolve:(fun _ (_, kind) -> kind)
    ])
  )

  let schema s =
    Schema.(schema [
      field "store"
        ~typ:(non_null Lazy.(force store))
        ~args:[]
        ~resolve:(fun _ src -> s)
    ])

  let start_server s =
    Server.start ~ctx:(fun () -> ()) (schema s)
end
