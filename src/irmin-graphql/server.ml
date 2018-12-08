open Lwt.Infix

module Schema = Graphql_lwt.Schema
module Graphql_server = Graphql_cohttp.Make(Schema)(Cohttp_lwt.Body)

module type S = sig
  type store
  type server

  val schema : store -> unit Schema.schema
  val execute_request : unit Schema.schema -> Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t
  val run_server: server -> store -> unit Lwt.t
end

let of_irmin_result = function
  | Ok _ as ok -> ok
  | Error (`Msg msg) -> Error msg

type commit_input = {
  author: string option;
  message: string option;
}

module type CONFIG = sig
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type SERVER = sig
  type conn
  type server

  val respond_string :
    ?flush:bool ->
    ?headers:Cohttp.Header.t ->
    status:Cohttp.Code.status_code ->
    body:string -> unit -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t

  val run : server -> (conn -> Cohttp_lwt.Request.t -> Cohttp_lwt.Body.t -> (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt.t) -> unit Lwt.t
end

module Make(Server : SERVER)(Config: CONFIG)(Store : Irmin.S) = struct
  module Sync = Irmin.Sync (Store)

  type tree_item = {
    key: Store.key;
    value: Store.contents option;
    metadata: Store.metadata option;
  }

  let mk_info input =
    match input with
    | Some input ->
      let message = match input.message with None -> "" | Some m -> m in
      let author = input.author in
      Config.info ?author "%s" message
    | None ->
      Config.info ""

  type store = Store.t
  type server = Server.server

  module Input = struct
    let coerce_key = function
      | `String s ->
        of_irmin_result (Irmin.Type.of_string Store.key_t s )
      | _ -> Error "invalid key encoding"

    let coerce_value = function
      | `String s ->
        of_irmin_result (Irmin.Type.of_string Store.contents_t s)
      | _ -> Error "invalid value encoding"

    let coerce_metadata = function
      | `String s ->
        of_irmin_result (Irmin.Type.of_string Store.metadata_t s)
      | _ -> Error "invalid metadata encoding"

    let coerce_step = function
      | `String s -> of_irmin_result (Irmin.Type.of_string Store.step_t s)
      | _ -> Error "invalid step encoding"

    let coerce_branch = function
      | `String s -> of_irmin_result @@ Irmin.Type.of_string Store.branch_t s
      | _ -> Error "invalid branch encoding"

    let coerce_remote = function
      | `String s ->
        (match Config.remote with
         | Some remote -> Ok (remote s)
         | None -> Error "sync is not available")
      | _ -> Error "invalid remote encoding"

    let coerce_hash = function
      | `String s ->
        of_irmin_result @@ Irmin.Type.of_string Store.Hash.t s
      | _ -> Error "invalid hash encoding"

    let key = Schema.Arg.(scalar "Key" ~coerce:coerce_key)
    let step = Schema.Arg.(scalar "Step" ~coerce:coerce_step)
    let commit_hash = Schema.Arg.(scalar "CommitHash" ~coerce:coerce_hash)
    let branch = Schema.Arg.(scalar "BranchName" ~coerce:coerce_branch)
    let remote = Schema.Arg.(scalar "Remote" ~coerce:coerce_remote)
    let value = Schema.Arg.(scalar "Value" ~coerce:coerce_value)
    let metadata = Schema.Arg.(scalar "Metadata" ~coerce:coerce_metadata)
    let info = Schema.Arg.(
        obj "InfoInput"
          ~fields:[
            arg "author" ~typ:string;
            arg "message" ~typ:string;
          ]
          ~coerce:(fun author message -> {author; message})
      )

    let item = Schema.Arg.(
        obj "TreeItem"
          ~fields:[
            arg "key" ~typ:(non_null key);
            arg "value" ~typ:value;
            arg "metadata" ~typ:metadata;
          ]
          ~coerce:(fun key value metadata -> {key; value; metadata})
      )

    let tree = Schema.Arg.(
        non_null (list (non_null item)))
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
                ~resolve:(fun _ c ->
                    Irmin.Type.to_string Store.Hash.t (Store.Commit.hash c))
              ;
            ])
    )

  and info : ('ctx, Irmin.Info.t option) Schema.typ Lazy.t = lazy Schema.(
      obj "Info"
        ~fields:(fun _info -> [
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
                ~resolve:(fun _ (_, key) -> Irmin.Type.to_string Store.key_t key)
              ;
              io_field "get"
                ~args:Arg.[arg "step" ~typ:Input.step]
                ~typ:node
                ~resolve:(fun _ (tree, key) step ->
                    Store.Tree.get_tree tree key >>= fun tree ->
                    let key =
                      match step with
                      | Some s -> Ok (Store.Key.v [s])
                      | None -> Ok Store.Key.empty
                    in
                    match key with
                    | Ok key ->
                      Lwt.return_ok (Some (tree, key))
                    | Error msg -> Lwt.return_error msg
                  )
              ;
              io_field "value"
                ~args:[]
                ~typ:string
                ~resolve:(fun _ (tree, key) ->
                    Store.Tree.find tree key >>= function
                    | Some contents ->
                      let s = Irmin.Type.to_string Store.contents_t contents in
                      Lwt.return_ok (Some s)
                    | _ -> Lwt.return_ok None
                  );
              io_field "metadata"
                ~args:[]
                ~typ:string
                ~resolve:(fun _ (tree, key) ->
                    Store.Tree.find_all tree key >>= function
                    | Some (_contents, metadata) ->
                      let s = Irmin.Type.to_string Store.metadata_t metadata in
                      Lwt.return_ok (Some s)
                    | None -> Lwt.return_ok None
                  );
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
        ~fields:(fun _branch -> [
              field "name"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ (_, b) ->
                    Irmin.Type.to_string Store.branch_t b
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
                    | Some v -> Lwt.return_ok (Some (Irmin.Type.to_string Store.contents_t v))
                    | None -> Lwt.return_ok None
                  )
              ;
              io_field "get_tree"
                ~args:Arg.[arg "key" ~typ:(non_null Input.key)]
                ~typ:(list (non_null @@ Lazy.force contents))
                ~resolve:(fun _ (s, _) key ->
                    let rec tree_list base tree key acc =
                      match tree with
                      | `Contents (_, _) ->
                        (Store.Tree.of_concrete base, key) :: acc
                      | `Tree l ->
                        List.fold_right (fun (step, t) acc -> tree_list base t (Store.Key.rcons key step) [] @ acc) l acc
                    in
                    Store.find_tree s key >>= function
                    | Some t ->
                      Store.Tree.to_concrete t >>= fun t ->
                      let l = tree_list t t Store.Key.empty [] in
                      Lwt.return_ok (Some l)
                    | None -> Lwt.return_ok None
                  )
              ;
              io_field "get_all"
                ~args:Arg.[arg "key" ~typ:(non_null Input.key)]
                ~typ:(Lazy.force contents)
                ~resolve:(fun _ (s, _) key ->
                    Store.mem_tree s key >>= function
                    | true ->
                      Store.get_tree s Store.Key.empty >>= fun tree ->
                      Lwt.return_ok (Some (tree, key))
                    | false -> Lwt.return_ok None
                  )
              ;
              io_field "lca"
                ~typ:(non_null (list (non_null (Lazy.force commit))))
                ~args:Arg.[arg "commit" ~typ:(non_null Input.commit_hash)]
                ~resolve:(fun _ (s, _) commit ->
                    Store.Commit.of_hash (Store.repo s) commit >>= function
                    | Some commit ->
                      (Store.lcas_with_commit s commit >>= function
                        | Ok lcas -> Lwt.return_ok lcas
                        | Error e ->
                          let msg = Irmin.Type.to_string Store.lca_error_t e in
                          Lwt.return_error msg)
                    | None -> Lwt.return_error "Commit not found"
                  )
              ;
            ])
    )

  and contents : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t = lazy Schema.(
      obj "Contents"
        ~fields:(fun _contents -> [
              field "key"
                ~typ:(non_null string)
                ~args:[]
                ~resolve:(fun _ (_, key) -> Irmin.Type.to_string Store.key_t key)
              ;
              io_field "metadata"
                ~typ:string
                ~args:[]
                ~resolve:(fun _ (tree, key) ->
                    Store.Tree.find_all tree key >|= function
                    | None -> Ok None
                    | Some (_, metadata) ->
                      Ok (Some (Irmin.Type.to_string Store.metadata_t metadata))
                  )
              ;
              io_field "value"
                ~typ:string
                ~args:[]
                ~resolve:(fun _ (tree, key) ->
                    Store.Tree.find tree key >|= function
                    | None -> Ok None
                    | Some contents ->
                      Ok (Some (Irmin.Type.to_string Store.contents_t contents))
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

  let err_write e =
    Lwt.return_error (Irmin.Type.to_string Store.write_error_t e)

  let remote s = match Config.remote with
    | Some _ ->
      Schema.[
        io_field "clone"
          ~typ:(non_null Lazy.(force (commit)))
          ~args:Arg.[
              arg "branch" ~typ:Input.branch;
              arg "remote" ~typ:(non_null Input.remote)
            ]
          ~resolve:(fun _ _src branch remote ->
              mk_branch (Store.repo s) branch >>= fun t ->
              Sync.fetch t remote >>= function
              | Ok d ->
                Store.Head.set s d >|= fun () ->
                Ok d
              | Error e ->
                let err = Fmt.to_to_string Sync.pp_fetch_error e in
                Lwt_result.fail err
            )
        ;
        io_field "push"
          ~typ:(non_null bool)
          ~args:Arg.[
              arg "branch" ~typ:Input.branch;
              arg "remote" ~typ:(non_null Input.remote);
            ]
          ~resolve:(fun _ _src branch remote ->
              mk_branch (Store.repo s) branch >>= fun t ->
              Sync.push t remote >>= function
              | Ok _ -> Lwt.return_ok true
              | Error e ->
                let s = Fmt.to_to_string Sync.pp_push_error e in
                Lwt.return_error s
            )
        ;
        io_field "pull"
          ~typ:(Lazy.force commit)
          ~args:Arg.[
              arg "branch" ~typ:Input.branch;
              arg "remote" ~typ:(non_null Input.remote);
              arg "info" ~typ:Input.info;
            ]
          ~resolve:(fun _ _src branch remote info ->
              mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info info in
              Sync.pull t remote (`Merge info) >>= function
              | Ok _ ->
                (Store.Head.find t >>=
                 Lwt.return_ok)
              | Error (`Msg msg) -> Lwt.return_error msg
              | Error (`Conflict msg) -> Lwt.return_error ("conflict: " ^ msg)
              | Error `Not_available -> Lwt.return_error "not available"
              | Error `No_head -> Lwt.return_error "no head"
            )
        ;
      ]
    | None -> []

  let to_tree tree l = Lwt_list.fold_left_s (fun tree ->
    function
      | {key; value = Some v; metadata} ->
          Store.Tree.add tree ?metadata key v
      | {key; value = None; _} ->
          Store.Tree.remove tree key) tree l

  let mutations s = Schema.[
    io_field "set"
      ~typ:(Lazy.force commit)
      ~args:Arg.[
          arg "branch" ~typ:Input.branch;
          arg "key" ~typ:(non_null Input.key);
          arg "value" ~typ:(non_null Input.value);
          arg "info" ~typ:Input.info;
        ]
      ~resolve:(fun _ _src branch k v i ->
          mk_branch (Store.repo s) branch >>= fun t ->
          let info = mk_info i in
          (Store.set t k v ~info >>= function
            | Ok ()   -> Store.Head.find t >>= Lwt.return_ok
            | Error e -> err_write e)
        )
    ;
    io_field "set_tree"
      ~typ:(Lazy.force commit)
      ~args:Arg.[
          arg "branch" ~typ:Input.branch;
          arg "key" ~typ:(non_null Input.key);
          arg "tree" ~typ:(Input.tree);
          arg "info" ~typ:Input.info;
        ]
      ~resolve:(fun _ _src branch k items i ->
          mk_branch (Store.repo s) branch >>= fun t ->
          let info = mk_info i in
           Lwt.catch (fun () ->
               let tree = Store.Tree.empty in
               to_tree tree items
               >>= fun tree ->
               Store.set_tree_exn t ~info k tree >>= fun () ->
               Store.Head.find t >>= Lwt.return_ok)
           (function
             | Failure e -> Lwt.return_error e
             | e -> raise e)
        )
    ;
    io_field "update_tree"
      ~typ:(Lazy.force commit)
      ~args:Arg.[
          arg "branch" ~typ:Input.branch;
          arg "key" ~typ:(non_null Input.key);
          arg "tree" ~typ:(Input.tree);
          arg "info" ~typ:Input.info;
        ]
      ~resolve:(fun _ _src branch k items i ->
          mk_branch (Store.repo s) branch >>= fun t ->
          let info = mk_info i in
           Lwt.catch (fun () ->
               Store.with_tree_exn t k ~info (fun tree ->
                   let tree = match tree with
                     | Some t -> t
                     | None -> Store.Tree.empty
                   in
                   to_tree tree items >>= Lwt.return_some)
               >>= fun () ->
               Store.Head.find t >>= Lwt.return_ok)
           (function
             | Failure e -> Lwt.return_error e
             | e -> raise e)
        )
    ;
    io_field "set_all"
      ~typ:(Lazy.force commit)
      ~args:Arg.[
          arg "branch" ~typ:Input.branch;
          arg "key" ~typ:(non_null Input.key);
          arg "value" ~typ:(non_null Input.value);
          arg "metadata" ~typ:(Input.metadata);
          arg "info" ~typ:Input.info;
        ]
      ~resolve:(fun _ _src branch k v m i ->
          mk_branch (Store.repo s) branch >>= fun t ->
          let info = mk_info i in
          (Store.find_tree t k >>= (function
             | Some tree -> Lwt.return tree
             | None -> Lwt.return Store.Tree.empty) >>= fun tree ->
           Store.Tree.add tree k ?metadata:m v >>= fun tree ->
           Store.set_tree t k tree ~info >>= function
           | Ok ()   -> Store.Head.find t >>= Lwt.return_ok
           | Error e -> err_write e)
        )
    ;
    io_field "remove"
      ~typ:(Lazy.force commit)
      ~args:Arg.[
          arg "branch" ~typ:Input.branch;
          arg "key" ~typ:(non_null Input.key);
          arg "info" ~typ:Input.info
        ]
      ~resolve:(fun _ _src branch key i ->
          mk_branch (Store.repo s) branch >>= fun t ->
          let info = mk_info i in
          Store.remove t key ~info >>= function
            | Ok () -> Store.Head.find t >>= Lwt.return_ok
            | Error e -> err_write e
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
          mk_branch (Store.repo s) into >>= fun t ->
          let info = mk_info i in
          Store.merge_with_branch t from ~info >>= fun _ ->
          Store.Head.find t >>=
          Lwt.return_ok
        )
    ;
    io_field "revert"
      ~typ:(Lazy.force commit)
      ~args:Arg.[
          arg "branch" ~typ:Input.branch;
          arg "commit" ~typ:(non_null Input.commit_hash);
        ]
      ~resolve:(fun _ _src branch commit ->
          mk_branch (Store.repo s) branch >>= fun t ->
          Store.Commit.of_hash (Store.repo s) commit >>= function
          | Some commit ->
            Store.Head.set t commit >>= fun () ->
            Lwt.return_ok (Some commit)
          | None -> Lwt.return_ok None
        )
    ;
  ]

  let schema s =
    let mutations = mutations s @ remote s in
    Schema.(schema ~mutations [
        io_field "commit"
          ~typ:(Lazy.force commit)
          ~args:Arg.[
              arg "hash" ~typ:(non_null Input.commit_hash)
            ]
          ~resolve:(fun _ _src hash ->
              Store.Commit.of_hash (Store.repo s) hash >>= Lwt.return_ok
            );
        io_field "branches"
          ~typ:(non_null (list (non_null string)))
          ~args:[]
          ~resolve:(fun _ _ ->
              Store.Branch.list (Store.repo s) >|= fun l ->
              let branches = List.map (fun b ->
                  Irmin.Type.to_string Store.branch_t b) l
              in Ok branches
            );
        io_field "master"
          ~typ:(Lazy.force branch)
          ~args:[]
          ~resolve:(fun _ _ ->
              Store.master (Store.repo s) >>= fun s ->
              Lwt.return_ok (Some (s, Store.Branch.master))
            );
        io_field "branch"
          ~typ:(Lazy.force (branch))
          ~args:Arg.[arg "name" ~typ:(non_null Input.branch)]
          ~resolve:(fun _ _ branch ->
              Store.of_branch (Store.repo s) branch >>= fun t ->
              Lwt.return_ok (Some (t, branch))
            )
      ])

  let execute_request ctx req = Graphql_server.execute_request ctx () req

  let run_server server store =
    let schema = schema store in
    let callback = Graphql_server.make_callback (fun _ctx -> ()) schema in
    Server.run server callback
end

