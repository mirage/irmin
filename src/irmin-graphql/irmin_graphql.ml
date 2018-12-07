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

type tree_item = {
  key: string;
  value: string option;
  metadata: string option;
}

module type STORE = sig
  include Irmin.S
  val remote: (?headers:Cohttp.Header.t -> string -> Irmin.remote) option
  val info: ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module Make(Store : STORE) : S with type store = Store.t = struct
  module Sync = Irmin.Sync (Store)

  let mk_info input =
    match input with
    | Some input ->
      let message = match input.message with None -> "" | Some m -> m in
      let author = input.author in
      Store.info ?author "%s" message
    | None ->
      Store.info ""

  type store = Store.t

  let from_string key f = function
    | `String s -> Ok (f s)
    | _ -> Error ("Invalid string input: " ^ key)

  let from_string_err key f = function
    | `String s -> f s |> of_irmin_result
    | _ -> Error ("Invalid string input: " ^ key)

  module Input = struct
    let coerce x = Ok x
    let key = Schema.Arg.(scalar "Key" ~coerce)
    let step = Schema.Arg.(scalar "Step" ~coerce)
    let commit_hash = Schema.Arg.(scalar "CommitHash" ~coerce)
    let branch = Schema.Arg.(scalar "BranchName" ~coerce)
    let remote = Schema.Arg.(scalar "Remote" ~coerce)
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
          arg "key" ~typ:(non_null string);
          arg "value" ~typ:string;
          arg "metadata" ~typ:string;
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
                ~args:Arg.[arg "key" ~typ:Input.step]
                ~typ:node
                ~resolve:(fun _ (tree, key) step ->
                  Store.Tree.get_tree tree key >>= fun tree ->
                    let key =
                      match step with
                      | Some s ->
                          let conv = (Irmin.Type.of_string Store.key_t) in
                          (match from_string_err "key" conv  s with
                          | Ok k -> Ok k
                          | Error e -> Error e)
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
                    match from_string_err "key" (Irmin.Type.of_string Store.key_t) key with
                    | Ok key ->
                      (Store.find s key >>= function
                        | Some v -> Lwt.return_ok (Some (Irmin.Type.to_string Store.contents_t v))
                        | None -> Lwt.return_ok None)
                    | Error msg -> Lwt.return_error msg
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
                    match from_string_err "key" (Irmin.Type.of_string Store.key_t) key with
                    | Ok key ->
                        (Store.find_tree s key >>= function
                          | Some t ->
                              Store.Tree.to_concrete t >>= fun t ->
                              let l = tree_list t t Store.Key.empty [] in
                              Lwt.return_ok (Some l)
                          | None -> Lwt.return_ok None)
                    | Error msg -> Lwt.return_error msg
                  )
              ;
              io_field "get_all"
                ~args:Arg.[arg "key" ~typ:(non_null Input.key)]
                ~typ:(Lazy.force contents)
                ~resolve:(fun _ (s, _) key ->
                    match from_string_err "key" (Irmin.Type.of_string Store.key_t) key with
                    | Ok key ->
                      (Store.mem_tree s key >>= function
                        | true ->
                            Store.get_tree s Store.Key.empty >>= fun tree ->
                            Lwt.return_ok (Some (tree, key))
                        | false -> Lwt.return_ok None)
                    | Error msg -> Lwt.return_error msg
                  )
              ;
              io_field "lca"
                ~typ:(non_null (list (non_null (Lazy.force commit))))
                ~args:Arg.[arg "commit" ~typ:(non_null Input.commit_hash)]
                ~resolve:(fun _ (s, _) commit ->
                    match from_string_err "commit" (Irmin.Type.of_string Store.Hash.t) commit with
                    | Ok commit ->
                      (Store.Commit.of_hash (Store.repo s) commit >>= function
                      | Some commit ->
                        (Store.lcas_with_commit s commit >>= function
                          | Ok lcas -> Lwt.return_ok lcas
                          | Error e ->
                            let msg = Irmin.Type.to_string Store.lca_error_t e in
                            Lwt.return_error msg)
                      | None -> Lwt.return_error "Commit not found")
                    | Error msg -> Lwt.return_error msg
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

  let to_branch ?(key = "branch") = function
    | Some branch ->
        (match from_string_err key (Irmin.Type.of_string Store.branch_t) branch with
        | Ok b -> Ok (Some b)
        | Error msg -> Error msg)
    | None -> Ok None

  let err_write e =
    Lwt.return_error (Irmin.Type.to_string Store.write_error_t e)

  let remote s = match Store.remote with
    | Some remote_fn ->
        Schema.[
            io_field "clone"
              ~typ:(non_null Lazy.(force (commit)))
              ~args:Arg.[
                  arg "branch" ~typ:Input.branch;
                  arg "remote" ~typ:(non_null Input.remote)
                ]
              ~resolve:(fun _ _src branch remote ->
                  let branch = to_branch branch in
                  match from_string "remote" remote_fn remote, branch with
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
            io_field "push"
              ~typ:(string)
              ~args:Arg.[
                  arg "branch" ~typ:Input.branch;
                  arg "remote" ~typ:(non_null Input.remote);
                ]
              ~resolve:(fun _ _src branch remote ->
                  match from_string "remote" remote_fn remote, to_branch branch with
                  | Ok remote, Ok branch ->
                    (mk_branch (Store.repo s) branch >>= fun t ->
                    Sync.push t remote >>= function
                    | Ok _ -> Lwt.return_ok None
                    | Error e ->
                        let s = Fmt.to_to_string Sync.pp_push_error e in
                        Lwt.return_ok @@ Some s)
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
                  match from_string "remote" remote_fn remote, to_branch branch with
                  | Ok remote, Ok branch ->
                    (mk_branch (Store.repo s) branch >>= fun t ->
                    Sync.pull t remote `Set >>= function
                    | Ok _ ->
                      (Store.Head.find t >>=
                       Lwt.return_ok)
                    | Error (`Msg msg) -> Lwt.return_error msg
                    | Error (`Conflict msg) -> Lwt.return_error ("conflict: " ^ msg)
                    | Error `Not_available -> Lwt.return_error "not available"
                    | Error `No_head -> Lwt.return_error "no head")
                  | Error msg, _ | _, Error msg -> Lwt.return_error msg
                )
            ;
        ]
      | None -> []

  let unwrap_metadata m =
    match m with
    | Some m ->
        (match Irmin.Type.of_string Store.metadata_t m with
        | Ok m -> Some m
        | Error (`Msg e) -> failwith e)
    | None -> None

  let to_tree tree l = Lwt_list.fold_left_s (fun tree -> function
    | {key; value = Some x; metadata} ->
        let k = Irmin.Type.of_string Store.key_t key in
        let v = Irmin.Type.of_string Store.contents_t x in
        let metadata = unwrap_metadata metadata in
        (match k, v with
        | Ok k, Ok v ->
          Store.Tree.add tree ?metadata k v
        | Error (`Msg e), _ | _, Error (`Msg e) -> failwith e)
    | {key; value = None; _} ->
        let k = Irmin.Type.of_string Store.key_t key in
        (match k with
        | Ok k ->
          Store.Tree.remove tree k
        | Error (`Msg e) -> failwith e)) tree l


  let mutations s = Schema.[
      io_field "set"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "key" ~typ:(non_null string);
            arg "value" ~typ:(non_null string);
            arg "info" ~typ:Input.info;
          ]
        ~resolve:(fun _ _src branch k v i ->
            match to_branch branch with
            | Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              let key = Irmin.Type.of_string Store.key_t k in
              let value = Irmin.Type.of_string Store.contents_t v in
              match key, value with
              | Ok key, Ok value ->
                (Store.set t key value ~info >>= function
                | Ok ()   -> Store.Head.find t >>= Lwt.return_ok
                | Error e -> err_write e)
              | Error (`Msg msg), _ | _, Error (`Msg msg) -> Lwt.return_error msg)
            | Error msg -> Lwt.return_error msg
          )
      ;
      io_field "set_tree"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "key" ~typ:(non_null string);
            arg "tree" ~typ:(Input.tree);
            arg "info" ~typ:Input.info;
          ]
        ~resolve:(fun _ _src branch k items i ->
          match to_branch branch with
          | Ok branch ->
              mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              let key = Irmin.Type.of_string Store.key_t k in
              (match key with
              | Ok key ->
                Lwt.catch (fun () ->
                  let tree = Store.Tree.empty in
                  to_tree tree items
                  >>= fun tree ->
                  Store.set_tree_exn t ~info key tree >>= fun () ->
                  Store.Head.find t >>= Lwt.return_ok)
                  (function
                    | Failure e -> Lwt.return_error e
                    | e -> raise e)
              | Error (`Msg msg) -> Lwt.return_error msg)
          | Error msg -> Lwt.return_error msg
        )
      ;
      io_field "update_tree"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "key" ~typ:(non_null string);
            arg "tree" ~typ:(Input.tree);
            arg "info" ~typ:Input.info;
          ]
        ~resolve:(fun _ _src branch k items i ->
          match to_branch branch with
          | Ok branch ->
              mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              let key = Irmin.Type.of_string Store.key_t k in
              (match key with
              | Ok key ->
                Lwt.catch (fun () ->
                  Store.with_tree_exn t key ~info (fun tree ->
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
              | Error (`Msg msg) -> Lwt.return_error msg)
          | Error msg -> Lwt.return_error msg
        )
      ;
      io_field "set_all"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "key" ~typ:(non_null string);
            arg "value" ~typ:(non_null string);
            arg "metadata" ~typ:(string);
            arg "info" ~typ:Input.info;
          ]
        ~resolve:(fun _ _src branch k v m i ->
            match to_branch branch with
            | Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              let key = Irmin.Type.of_string Store.key_t k in
              let value = Irmin.Type.of_string Store.contents_t v in
              let metadata = match m with
                | Some m ->
                    (match Irmin.Type.of_string Store.metadata_t m with
                    | Ok x -> Ok (Some x)
                    | Error msg -> Error msg)
                | None -> Ok None
              in
              match key, value, metadata with
              | Ok key, Ok value, Ok metadata ->
                (Store.find_tree t key >>= (function
                  | Some tree -> Lwt.return tree
                  | None -> Lwt.return Store.Tree.empty) >>= fun tree ->
                Store.Tree.add tree key ?metadata value >>= fun tree ->
                Store.set_tree t key tree ~info >>= function
                | Ok ()   -> Store.Head.find t >>= Lwt.return_ok
                | Error e -> err_write e)
              | Error (`Msg msg), _, _
              | _, Error (`Msg msg), _
              | _, _, Error (`Msg msg) -> Lwt.return_error msg)
            | Error msg -> Lwt.return_error msg
          )
      ;
      io_field "remove"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "key" ~typ:(non_null string);
            arg "info" ~typ:Input.info
          ]
        ~resolve:(fun _ _src branch key i ->
            match to_branch branch with
            | Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              let info = mk_info i in
              let key = Irmin.Type.of_string Store.key_t key in
              match key with
              | Ok key ->
                (Store.remove t key ~info >>= function
                  | Ok () -> Store.Head.find t >>= Lwt.return_ok
                  | Error e -> err_write e)
              | Error (`Msg msg) -> Lwt.return_error msg)
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
            match to_branch ~key:"from" (Some from), to_branch into with
            | Ok from, Ok into ->
              let from =
                match from with Some x -> x | None -> Store.Branch.master
              in
              mk_branch (Store.repo s) into >>= fun t ->
              let info = mk_info i in
              Store.merge_with_branch t from ~info >>= fun _ ->
              Store.Head.find t >>=
              Lwt.return_ok
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
          )
      ;
      io_field "revert"
        ~typ:(Lazy.force commit)
        ~args:Arg.[
            arg "branch" ~typ:Input.branch;
            arg "commit" ~typ:(non_null Input.commit_hash);
          ]
        ~resolve:(fun _ _src branch commit ->
            let branch = to_branch branch in
            let conv = Irmin.Type.of_string Store.Hash.t in
            match from_string_err "commit" conv commit, branch with
            | Ok commit, Ok branch ->
              (mk_branch (Store.repo s) branch >>= fun t ->
              Store.Commit.of_hash (Store.repo s) commit >>= function
              | Some commit ->
                Store.Head.set t commit >>= fun () ->
                Lwt.return_ok (Some commit)
              | None -> Lwt.return_ok None)
            | Error msg, _ | _, Error msg -> Lwt.return_error msg
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
            match from_string_err "hash" (Irmin.Type.of_string Store.Hash.t) hash with
            | Ok commit -> Store.Commit.of_hash (Store.repo s) commit >>= Lwt.return_ok
            | Error msg -> Lwt.return_error msg
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
              let conv = Irmin.Type.of_string Store.Branch.t in
              let branch = from_string_err "name" conv branch in
              match branch with
              | Ok branch ->
                Store.of_branch (Store.repo s) branch >>= fun t ->
                Lwt.return_ok (Some (t, branch))
              | Error msg -> Lwt.return_error msg

            )
      ])

  let start_server ?port s =
    Server.start ?port ~ctx:(fun _req -> ()) (schema s)
end

