open Lwt.Infix
module Schema = Graphql_lwt.Schema

module type S = sig
  module IO : Cohttp_lwt.S.IO

  type repo

  type server

  type response_action =
    [ `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  val schema : repo -> unit Schema.schema

  val execute_request :
    unit Schema.schema ->
    Cohttp_lwt.Request.t ->
    Cohttp_lwt.Body.t ->
    response_action Lwt.t

  val v : repo -> server
end

let of_irmin_result = function
  | Ok _ as ok -> ok
  | Error (`Msg msg) -> Error msg

module Option = struct
  let map f t = match t with None -> None | Some x -> Some (f x)
end

module Result = struct
  let ok x = Ok x
end

module type CONFIG = sig
  val remote : (?headers:Cohttp.Header.t -> string -> Irmin.remote) option

  val info :
    ?author:string -> ('a, Format.formatter, unit, Irmin.Info.f) format4 -> 'a
end

module type CUSTOM_TYPE = sig
  type t

  val schema_typ : (unit, t option) Schema.typ

  val arg_typ : t option Schema.Arg.arg_typ
end

module type CUSTOM_TYPES = sig
  type key

  type metadata

  type contents

  type hash

  type branch

  module Key : CUSTOM_TYPE with type t := key

  module Metadata : CUSTOM_TYPE with type t := metadata

  module Contents : CUSTOM_TYPE with type t := contents

  module Hash : CUSTOM_TYPE with type t := hash

  module Branch : CUSTOM_TYPE with type t := branch
end

module Default_type (T : sig
  include Irmin.Type.S

  val name : string
end) =
struct
  let schema_typ =
    let coerce t = `String (Irmin.Type.to_string T.t t) in
    Schema.scalar T.name ~coerce

  let arg_typ =
    let coerce = function
      | `String s -> of_irmin_result (Irmin.Type.of_string T.t s)
      | _ -> Error "Invalid input value"
    in
    Schema.Arg.scalar T.name ~coerce
end

module Default_types (S : Irmin.S) = struct
  module Key = Default_type (struct
    include S.Key

    let name = "Key"
  end)

  module Metadata = Default_type (struct
    include S.Metadata

    let name = "Metadata"
  end)

  module Contents = Default_type (struct
    include S.Contents

    let name = "Value"
  end)

  module Hash = Default_type (struct
    include S.Hash

    let name = "Hash"
  end)

  module Branch = Default_type (struct
    include S.Branch

    let name = "BranchName"
  end)
end

module Make_ext
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.S)
    (Types : CUSTOM_TYPES
               with type key := Store.key
                and type metadata := Store.metadata
                and type contents := Store.contents
                and type hash := Store.hash
                and type branch := Store.branch) =
struct
  module IO = Server.IO
  module Sync = Irmin.Sync (Store)
  module Graphql_server = Graphql_cohttp.Make (Schema) (IO) (Cohttp_lwt.Body)

  type repo = Store.repo

  type server = Server.t

  type txn_args = {
    author : string option;
    message : string option;
    retries : int option;
    allow_empty : bool option;
    parents : Store.Hash.t list option;
  }

  let txn_args repo input =
    match input with
    | Some input ->
        let message = match input.message with None -> "" | Some m -> m in
        let author = input.author in
        let parents =
          match input.parents with
          | Some l ->
              Lwt_list.filter_map_s
                (fun hash -> Store.Commit.of_hash repo hash)
                l
              >>= Lwt.return_some
          | None -> Lwt.return_none
        in
        parents >|= fun parents ->
        ( Config.info ?author "%s" message,
          input.retries,
          input.allow_empty,
          parents )
    | None -> Lwt.return (Config.info "", None, None, None)

  type response_action =
    [ `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  type tree_item = {
    key : Store.key;
    value : Store.contents option;
    metadata : Store.metadata option;
  }

  let mk_branch repo = function
    | Some b -> Store.of_branch repo b
    | None -> Store.master repo

  let rec concat_key a b =
    match Store.Key.decons a with
    | None -> b
    | Some (step, a_tl) -> Store.Key.cons step (concat_key a_tl b)

  module Input = struct
    let coerce_remote = function
      | `String s -> (
          match Config.remote with
          | Some remote -> Ok (remote s)
          | None -> Error "sync is not available" )
      | _ -> Error "Invalid input value"

    let remote = Schema.Arg.(scalar "Remote" ~coerce:coerce_remote)

    let key = Types.Key.arg_typ

    let commit_hash = Types.Hash.arg_typ

    let branch = Types.Branch.arg_typ

    let value = Types.Contents.arg_typ

    let metadata = Types.Metadata.arg_typ

    let info =
      Schema.Arg.(
        obj "InfoInput"
          ~fields:
            [
              arg "author" ~typ:string;
              arg "message" ~typ:string;
              arg "retries" ~typ:int;
              arg "allow_empty" ~typ:bool;
              arg "parents" ~typ:(list (non_null commit_hash));
            ]
          ~coerce:(fun author message retries allow_empty parents ->
            { author; message; retries; allow_empty; parents }))

    let item =
      Schema.Arg.(
        obj "TreeItem"
          ~fields:
            [
              arg "key" ~typ:(non_null key);
              arg "value" ~typ:value;
              arg "metadata" ~typ:metadata;
            ]
          ~coerce:(fun key value metadata -> { key; value; metadata }))

    let tree = Schema.Arg.(list (non_null item))
  end

  let rec commit =
    lazy
      Schema.(
        obj "Commit" ~fields:(fun _ ->
            [
              field "tree"
                ~typ:(non_null (Lazy.force tree))
                ~args:[]
                ~resolve:(fun _ c -> (Store.Commit.tree c, Store.Key.empty));
              field "parents"
                ~typ:(non_null (list (non_null Types.Hash.schema_typ)))
                ~args:[]
                ~resolve:(fun _ c -> Store.Commit.parents c);
              field "info"
                ~typ:(non_null Lazy.(force info))
                ~args:[]
                ~resolve:(fun _ c -> Store.Commit.info c);
              field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                ~resolve:(fun _ c -> Store.Commit.hash c);
            ]))

  and info : ('ctx, Irmin.Info.t option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "Info" ~fields:(fun _info ->
            [
              field "date" ~typ:(non_null string) ~args:[] ~resolve:(fun _ i ->
                  Irmin.Info.date i |> Int64.to_string);
              field "author" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ i -> Irmin.Info.author i);
              field "message" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ i -> Irmin.Info.message i);
            ]))

  and tree : ('ctx, (Store.tree * Store.key) option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "Tree" ~fields:(fun _ ->
            [
              field "key" ~typ:(non_null Types.Key.schema_typ) ~args:[]
                ~resolve:(fun _ (_, key) -> key);
              io_field "get"
                ~args:Arg.[ arg "key" ~typ:(non_null Input.key) ]
                ~typ:Types.Contents.schema_typ
                ~resolve:(fun _ (tree, _) key ->
                  Store.Tree.find tree key >|= Result.ok);
              io_field "get_contents"
                ~args:Arg.[ arg "key" ~typ:(non_null Input.key) ]
                ~typ:Lazy.(force contents)
                ~resolve:(fun _ (tree, tree_key) key ->
                  Store.Tree.find_all tree key
                  >|= Option.map (fun (c, m) ->
                          let key' = concat_key tree_key key in
                          (c, m, key'))
                  >|= Result.ok);
              io_field "get_tree"
                ~args:Arg.[ arg "key" ~typ:(non_null Input.key) ]
                ~typ:Lazy.(force tree)
                ~resolve:(fun _ (tree, tree_key) key ->
                  Store.Tree.find_tree tree key
                  >|= Option.map (fun tree ->
                          let tree_key' = concat_key tree_key key in
                          (tree, tree_key'))
                  >|= Result.ok);
              io_field "list_contents_recursively" ~args:[]
                ~typ:(non_null (list (non_null Lazy.(force contents))))
                ~resolve:(fun _ (tree, key) ->
                  let rec tree_list ?(acc = []) concrete_tree key =
                    match concrete_tree with
                    | `Contents (c, m) -> (c, m, key) :: acc
                    | `Tree l ->
                        List.fold_left
                          (fun acc (step, t) ->
                            let key' = Store.Key.rcons key step in
                            tree_list t key' ~acc)
                          acc l
                        |> List.rev
                  in
                  Store.Tree.to_concrete tree >|= fun concrete_tree ->
                  Ok (tree_list concrete_tree key));
              field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                ~resolve:(fun _ (tree, _) -> Store.Tree.hash tree);
              io_field "list"
                ~typ:(non_null (list (non_null node)))
                ~args:[]
                ~resolve:(fun _ (tree, tree_key) ->
                  Store.Tree.list tree Store.Key.empty
                  >>= Lwt_list.map_p (fun (step, kind) ->
                          let relative_key = Store.Key.v [ step ] in
                          let absolute_key = Store.Key.rcons tree_key step in
                          match kind with
                          | `Contents ->
                              Store.Tree.get_all tree relative_key
                              >|= fun (c, m) ->
                              Lazy.(force contents_as_node (c, m, absolute_key))
                          | `Node ->
                              Store.Tree.get_tree tree relative_key >|= fun t ->
                              Lazy.(force tree_as_node (t, absolute_key)))
                  >>= Lwt.return_ok);
            ]))

  and branch : ('ctx, (Store.t * Store.Branch.t) option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "Branch" ~fields:(fun _branch ->
            [
              field "name" ~typ:(non_null Types.Branch.schema_typ) ~args:[]
                ~resolve:(fun _ (_, b) -> b);
              io_field "head" ~args:[] ~typ:(Lazy.force commit)
                ~resolve:(fun _ (t, _) -> Store.Head.find t >>= Lwt.return_ok);
              io_field "tree" ~args:[]
                ~typ:(non_null Lazy.(force tree))
                ~resolve:(fun _ (t, _) ->
                  Store.tree t >>= fun tree ->
                  Lwt.return_ok (tree, Store.Key.empty));
              io_field "lcas"
                ~typ:(non_null (list (non_null (Lazy.force commit))))
                ~args:Arg.[ arg "commit" ~typ:(non_null Input.commit_hash) ]
                ~resolve:(fun _ (t, _) commit ->
                  Store.Commit.of_hash (Store.repo t) commit >>= function
                  | Some commit -> (
                      Store.lcas_with_commit t commit >>= function
                      | Ok lcas -> Lwt.return_ok lcas
                      | Error e ->
                          let msg = Irmin.Type.to_string Store.lca_error_t e in
                          Lwt.return_error msg )
                  | None -> Lwt.return_error "Commit not found");
            ]))

  and contents :
      ('ctx, (Store.contents * Store.metadata * Store.key) option) Schema.typ
      Lazy.t =
    lazy
      Schema.(
        obj "Contents" ~fields:(fun _contents ->
            [
              field "key" ~typ:(non_null Types.Key.schema_typ) ~args:[]
                ~resolve:(fun _ (_, _, key) -> key);
              field "metadata" ~typ:(non_null Types.Metadata.schema_typ)
                ~args:[] ~resolve:(fun _ (_, metadata, _) -> metadata);
              field "value" ~typ:(non_null Types.Contents.schema_typ) ~args:[]
                ~resolve:(fun _ (contents, _, _) -> contents);
              field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                ~resolve:(fun _ (contents, _, _) ->
                  Store.Contents.hash contents);
            ]))

  and node = Schema.union "Node"

  and tree_as_node = lazy (Schema.add_type node (Lazy.force tree))

  and contents_as_node = lazy (Schema.add_type node (Lazy.force contents))

  [@@@ocaml.warning "-5"]

  let _ = Lazy.force tree_as_node

  let _ = Lazy.force contents_as_node

  let err_write e =
    Lwt.return_error (Irmin.Type.to_string Store.write_error_t e)

  let remote s =
    match Config.remote with
    | Some _ ->
        Schema.
          [
            io_field "clone"
              ~typ:Lazy.(force commit)
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                  ]
              ~resolve:(fun _ _src branch remote ->
                mk_branch s branch >>= fun t ->
                Sync.fetch t remote >>= function
                | Ok (`Head d) -> Store.Head.set t d >|= fun () -> Ok (Some d)
                | Ok `Empty -> Lwt.return_ok None
                | Error (`Msg e) -> Lwt.return_error e);
            io_field "push" ~typ:(Lazy.force commit)
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                    arg "depth" ~typ:int;
                  ]
              ~resolve:(fun _ _src branch remote depth ->
                mk_branch s branch >>= fun t ->
                Sync.push t ?depth remote >>= function
                | Ok (`Head commit) -> Lwt.return_ok (Some commit)
                | Ok `Empty -> Lwt.return_ok None
                | Error e ->
                    let s = Fmt.to_to_string Sync.pp_push_error e in
                    Lwt.return_error s);
            io_field "pull" ~typ:(Lazy.force commit)
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                    arg "info" ~typ:Input.info;
                    arg "depth" ~typ:int;
                  ]
              ~resolve:(fun _ _src branch remote info depth ->
                mk_branch s branch >>= fun t ->
                let strategy =
                  match info with
                  | Some info ->
                      txn_args s (Some info) >|= fun (info, _, _, _) ->
                      `Merge info
                  | None -> Lwt.return `Set
                in
                strategy >>= Sync.pull ?depth t remote >>= function
                | Ok (`Head h) -> Lwt.return_ok (Some h)
                | Ok `Empty -> Lwt.return_ok None
                | Error (`Msg msg) -> Lwt.return_error msg
                | Error (`Conflict msg) -> Lwt.return_error ("conflict: " ^ msg));
          ]
    | None -> []

  let to_tree tree l =
    Lwt_list.fold_left_s
      (fun tree -> function
        | { key; value = Some v; metadata } ->
            Store.Tree.add tree ?metadata key v
        | { key; value = None; _ } -> Store.Tree.remove tree key)
      tree l

  let mutations s =
    Schema.
      [
        io_field "set" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "value" ~typ:(non_null Input.value);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k v i ->
            mk_branch s branch >>= fun t ->
            txn_args s i >>= fun (info, retries, allow_empty, parents) ->
            Store.set t ?retries ?allow_empty ?parents k v ~info >>= function
            | Ok () -> Store.Head.find t >>= Lwt.return_ok
            | Error e -> err_write e);
        io_field "set_tree" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "tree" ~typ:(non_null Input.tree);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k items i ->
            mk_branch s branch >>= fun t ->
            txn_args s i >>= fun (info, retries, allow_empty, parents) ->
            Lwt.catch
              (fun () ->
                let tree = Store.Tree.empty in
                to_tree tree items >>= fun tree ->
                Store.set_tree t ?retries ?allow_empty ?parents ~info k tree
                >>= function
                | Ok _ -> Store.Head.find t >>= Lwt.return_ok
                | Error e ->
                    Lwt.return_error
                      (Irmin.Type.to_string Store.write_error_t e))
              (function Failure e -> Lwt.return_error e | e -> raise e));
        io_field "update_tree" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "tree" ~typ:(non_null Input.tree);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k items i ->
            mk_branch s branch >>= fun t ->
            txn_args s i >>= fun (info, retries, allow_empty, parents) ->
            Lwt.catch
              (fun () ->
                Store.with_tree t ?retries ?allow_empty ?parents k ~info
                  (fun tree ->
                    let tree =
                      match tree with Some t -> t | None -> Store.Tree.empty
                    in
                    to_tree tree items >>= Lwt.return_some)
                >>= function
                | Ok _ -> Store.Head.find t >>= Lwt.return_ok
                | Error e ->
                    Lwt.return_error
                      (Irmin.Type.to_string Store.write_error_t e))
              (function Failure e -> Lwt.return_error e | e -> raise e));
        io_field "set_all" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "value" ~typ:(non_null Input.value);
                arg "metadata" ~typ:Input.metadata;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k v m i ->
            mk_branch s branch >>= fun t ->
            txn_args s i >>= fun (info, retries, allow_empty, parents) ->
            (Store.find_tree t k >>= function
             | Some tree -> Lwt.return tree
             | None -> Lwt.return Store.Tree.empty)
            >>= fun tree ->
            Store.Tree.add tree k ?metadata:m v >>= fun tree ->
            Store.set_tree t ?retries ?allow_empty ?parents k tree ~info
            >>= function
            | Ok () -> Store.Head.find t >>= Lwt.return_ok
            | Error e -> err_write e);
        io_field "test_and_set" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "test" ~typ:Input.value;
                arg "set" ~typ:Input.value;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k test set i ->
            mk_branch s branch >>= fun t ->
            txn_args s i >>= fun (info, retries, allow_empty, parents) ->
            Store.test_and_set ?retries ?allow_empty ?parents ~info t k ~test
              ~set
            >>= function
            | Ok _ -> Store.Head.find t >>= Lwt.return_ok
            | Error e -> err_write e);
        io_field "test_and_set_branch" ~typ:(non_null bool)
          ~args:
            Arg.
              [
                arg "branch" ~typ:(non_null Input.branch);
                arg "test" ~typ:Input.commit_hash;
                arg "set" ~typ:Input.commit_hash;
              ]
          ~resolve:(fun _ _src branch test set ->
            let branches = Store.Private.Repo.branch_t s in
            Store.Private.Branch.test_and_set branches branch ~test ~set
            >>= Lwt.return_ok);
        io_field "remove" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key i ->
            mk_branch s branch >>= fun t ->
            txn_args s i >>= fun (info, retries, allow_empty, parents) ->
            Store.remove t ?retries ?allow_empty ?parents key ~info >>= function
            | Ok () -> Store.Head.find t >>= Lwt.return_ok
            | Error e -> err_write e);
        io_field "merge" ~typ:Types.Hash.schema_typ
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "value" ~typ:Input.value;
                arg "old" ~typ:Input.value;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key value old info ->
            mk_branch s branch >>= fun t ->
            txn_args s info >>= fun (info, retries, allow_empty, parents) ->
            Store.merge t key ~info ?retries ?allow_empty ?parents ~old value
            >>= function
            | Ok _ -> Store.hash t key >>= Lwt.return_ok
            | Error e ->
                Lwt.return_error (Irmin.Type.to_string Store.write_error_t e));
        io_field "merge_tree" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "key" ~typ:(non_null Input.key);
                arg "value" ~typ:Input.tree;
                arg "old" ~typ:Input.tree;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key value old info ->
            mk_branch s branch >>= fun t ->
            txn_args s info >>= fun (info, retries, allow_empty, parents) ->
            ( match old with
            | Some old ->
                let tree = Store.Tree.empty in
                to_tree tree old >>= Lwt.return_some
            | None -> Lwt.return_none )
            >>= fun old ->
            ( match value with
            | Some value ->
                let tree = Store.Tree.empty in
                to_tree tree value >>= Lwt.return_some
            | None -> Lwt.return_none )
            >>= fun value ->
            Store.merge_tree t key ~info ?retries ?allow_empty ?parents ~old
              value
            >>= function
            | Ok _ -> Store.Head.find t >>= Lwt.return_ok
            | Error e ->
                Lwt.return_error (Irmin.Type.to_string Store.write_error_t e));
        io_field "merge_with_branch" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "from" ~typ:(non_null Input.branch);
                arg "info" ~typ:Input.info;
                arg "max_depth" ~typ:int;
                arg "n" ~typ:int;
              ]
          ~resolve:(fun _ _src into from i max_depth n ->
            mk_branch s into >>= fun t ->
            txn_args s i >>= fun (info, _, _, _) ->
            Store.merge_with_branch t from ~info ?max_depth ?n >>= fun _ ->
            Store.Head.find t >>= Lwt.return_ok);
        io_field "merge_with_commit" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "from" ~typ:(non_null Input.commit_hash);
                arg "info" ~typ:Input.info;
                arg "max_depth" ~typ:int;
                arg "n" ~typ:int;
              ]
          ~resolve:(fun _ _src into from i max_depth n ->
            mk_branch s into >>= fun t ->
            txn_args s i >>= fun (info, _, _, _) ->
            Store.Commit.of_hash (Store.repo t) from >>= function
            | Some from -> (
                Store.merge_with_commit t from ~info ?max_depth ?n >>= function
                | Ok _ -> Store.Head.find t >>= Lwt.return_ok
                | Error e ->
                    Lwt.return_error
                      (Irmin.Type.to_string Irmin.Merge.conflict_t e) )
            | None -> Lwt.return_error "invalid hash");
        io_field "revert" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "commit" ~typ:(non_null Input.commit_hash);
              ]
          ~resolve:(fun _ _src branch commit ->
            Store.Commit.of_hash s commit >>= function
            | Some commit ->
                mk_branch s branch >>= fun t ->
                Store.Head.set t commit >>= fun () ->
                Lwt.return_ok (Some commit)
            | None -> Lwt.return_ok None);
      ]

  let diff =
    Schema.(
      obj "Diff" ~fields:(fun _ ->
          [
            field "commit"
              ~typ:(non_null Lazy.(force commit))
              ~args:[]
              ~resolve:(fun _ctx -> function
                | `Added c | `Removed c | `Updated (_, c) -> c);
          ]))

  let map_diff diff ~added ~removed ~updated =
    match diff with
    | `Added x -> `Added (added x)
    | `Removed x -> `Removed (removed x)
    | `Updated (x, y) -> `Updated (updated x y)

  let subscriptions s =
    Schema.
      [
        subscription_field "watch" ~typ:(non_null diff)
          ~args:Arg.[ arg "branch" ~typ:Input.branch; arg "key" ~typ:Input.key ]
          ~resolve:(fun _ctx branch key ->
            mk_branch s branch >>= fun t ->
            let stream, push = Lwt_stream.create () in
            let destroy_stream watch () =
              push None;
              Lwt.ignore_result (Store.unwatch watch)
            in
            match key with
            | None ->
                Store.watch t (fun diff ->
                    push (Some diff);
                    Lwt.return_unit)
                >|= fun watch -> Ok (stream, destroy_stream watch)
            | Some key ->
                Store.watch_key t key (function diff ->
                    push
                      (Some
                         (map_diff diff
                            ~added:(fun (c, _) -> c)
                            ~removed:(fun (c, _) -> c)
                            ~updated:(fun (before, _) (after, _) ->
                              (before, after))));
                    Lwt.return_unit)
                >|= fun watch -> Ok (stream, destroy_stream watch));
      ]

  let schema s =
    let mutations = mutations s @ remote s in
    let subscriptions = subscriptions s in
    Schema.(
      schema ~mutations ~subscriptions
        [
          io_field "commit" ~typ:(Lazy.force commit)
            ~args:Arg.[ arg "hash" ~typ:(non_null Input.commit_hash) ]
            ~resolve:(fun _ _src hash ->
              Store.Commit.of_hash s hash >>= Lwt.return_ok);
          io_field "branches"
            ~typ:(non_null (list (non_null Lazy.(force branch))))
            ~args:[]
            ~resolve:(fun _ _ ->
              Store.Branch.list s
              >>= Lwt_list.map_p (fun branch ->
                      Store.of_branch s branch >|= fun store -> (store, branch))
              >|= Result.ok);
          io_field "master" ~typ:(Lazy.force branch) ~args:[]
            ~resolve:(fun _ _ ->
              Store.master s >>= fun t ->
              Lwt.return_ok (Some (t, Store.Branch.master)));
          io_field "branch" ~typ:(Lazy.force branch)
            ~args:Arg.[ arg "name" ~typ:(non_null Input.branch) ]
            ~resolve:(fun _ _ branch ->
              Store.of_branch s branch >>= fun t ->
              Lwt.return_ok (Some (t, branch)));
        ])

  let execute_request ctx req = Graphql_server.execute_request ctx () req

  let v store =
    let schema = schema store in
    let callback = Graphql_server.make_callback (fun _ctx -> ()) schema in
    Server.make_response_action ~callback ()
end

module Make (Server : Cohttp_lwt.S.Server) (Config : CONFIG) (Store : Irmin.S) =
struct
  module Types = Default_types (Store)
  include Make_ext (Server) (Config) (Store) (Types)
end
