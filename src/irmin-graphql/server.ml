(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
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
  type info

  val remote : (?headers:Cohttp.Header.t -> string -> Irmin.remote) option

  val info :
    ?author:string -> ('a, Format.formatter, unit, unit -> info) format4 -> 'a
end

module type CUSTOM_TYPE = sig
  type t

  val schema_typ : (unit, t option) Schema.typ
  val arg_typ : t option Schema.Arg.arg_typ
end

module type CUSTOM_TYPES = sig
  type path
  type metadata
  type contents
  type hash
  type branch
  type commit_key
  type node_key
  type contents_key

  module Path : CUSTOM_TYPE with type t := path
  module Metadata : CUSTOM_TYPE with type t := metadata
  module Contents : CUSTOM_TYPE with type t := contents
  module Hash : CUSTOM_TYPE with type t := hash
  module Branch : CUSTOM_TYPE with type t := branch
  module Commit_key : CUSTOM_TYPE with type t := commit_key
  module Contents_key : CUSTOM_TYPE with type t := contents_key
  module Node_key : CUSTOM_TYPE with type t := node_key
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

module Default_types (S : Irmin.Generic_key.S) = struct
  module Path = Default_type (struct
    include S.Path

    let name = "Path"
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

  module Commit_key = Default_type (struct
    type t = S.commit_key

    let t = S.commit_key_t
    let name = "CommitKey"
  end)

  module Node_key = Default_type (struct
    type t = S.node_key

    let t = S.node_key_t
    let name = "NodeKey"
  end)

  module Contents_key = Default_type (struct
    type t = S.contents_key

    let t = S.contents_key_t
    let name = "ContentsKey"
  end)
end

module Make_ext
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.Generic_key.S with type Schema.Info.t = Config.info)
    (Types : CUSTOM_TYPES
               with type path := Store.path
                and type metadata := Store.metadata
                and type contents := Store.contents
                and type hash := Store.hash
                and type branch := Store.branch
                and type commit_key := Store.commit_key
                and type node_key := Store.node_key
                and type contents_key := Store.contents_key) =
struct
  module IO = Server.IO
  module Sync = Irmin.Sync.Make (Store)
  module Graphql_server = Graphql_cohttp.Make (Schema) (IO) (Cohttp_lwt.Body)
  module Info = Store.Info

  type repo = Store.repo
  type server = Server.t
  type info = Store.info

  type txn_args = {
    author : string option;
    message : string option;
    retries : int option;
    allow_empty : bool option;
    parents : Store.commit_key list option;
  }

  let txn_args repo input =
    match input with
    | Some input ->
        let message = match input.message with None -> "" | Some m -> m in
        let author = input.author in
        let parents =
          match input.parents with
          | Some l ->
              Lwt_list.filter_map_s (Store.Commit.of_key repo) l
              >>= Lwt.return_some
          | None -> Lwt.return_none
        in
        let+ parents = parents in
        ( Config.info ?author "%s" message,
          input.retries,
          input.allow_empty,
          parents )
    | None -> Lwt.return (Config.info "", None, None, None)

  type response_action =
    [ `Expert of Cohttp.Response.t * (IO.ic -> IO.oc -> unit Lwt.t)
    | `Response of Cohttp.Response.t * Cohttp_lwt.Body.t ]

  type tree_item = {
    path : Store.path;
    value : Store.contents option;
    metadata : Store.metadata option;
  }

  let mk_branch repo = function
    | Some b -> Store.of_branch repo b
    | None -> Store.main repo

  let rec concat_path a b =
    match Store.Path.decons a with
    | None -> b
    | Some (step, a_tl) -> Store.Path.cons step (concat_path a_tl b)

  module Input = struct
    let coerce_remote = function
      | `String s -> (
          match Config.remote with
          | Some remote -> Ok (remote s)
          | None -> Error "sync is not available")
      | _ -> Error "Invalid input value"

    let remote = Schema.Arg.(scalar "Remote" ~coerce:coerce_remote)
    let path = Types.Path.arg_typ
    let hash = Types.Hash.arg_typ
    let commit_key = Types.Commit_key.arg_typ
    let branch = Types.Branch.arg_typ
    let value = Types.Contents.arg_typ
    let metadata = Types.Metadata.arg_typ
    let contents_key = Types.Contents_key.arg_typ

    let info =
      Schema.Arg.(
        obj "InfoInput"
          ~fields:
            [
              arg "author" ~typ:string;
              arg "message" ~typ:string;
              arg "retries" ~typ:int;
              arg "allow_empty" ~typ:bool;
              arg "parents" ~typ:(list (non_null commit_key));
            ]
          ~coerce:(fun author message retries allow_empty parents ->
            { author; message; retries; allow_empty; parents }))

    let item =
      Schema.Arg.(
        obj "TreeItem"
          ~fields:
            [
              arg "path" ~typ:(non_null path);
              arg "value" ~typ:value;
              arg "metadata" ~typ:metadata;
            ]
          ~coerce:(fun path value metadata -> { path; value; metadata }))

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
                ~resolve:(fun _ c -> (Store.Commit.tree c, Store.Path.empty));
              field "parents"
                ~typ:(non_null (list (non_null Types.Commit_key.schema_typ)))
                ~args:[]
                ~resolve:(fun _ c -> Store.Commit.parents c);
              field "info"
                ~typ:(non_null Lazy.(force info))
                ~args:[]
                ~resolve:(fun _ c -> Store.Commit.info c);
              field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                ~resolve:(fun _ c -> Store.Commit.hash c);
              field "key" ~typ:(non_null Types.Commit_key.schema_typ) ~args:[]
                ~resolve:(fun _ c -> Store.Commit.key c);
            ]))

  and info : ('ctx, info option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "Info" ~fields:(fun _info ->
            [
              field "date" ~typ:(non_null string) ~args:[] ~resolve:(fun _ i ->
                  Info.date i |> Int64.to_string);
              field "author" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ i -> Info.author i);
              field "message" ~typ:(non_null string) ~args:[]
                ~resolve:(fun _ i -> Info.message i);
            ]))

  and tree : ('ctx, (Store.tree * Store.path) option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "Tree" ~fields:(fun _ ->
            [
              field "path" ~typ:(non_null Types.Path.schema_typ) ~args:[]
                ~resolve:(fun _ (_, path) -> path);
              io_field "get"
                ~args:Arg.[ arg "path" ~typ:(non_null Input.path) ]
                ~typ:Types.Contents.schema_typ
                ~resolve:(fun _ (tree, _) path ->
                  Store.Tree.find tree path >|= Result.ok);
              io_field "get_contents"
                ~args:Arg.[ arg "path" ~typ:(non_null Input.path) ]
                ~typ:Lazy.(force contents)
                ~resolve:(fun _ (tree, tree_path) path ->
                  Store.Tree.find_all tree path
                  >|= Option.map (fun (c, m) ->
                          let path' = concat_path tree_path path in
                          (c, m, path'))
                  >|= Result.ok);
              io_field "get_tree"
                ~args:Arg.[ arg "path" ~typ:(non_null Input.path) ]
                ~typ:Lazy.(force tree)
                ~resolve:(fun _ (tree, tree_path) path ->
                  Store.Tree.find_tree tree path
                  >|= Option.map (fun tree ->
                          let tree_path' = concat_path tree_path path in
                          (tree, tree_path'))
                  >|= Result.ok);
              io_field "list_contents_recursively" ~args:[]
                ~typ:(non_null (list (non_null Lazy.(force contents))))
                ~resolve:(fun _ (tree, path) ->
                  let rec tree_list ?(acc = []) concrete_tree path =
                    match concrete_tree with
                    | `Contents (c, m) -> (c, m, path) :: acc
                    | `Tree l ->
                        List.fold_left
                          (fun acc (step, t) ->
                            let path' = Store.Path.rcons path step in
                            tree_list t path' ~acc)
                          acc l
                        |> List.rev
                  in
                  let+ concrete_tree = Store.Tree.to_concrete tree in
                  Ok (tree_list concrete_tree path));
              field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                ~resolve:(fun _ (tree, _) -> Store.Tree.hash tree);
              field "key" ~typ:kinded_key ~args:[] ~resolve:(fun _ (tree, _) ->
                  match Store.Tree.key tree with
                  | Some (`Contents (k, m)) ->
                      Some (Lazy.force contents_key_as_kinded_key (k, m))
                  | Some (`Node k) -> Some (Lazy.force node_key_as_kinded_key k)
                  | None -> None);
              io_field "list"
                ~typ:(non_null (list (non_null node)))
                ~args:[]
                ~resolve:(fun _ (tree, tree_path) ->
                  Store.Tree.list tree Store.Path.empty
                  >>= Lwt_list.map_s (fun (step, tree) ->
                          let absolute_path = Store.Path.rcons tree_path step in
                          match Store.Tree.destruct tree with
                          | `Contents (c, m) ->
                              let+ c = Store.Tree.Contents.force_exn c in
                              Lazy.(
                                force contents_as_node (c, m, absolute_path))
                          | _ ->
                              Lwt.return
                                Lazy.(force tree_as_node (tree, absolute_path)))
                  >|= Result.ok);
            ]))

  and branch : ('ctx, (Store.t * Store.Branch.t) option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "Branch" ~fields:(fun _branch ->
            [
              field "name" ~typ:(non_null Types.Branch.schema_typ) ~args:[]
                ~resolve:(fun _ (_, b) -> b);
              io_field "head" ~args:[] ~typ:(Lazy.force commit)
                ~resolve:(fun _ (t, _) -> Store.Head.find t >|= Result.ok);
              io_field "tree" ~args:[]
                ~typ:(non_null Lazy.(force tree))
                ~resolve:(fun _ (t, _) ->
                  let+ tree = Store.tree t in
                  Ok (tree, Store.Path.empty));
              io_field "last_modified"
                ~typ:(non_null (list (non_null (Lazy.force commit))))
                ~args:
                  Arg.
                    [
                      arg "path" ~typ:(non_null Input.path);
                      arg "depth" ~typ:int;
                      arg "n" ~typ:int;
                    ]
                ~resolve:(fun _ (t, _) path depth n ->
                  Store.last_modified ?depth ?n t path >|= Result.ok);
              io_field "lcas"
                ~typ:(non_null (list (non_null (Lazy.force commit))))
                ~args:Arg.[ arg "commit" ~typ:(non_null Input.hash) ]
                ~resolve:(fun _ (t, _) commit ->
                  Store.Commit.of_hash (Store.repo t) commit >>= function
                  | Some commit -> (
                      Store.lcas_with_commit t commit >>= function
                      | Ok lcas -> Lwt.return (Ok lcas)
                      | Error e ->
                          let msg = Irmin.Type.to_string Store.lca_error_t e in
                          Lwt.return (Error msg))
                  | None -> Lwt.return (Error "Commit not found"));
            ]))

  and contents :
      ('ctx, (Store.contents * Store.metadata * Store.path) option) Schema.typ
      Lazy.t =
    lazy
      Schema.(
        obj "Contents" ~fields:(fun _contents ->
            [
              field "path" ~typ:(non_null Types.Path.schema_typ) ~args:[]
                ~resolve:(fun _ (_, _, path) -> path);
              field "metadata" ~typ:(non_null Types.Metadata.schema_typ)
                ~args:[] ~resolve:(fun _ (_, metadata, _) -> metadata);
              field "value" ~typ:(non_null Types.Contents.schema_typ) ~args:[]
                ~resolve:(fun _ (contents, _, _) -> contents);
              field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                ~resolve:(fun _ (contents, _, _) ->
                  Store.Contents.hash contents);
            ]))

  and contents_key_item :
      ('ctx, (Store.contents_key * Store.metadata) option) Schema.typ Lazy.t =
    lazy
      Schema.(
        obj "ContentsKeyItem" ~fields:(fun _contents ->
            [
              field "metadata" ~typ:(non_null Types.Metadata.schema_typ)
                ~args:[] ~resolve:(fun _ (_, metadata) -> metadata);
              field "key" ~typ:(non_null Types.Contents_key.schema_typ) ~args:[]
                ~resolve:(fun _ (key, _) -> key);
            ]))

  and node = Schema.union "Node"
  and tree_as_node = lazy (Schema.add_type node (Lazy.force tree))
  and contents_as_node = lazy (Schema.add_type node (Lazy.force contents))
  and kinded_key = Schema.union "KindedKey"

  and node_key_as_kinded_key =
    lazy (Schema.add_type kinded_key Types.Node_key.schema_typ)

  and contents_key_as_kinded_key =
    lazy (Schema.add_type kinded_key (Lazy.force contents_key_item))

  [@@@ocaml.warning "-5"]

  let _ = Lazy.force tree_as_node
  let _ = Lazy.force contents_as_node
  let _ = Lazy.force node_key_as_kinded_key
  let _ = Lazy.force contents_key_as_kinded_key

  let err_write e =
    Lwt.return (Error (Irmin.Type.to_string Store.write_error_t e))

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
                let* t = mk_branch s branch in
                Sync.fetch t remote >>= function
                | Ok (`Head d) -> Store.Head.set t d >|= fun () -> Ok (Some d)
                | Ok `Empty -> Lwt.return (Ok None)
                | Error (`Msg e) -> Lwt.return (Error e));
            io_field "push" ~typ:(Lazy.force commit)
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                    arg "depth" ~typ:int;
                  ]
              ~resolve:(fun _ _src branch remote depth ->
                let* t = mk_branch s branch in
                Sync.push t ?depth remote >>= function
                | Ok (`Head commit) -> Lwt.return (Ok (Some commit))
                | Ok `Empty -> Lwt.return (Ok None)
                | Error e ->
                    let s = Fmt.to_to_string Sync.pp_push_error e in
                    Lwt.return (Error s));
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
                let* t = mk_branch s branch in
                let strategy =
                  match info with
                  | Some info ->
                      let+ info, _, _, _ = txn_args s (Some info) in
                      `Merge info
                  | None -> Lwt.return `Set
                in
                strategy >>= Sync.pull ?depth t remote >>= function
                | Ok (`Head h) -> Lwt.return (Ok (Some h))
                | Ok `Empty -> Lwt.return (Ok None)
                | Error (`Msg msg) -> Lwt.return (Error msg)
                | Error (`Conflict msg) ->
                    Lwt.return (Error ("conflict: " ^ msg)));
          ]
    | None -> []

  let to_tree tree l =
    Lwt_list.fold_left_s
      (fun tree -> function
        | { path; value = Some v; metadata } ->
            Store.Tree.add tree ?metadata path v
        | { path; value = None; _ } -> Store.Tree.remove tree path)
      tree l

  let mutations s =
    Schema.
      [
        io_field "set" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "value" ~typ:(non_null Input.value);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k v i ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s i in
            Store.set t ?retries ?allow_empty ?parents k v ~info >>= function
            | Ok () -> Store.Head.find t >|= Result.ok
            | Error e -> err_write e);
        io_field "set_tree" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "tree" ~typ:(non_null Input.tree);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k items i ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s i in
            Lwt.catch
              (fun () ->
                let tree = Store.Tree.empty () in
                let* tree = to_tree tree items in
                Store.set_tree t ?retries ?allow_empty ?parents ~info k tree
                >>= function
                | Ok _ -> Store.Head.find t >|= Result.ok
                | Error e -> err_write e)
              (function Failure e -> Lwt.return (Error e) | e -> raise e));
        io_field "update_tree" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "tree" ~typ:(non_null Input.tree);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k items i ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s i in
            Lwt.catch
              (fun () ->
                Store.with_tree t ?retries ?allow_empty ?parents k ~info
                  (fun tree ->
                    let tree =
                      match tree with
                      | Some t -> t
                      | None -> Store.Tree.empty ()
                    in
                    to_tree tree items >>= Lwt.return_some)
                >>= function
                | Ok _ -> Store.Head.find t >|= Result.ok
                | Error e -> err_write e)
              (function Failure e -> Lwt.return (Error e) | e -> raise e));
        io_field "set_all" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "value" ~typ:(non_null Input.value);
                arg "metadata" ~typ:Input.metadata;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k v m i ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s i in
            let* tree =
              Store.find_tree t k >>= function
              | Some tree -> Lwt.return tree
              | None -> Lwt.return (Store.Tree.empty ())
            in
            let* tree = Store.Tree.add tree k ?metadata:m v in
            Store.set_tree t ?retries ?allow_empty ?parents k tree ~info
            >>= function
            | Ok () -> Store.Head.find t >|= Result.ok
            | Error e -> err_write e);
        io_field "test_and_set" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "test" ~typ:Input.value;
                arg "set" ~typ:Input.value;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k test set i ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s i in
            Store.test_and_set ?retries ?allow_empty ?parents ~info t k ~test
              ~set
            >>= function
            | Ok _ -> Store.Head.find t >|= Result.ok
            | Error e -> err_write e);
        io_field "test_and_set_branch" ~typ:(non_null bool)
          ~args:
            Arg.
              [
                arg "branch" ~typ:(non_null Input.branch);
                arg "test" ~typ:Input.commit_key;
                arg "set" ~typ:Input.commit_key;
              ]
          ~resolve:(fun _ _src branch test set ->
            let branches = Store.Backend.Repo.branch_t s in
            Store.Backend.Branch.test_and_set branches branch ~test ~set
            >|= Result.ok);
        io_field "remove" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key i ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s i in
            Store.remove t ?retries ?allow_empty ?parents key ~info >>= function
            | Ok () -> Store.Head.find t >|= Result.ok
            | Error e -> err_write e);
        io_field "merge" ~typ:Types.Hash.schema_typ
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "value" ~typ:Input.value;
                arg "old" ~typ:Input.value;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key value old info ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s info in
            Store.merge t key ~info ?retries ?allow_empty ?parents ~old value
            >>= function
            | Ok _ -> Store.hash t key >|= Result.ok
            | Error e -> err_write e);
        io_field "merge_tree" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "value" ~typ:Input.tree;
                arg "old" ~typ:Input.tree;
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key value old info ->
            let* t = mk_branch s branch in
            let* info, retries, allow_empty, parents = txn_args s info in
            let* old =
              match old with
              | Some old ->
                  let tree = Store.Tree.empty () in
                  to_tree tree old >>= Lwt.return_some
              | None -> Lwt.return_none
            in
            let* value =
              match value with
              | Some value ->
                  let tree = Store.Tree.empty () in
                  to_tree tree value >>= Lwt.return_some
              | None -> Lwt.return_none
            in
            Store.merge_tree t key ~info ?retries ?allow_empty ?parents ~old
              value
            >>= function
            | Ok _ -> Store.Head.find t >|= Result.ok
            | Error e -> err_write e);
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
            let* t = mk_branch s into in
            let* info, _, _, _ = txn_args s i in
            let* _ = Store.merge_with_branch t from ~info ?max_depth ?n in
            Store.Head.find t >|= Result.ok);
        io_field "merge_with_commit" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "from" ~typ:(non_null Input.hash);
                arg "info" ~typ:Input.info;
                arg "max_depth" ~typ:int;
                arg "n" ~typ:int;
              ]
          ~resolve:(fun _ _src into from i max_depth n ->
            let* t = mk_branch s into in
            let* info, _, _, _ = txn_args s i in
            Store.Commit.of_hash (Store.repo t) from >>= function
            | Some from -> (
                Store.merge_with_commit t from ~info ?max_depth ?n >>= function
                | Ok _ -> Store.Head.find t >|= Result.ok
                | Error e ->
                    Lwt.return
                      (Error (Irmin.Type.to_string Irmin.Merge.conflict_t e)))
            | None -> Lwt.return (Error "invalid hash"));
        io_field "revert" ~typ:(Lazy.force commit)
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "commit" ~typ:(non_null Input.hash);
              ]
          ~resolve:(fun _ _src branch commit ->
            Store.Commit.of_hash s commit >>= function
            | Some commit ->
                let* t = mk_branch s branch in
                Store.Head.set t commit >|= fun () -> Ok (Some commit)
            | None -> Lwt.return (Ok None));
      ]

  let diff =
    Schema.(
      obj "Diff" ~fields:(fun _ ->
          [
            field "commit"
              ~typ:(non_null Lazy.(force commit))
              ~args:[]
              ~resolve:
                (fun _ctx -> function
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
          ~args:
            Arg.[ arg "branch" ~typ:Input.branch; arg "path" ~typ:Input.path ]
          ~resolve:(fun _ctx branch path ->
            let* t = mk_branch s branch in
            let stream, push = Lwt_stream.create () in
            let destroy_stream watch () =
              push None;
              Lwt.ignore_result (Store.unwatch watch)
            in
            match path with
            | None ->
                let+ watch =
                  Store.watch t (fun diff ->
                      push (Some diff);
                      Lwt.return_unit)
                in
                Ok (stream, destroy_stream watch)
            | Some path ->
                let+ watch =
                  Store.watch_key t path (function diff ->
                      push
                        (Some
                           (map_diff diff
                              ~added:(fun (c, _) -> c)
                              ~removed:(fun (c, _) -> c)
                              ~updated:(fun (before, _) (after, _) ->
                                (before, after))));
                      Lwt.return_unit)
                in
                Ok (stream, destroy_stream watch));
      ]

  let schema s =
    let mutations = mutations s @ remote s in
    let subscriptions = subscriptions s in
    Schema.(
      schema ~mutations ~subscriptions
        [
          io_field "commit" ~typ:(Lazy.force commit)
            ~args:Arg.[ arg "hash" ~typ:(non_null Input.hash) ]
            ~resolve:(fun _ _src hash ->
              Store.Commit.of_hash s hash >|= Result.ok);
          io_field "contents" ~typ:Types.Contents.schema_typ
            ~args:Arg.[ arg "hash" ~typ:(non_null Input.hash) ]
            ~resolve:(fun _ _src k -> Store.Contents.of_hash s k >|= Result.ok);
          io_field "commit_of_key" ~typ:(Lazy.force commit)
            ~args:Arg.[ arg "key" ~typ:(non_null Input.commit_key) ]
            ~resolve:(fun _ _src k -> Store.Commit.of_key s k >|= Result.ok);
          io_field "contents_of_key" ~typ:Types.Contents.schema_typ
            ~args:Arg.[ arg "key" ~typ:(non_null Input.contents_key) ]
            ~resolve:(fun _ _src k -> Store.Contents.of_key s k >|= Result.ok);
          io_field "branches"
            ~typ:(non_null (list (non_null Lazy.(force branch))))
            ~args:[]
            ~resolve:(fun _ _ ->
              Store.Branch.list s
              >>= Lwt_list.map_p (fun branch ->
                      let+ store = Store.of_branch s branch in
                      (store, branch))
              >|= Result.ok);
          io_field "main" ~typ:(Lazy.force branch) ~args:[] ~resolve:(fun _ _ ->
              let+ t = Store.main s in
              Ok (Some (t, Store.Branch.main)));
          io_field "branch" ~typ:(Lazy.force branch)
            ~args:Arg.[ arg "name" ~typ:(non_null Input.branch) ]
            ~resolve:(fun _ _ branch ->
              let+ t = Store.of_branch s branch in
              Ok (Some (t, branch)));
        ])

  let execute_request ctx req = Graphql_server.execute_request ctx () req

  let v store =
    let schema = schema store in
    let callback = Graphql_server.make_callback (fun _ctx -> ()) schema in
    Server.make_response_action ~callback ()
end

module Make
    (Server : Cohttp_lwt.S.Server)
    (Config : CONFIG)
    (Store : Irmin.Generic_key.S with type Schema.Info.t = Config.info) =
struct
  module Types = Default_types (Store)
  include Make_ext (Server) (Config) (Store) (Types)
end
