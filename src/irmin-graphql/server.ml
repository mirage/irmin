(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
    Cohttp.Request.t ->
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

  val remote :
    (?headers:Cohttp.Header.t -> string -> unit -> Irmin.remote) option

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
          | Some l -> Some (List.filter_map (Store.Commit.of_key repo) l)
          | None -> None
        in
        ( Config.info ?author "%s" message,
          input.retries,
          input.allow_empty,
          parents )
    | None -> (Config.info "", None, None, None)

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

  type 'ctx store_schema = {
    commit : ('ctx, Store.commit option) Schema.typ;
    info : ('ctx, info option) Schema.typ;
    tree : ('ctx, (Store.tree * Store.path) option) Schema.typ;
    branch : ('ctx, (Store.t * Store.Branch.t) option) Schema.typ;
    contents :
      ('ctx, (Store.contents * Store.metadata * Store.path) option) Schema.typ;
    contents_key_value :
      ('ctx, (Store.contents_key * Store.metadata) option) Schema.typ;
    node_key_value : ('ctx, Store.node_key option) Schema.typ;
  }

  let rec store_schema =
    lazy
      ( Schema.fix @@ fun recursive ->
        let commit =
          Schema.(
            recursive.obj "Commit" ~fields:(fun t ->
                [
                  field "tree" ~typ:(non_null t.tree) ~args:[]
                    ~resolve:(fun _ c ->
                      (Store.Commit.tree c, Store.Path.empty));
                  field "parents"
                    ~typ:
                      (non_null (list (non_null Types.Commit_key.schema_typ)))
                    ~args:[]
                    ~resolve:(fun _ c -> Store.Commit.parents c);
                  field "info" ~typ:(non_null t.info) ~args:[]
                    ~resolve:(fun _ c -> Store.Commit.info c);
                  field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                    ~resolve:(fun _ c -> Store.Commit.hash c);
                  field "key" ~typ:(non_null Types.Commit_key.schema_typ)
                    ~args:[] ~resolve:(fun _ c -> Store.Commit.key c);
                ]))
        in
        let info =
          Schema.(
            obj "Info"
              ~fields:
                [
                  field "date" ~typ:(non_null string) ~args:[]
                    ~resolve:(fun _ i -> Info.date i |> Int64.to_string);
                  field "author" ~typ:(non_null string) ~args:[]
                    ~resolve:(fun _ i -> Info.author i);
                  field "message" ~typ:(non_null string) ~args:[]
                    ~resolve:(fun _ i -> Info.message i);
                ])
        in
        let tree =
          Schema.(
            recursive.obj "Tree" ~fields:(fun t ->
                [
                  field "path" ~typ:(non_null Types.Path.schema_typ) ~args:[]
                    ~resolve:(fun _ (_, path) -> path);
                  field "get"
                    ~args:Arg.[ arg "path" ~typ:(non_null Input.path) ]
                    ~typ:Types.Contents.schema_typ
                    ~resolve:(fun _ (tree, _) path -> Store.Tree.find tree path);
                  field "get_contents"
                    ~args:Arg.[ arg "path" ~typ:(non_null Input.path) ]
                    ~typ:t.contents
                    ~resolve:(fun _ (tree, tree_path) path ->
                      Store.Tree.find_all tree path
                      |> Option.map (fun (c, m) ->
                             let path' = concat_path tree_path path in
                             (c, m, path')));
                  field "get_tree"
                    ~args:Arg.[ arg "path" ~typ:(non_null Input.path) ]
                    ~typ:t.tree
                    ~resolve:(fun _ (tree, tree_path) path ->
                      Store.Tree.find_tree tree path
                      |> Option.map (fun tree ->
                             let tree_path' = concat_path tree_path path in
                             (tree, tree_path')));
                  field "list_contents_recursively" ~args:[]
                    ~typ:(non_null (list (non_null t.contents)))
                    ~resolve:(fun _ (tree, path) ->
                      let rec tree_list ?(acc = []) tree path =
                        match Store.Tree.destruct tree with
                        | `Contents (c, m) ->
                            let c = Store.Tree.Contents.force_exn c in
                            (c, m, path) :: acc
                        | `Node _ ->
                            let l = Store.Tree.list tree Store.Path.empty in
                            List.fold_left
                              (fun acc (step, t) ->
                                let path' = Store.Path.rcons path step in
                                tree_list t path' ~acc)
                              acc l
                            |> List.rev
                      in
                      tree_list tree path);
                  field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                    ~resolve:(fun _ (tree, _) -> Store.Tree.hash tree);
                  field "key" ~typ:kinded_key ~args:[]
                    ~resolve:(fun _ (tree, _) ->
                      match Store.Tree.key tree with
                      | Some (`Contents (k, m)) ->
                          let f = Lazy.force contents_key_as_kinded_key in
                          Some (f (k, m))
                      | Some (`Node k) ->
                          let f = Lazy.force node_key_as_kinded_key in
                          Some (f k)
                      | None -> None);
                  field "list"
                    ~typ:(non_null (list (non_null node)))
                    ~args:[]
                    ~resolve:(fun _ (tree, tree_path) ->
                      Store.Tree.list tree Store.Path.empty
                      |> List.map (fun (step, tree) ->
                             let absolute_path =
                               Store.Path.rcons tree_path step
                             in
                             match Store.Tree.destruct tree with
                             | `Contents (c, m) ->
                                 let c = Store.Tree.Contents.force_exn c in
                                 let f = Lazy.force contents_as_node in
                                 f (c, m, absolute_path)
                             | _ ->
                                 let f = Lazy.force tree_as_node in
                                 f (tree, absolute_path)));
                ]))
        in
        let branch =
          Schema.(
            recursive.obj "Branch" ~fields:(fun t ->
                [
                  field "name" ~typ:(non_null Types.Branch.schema_typ) ~args:[]
                    ~resolve:(fun _ (_, b) -> b);
                  field "head" ~args:[] ~typ:t.commit ~resolve:(fun _ (t, _) ->
                      Store.Head.find t);
                  field "tree" ~args:[] ~typ:(non_null t.tree)
                    ~resolve:(fun _ (t, _) ->
                      let tree = Store.tree t in
                      (tree, Store.Path.empty));
                  field "last_modified"
                    ~typ:(non_null (list (non_null t.commit)))
                    ~args:
                      Arg.
                        [
                          arg "path" ~typ:(non_null Input.path);
                          arg "depth" ~typ:int;
                          arg "n" ~typ:int;
                        ]
                    ~resolve:(fun _ (t, _) path depth n ->
                      Store.last_modified ?depth ?n t path);
                  io_field "lcas"
                    ~typ:(non_null (list (non_null t.commit)))
                    ~args:Arg.[ arg "commit" ~typ:(non_null Input.hash) ]
                    ~resolve:(fun _ (t, _) commit ->
                      Lwt_eio.run_eio @@ fun () ->
                      match Store.Commit.of_hash (Store.repo t) commit with
                      | Some commit -> (
                          match Store.lcas_with_commit t commit with
                          | Ok lcas -> Ok lcas
                          | Error e ->
                              let msg =
                                Irmin.Type.to_string Store.lca_error_t e
                              in
                              Error msg)
                      | None -> Error "Commit not found");
                ]))
        in
        let contents =
          Schema.(
            obj "Contents"
              ~fields:
                [
                  field "path" ~typ:(non_null Types.Path.schema_typ) ~args:[]
                    ~resolve:(fun _ (_, _, path) -> path);
                  field "metadata" ~typ:(non_null Types.Metadata.schema_typ)
                    ~args:[] ~resolve:(fun _ (_, metadata, _) -> metadata);
                  field "value" ~typ:(non_null Types.Contents.schema_typ)
                    ~args:[] ~resolve:(fun _ (contents, _, _) -> contents);
                  field "hash" ~typ:(non_null Types.Hash.schema_typ) ~args:[]
                    ~resolve:(fun _ (contents, _, _) ->
                      Store.Contents.hash contents);
                ])
        in

        let contents_key_value =
          Schema.(
            obj "ContentsKeyValue"
              ~fields:
                [
                  field "metadata" ~typ:(non_null Types.Metadata.schema_typ)
                    ~args:[] ~resolve:(fun _ (_, metadata) -> metadata);
                  field "contents" ~typ:(non_null Types.Contents_key.schema_typ)
                    ~args:[] ~resolve:(fun _ (key, _) -> key);
                ])
        in
        let node_key_value =
          Schema.(
            obj "NodeKeyValue"
              ~fields:
                [
                  field "node" ~typ:(non_null Types.Node_key.schema_typ)
                    ~args:[] ~resolve:(fun _ x -> x);
                ])
        in
        {
          commit;
          info;
          tree;
          branch;
          contents;
          contents_key_value;
          node_key_value;
        } )

  and kinded_key = Schema.union "KindedKey"
  and node = Schema.union "Node"
  and tree_as_node = lazy (Schema.add_type node (Lazy.force store_schema).tree)

  and contents_as_node =
    lazy (Schema.add_type node (Lazy.force store_schema).contents)

  and node_key_as_kinded_key =
    lazy (Schema.add_type kinded_key (Lazy.force store_schema).node_key_value)

  and contents_key_as_kinded_key =
    lazy
      (Schema.add_type kinded_key (Lazy.force store_schema).contents_key_value)

  [@@@ocaml.warning "-5"]

  let _ = Lazy.force tree_as_node
  let _ = Lazy.force contents_as_node
  let _ = Lazy.force node_key_as_kinded_key
  let _ = Lazy.force contents_key_as_kinded_key
  let store_schema = Lazy.force store_schema
  let err_write e = Error (Irmin.Type.to_string Store.write_error_t e)

  let remote s =
    match Config.remote with
    | Some _ ->
        Schema.
          [
            io_field "clone" ~typ:store_schema.commit
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                  ]
              ~resolve:(fun _ _src branch remote ->
                Lwt_eio.run_eio @@ fun () ->
                let t = mk_branch s branch in
                let remote = remote () in
                match Sync.fetch t remote with
                | Ok (`Head d) -> Store.Head.set t d |> fun () -> Ok (Some d)
                | Ok `Empty -> Ok None
                | Error (`Msg e) -> Error e);
            io_field "push" ~typ:store_schema.commit
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                    arg "depth" ~typ:int;
                  ]
              ~resolve:(fun _ _src branch remote depth ->
                Lwt_eio.run_eio @@ fun () ->
                let t = mk_branch s branch in
                let remote = remote () in
                match Sync.push t ?depth remote with
                | Ok (`Head commit) -> Ok (Some commit)
                | Ok `Empty -> Ok None
                | Error e ->
                    let s = Fmt.to_to_string Sync.pp_push_error e in
                    Error s);
            io_field "pull" ~typ:store_schema.commit
              ~args:
                Arg.
                  [
                    arg "branch" ~typ:Input.branch;
                    arg "remote" ~typ:(non_null Input.remote);
                    arg "info" ~typ:Input.info;
                    arg "depth" ~typ:int;
                  ]
              ~resolve:(fun _ _src branch remote info depth ->
                Lwt_eio.run_eio @@ fun () ->
                let t = mk_branch s branch in
                let strategy =
                  match info with
                  | Some info ->
                      let info, _, _, _ = txn_args s (Some info) in
                      `Merge info
                  | None -> `Set
                in
                let remote = remote () in
                match Sync.pull ?depth t remote strategy with
                | Ok (`Head h) -> Ok (Some h)
                | Ok `Empty -> Ok None
                | Error (`Msg msg) -> Error msg
                | Error (`Conflict msg) -> Error ("conflict: " ^ msg));
          ]
    | None -> []

  let to_tree tree l =
    List.fold_left
      (fun tree -> function
        | { path; value = Some v; metadata } ->
            Store.Tree.add tree ?metadata path v
        | { path; value = None; _ } -> Store.Tree.remove tree path)
      tree l

  let mutations s =
    Schema.
      [
        io_field "set" ~typ:store_schema.commit
          ~doc:"Associate contents with the given path"
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "value" ~typ:(non_null Input.value);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k v i ->
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            match Store.set t ?retries ?allow_empty ?parents k v ~info with
            | Ok () -> Store.Head.find t |> Result.ok
            | Error e -> err_write e);
        io_field "set_tree" ~typ:store_schema.commit
          ~doc:"Set the tree at \"path\""
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "tree" ~typ:(non_null Input.tree);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k items i ->
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            try
              let tree = Store.Tree.empty () in
              let tree = to_tree tree items in
              match
                Store.set_tree t ?retries ?allow_empty ?parents ~info k tree
              with
              | Ok _ -> Store.Head.find t |> Result.ok
              | Error e -> err_write e
            with Failure e -> Error e);
        io_field "update_tree" ~typ:store_schema.commit
          ~doc:"Add/remove items from the tree specified by \"path\""
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "tree" ~typ:(non_null Input.tree);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch k items i ->
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            try
              match
                Store.with_tree t ?retries ?allow_empty ?parents k ~info
                  (fun tree ->
                    let tree =
                      match tree with
                      | Some t -> t
                      | None -> Store.Tree.empty ()
                    in
                    Some (to_tree tree items))
              with
              | Ok _ -> Store.Head.find t |> Result.ok
              | Error e -> err_write e
            with Failure e -> Error e);
        io_field "set_all" ~typ:store_schema.commit
          ~doc:"Set contents and metadata"
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            let tree =
              match Store.find_tree t k with
              | Some tree -> tree
              | None -> Store.Tree.empty ()
            in
            let tree = Store.Tree.add tree k ?metadata:m v in
            match
              Store.set_tree t ?retries ?allow_empty ?parents k tree ~info
            with
            | Ok () -> Store.Head.find t |> Result.ok
            | Error e -> err_write e);
        io_field "test_and_set" ~typ:store_schema.commit
          ~doc:
            "Update a value with \"set\" argument if \"test\" matches the \
             current value"
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            match
              Store.test_and_set ?retries ?allow_empty ?parents ~info t k ~test
                ~set
            with
            | Ok _ -> Store.Head.find t |> Result.ok
            | Error e -> err_write e);
        io_field "test_set_and_get" ~typ:store_schema.commit
          ~doc:
            "Update a value with \"set\" argument if \"test\" matches the \
             current value. The commit returned is gauranteed to be that of a \
             successful update to the store."
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            match
              Store.test_set_and_get ?retries ?allow_empty ?parents ~info t k
                ~test ~set
            with
            | Ok _ as v -> v
            | Error e -> err_write e);
        io_field "test_and_set_branch" ~typ:(non_null bool)
          ~doc:
            "Update a branch with \"set\" argument if \"test\" matches the \
             current value"
          ~args:
            Arg.
              [
                arg "branch" ~typ:(non_null Input.branch);
                arg "test" ~typ:Input.commit_key;
                arg "set" ~typ:Input.commit_key;
              ]
          ~resolve:(fun _ _src branch test set ->
            Lwt_eio.run_eio @@ fun () ->
            let branches = Store.Backend.Repo.branch_t s in
            Ok (Store.Backend.Branch.test_and_set branches branch ~test ~set));
        io_field "remove" ~typ:store_schema.commit
          ~doc:"Remove a path from the store"
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "path" ~typ:(non_null Input.path);
                arg "info" ~typ:Input.info;
              ]
          ~resolve:(fun _ _src branch key i ->
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s i in
            match Store.remove t ?retries ?allow_empty ?parents key ~info with
            | Ok () -> Store.Head.find t |> Result.ok
            | Error e -> err_write e);
        io_field "merge" ~typ:Types.Hash.schema_typ
          ~doc:"Merge the current value at the given path with another value"
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s info in
            match
              Store.merge t key ~info ?retries ?allow_empty ?parents ~old value
            with
            | Ok _ -> Store.hash t key |> Result.ok
            | Error e -> err_write e);
        io_field "merge_tree" ~typ:store_schema.commit
          ~doc:"Merge a branch with a tree"
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let info, retries, allow_empty, parents = txn_args s info in
            let old =
              match old with
              | Some old ->
                  let tree = Store.Tree.empty () in
                  Some (to_tree tree old)
              | None -> None
            in
            let value =
              match value with
              | Some value ->
                  let tree = Store.Tree.empty () in
                  Some (to_tree tree value)
              | None -> None
            in
            match
              Store.merge_tree t key ~info ?retries ?allow_empty ?parents ~old
                value
            with
            | Ok _ -> Store.Head.find t |> Result.ok
            | Error e -> err_write e);
        io_field "merge_with_branch" ~typ:store_schema.commit
          ~doc:"Merge a branch with another branch"
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s into in
            let info, _, _, _ = txn_args s i in
            let _ = Store.merge_with_branch t from ~info ?max_depth ?n in
            Ok (Store.Head.find t));
        io_field "merge_with_commit"
          ~doc:"Merge a branch with a specific commit" ~typ:store_schema.commit
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
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s into in
            let info, _, _, _ = txn_args s i in
            match Store.Commit.of_hash (Store.repo t) from with
            | Some from -> (
                match Store.merge_with_commit t from ~info ?max_depth ?n with
                | Ok _ -> Store.Head.find t |> Result.ok
                | Error e ->
                    Error (Irmin.Type.to_string Irmin.Merge.conflict_t e))
            | None -> Error "invalid hash");
        io_field "revert" ~doc:"Revert to a previous commit"
          ~typ:store_schema.commit
          ~args:
            Arg.
              [
                arg "branch" ~typ:Input.branch;
                arg "commit" ~typ:(non_null Input.hash);
              ]
          ~resolve:(fun _ _src branch commit ->
            Lwt_eio.run_eio @@ fun () ->
            match Store.Commit.of_hash s commit with
            | Some commit ->
                let t = mk_branch s branch in
                Store.Head.set t commit;
                Ok (Some commit)
            | None -> Ok None);
      ]

  let diff =
    Schema.(
      obj "Diff"
        ~fields:
          [
            field "commit" ~typ:(non_null store_schema.commit) ~args:[]
              ~resolve:(fun _ctx -> function
              | `Added c | `Removed c | `Updated (_, c) -> c);
          ])

  let map_diff diff ~added ~removed ~updated =
    match diff with
    | `Added x -> `Added (added x)
    | `Removed x -> `Removed (removed x)
    | `Updated (x, y) -> `Updated (updated x y)

  let subscriptions s =
    Schema.
      [
        subscription_field "watch" ~typ:(non_null diff)
          ~doc:"Watch for changes to a branch"
          ~args:
            Arg.[ arg "branch" ~typ:Input.branch; arg "path" ~typ:Input.path ]
          ~resolve:(fun _ctx branch path ->
            Lwt_eio.run_eio @@ fun () ->
            let t = mk_branch s branch in
            let stream, push = Lwt_stream.create () in
            let destroy_stream watch () =
              push None;
              Store.unwatch watch
            in
            match path with
            | None ->
                let watch = Store.watch t (fun diff -> push (Some diff)) in
                Ok (stream, destroy_stream watch)
            | Some path ->
                let watch =
                  Store.watch_key t path (function diff ->
                      push
                        (Some
                           (map_diff diff
                              ~added:(fun (c, _) -> c)
                              ~removed:(fun (c, _) -> c)
                              ~updated:(fun (before, _) (after, _) ->
                                (before, after)))))
                in
                Ok (stream, destroy_stream watch));
      ]

  let schema s =
    let mutations = mutations s @ remote s in
    let subscriptions = subscriptions s in
    Schema.(
      schema ~mutations ~subscriptions
        [
          io_field "commit" ~doc:"Find commit by hash" ~typ:store_schema.commit
            ~args:Arg.[ arg "hash" ~typ:(non_null Input.hash) ]
            ~resolve:(fun _ _src hash ->
              Lwt_eio.run_eio @@ fun () ->
              Store.Commit.of_hash s hash |> Result.ok);
          io_field "contents" ~doc:"Find contents by hash"
            ~typ:Types.Contents.schema_typ
            ~args:Arg.[ arg "hash" ~typ:(non_null Input.hash) ]
            ~resolve:(fun _ _src k ->
              Lwt_eio.run_eio @@ fun () ->
              Store.Contents.of_hash s k |> Result.ok);
          io_field "contents_hash" ~doc:"Get the hash of some contents"
            ~typ:(non_null Types.Hash.schema_typ)
            ~args:Arg.[ arg "value" ~typ:(non_null Input.value) ]
            ~resolve:(fun _ _src c ->
              Lwt_eio.run_eio @@ fun () -> Store.Contents.hash c |> Result.ok);
          io_field "commit_of_key" ~doc:"Find commit by key"
            ~typ:store_schema.commit
            ~args:Arg.[ arg "key" ~typ:(non_null Input.commit_key) ]
            ~resolve:(fun _ _src k ->
              Lwt_eio.run_eio @@ fun () -> Store.Commit.of_key s k |> Result.ok);
          io_field "contents_of_key" ~doc:"Find contents by key"
            ~typ:Types.Contents.schema_typ
            ~args:Arg.[ arg "key" ~typ:(non_null Input.contents_key) ]
            ~resolve:(fun _ _src k ->
              Lwt_eio.run_eio @@ fun () ->
              Store.Contents.of_key s k |> Result.ok);
          io_field "branches" ~doc:"Get a list of all branches"
            ~typ:(non_null (list (non_null store_schema.branch)))
            ~args:[]
            ~resolve:(fun _ _ ->
              Lwt_eio.run_eio @@ fun () ->
              Store.Branch.list s
              |> List.map (fun branch ->
                     let store = Store.of_branch s branch in
                     (store, branch))
              |> Result.ok);
          io_field "main" ~doc:"Get main branch" ~typ:store_schema.branch
            ~args:[] ~resolve:(fun _ _ ->
              Lwt_eio.run_eio @@ fun () ->
              let t = Store.main s in
              Ok (Some (t, Store.Branch.main)));
          io_field "branch" ~doc:"Get branch by name" ~typ:store_schema.branch
            ~args:Arg.[ arg "name" ~typ:(non_null Input.branch) ]
            ~resolve:(fun _ _ branch ->
              Lwt_eio.run_eio @@ fun () ->
              let t = Store.of_branch s branch in
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
