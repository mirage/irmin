open Lwt.Syntax

module Make
    (IO : Conn.IO)
    (Codec : Conn.Codec.S)
    (Store : Irmin.Generic_key.S)
    (Tree : Tree.S
              with type concrete = Store.Tree.concrete
               and type kinded_key = Store.Tree.kinded_key)
    (Commit : Commit.S with type hash = Store.hash and type tree = Tree.t) =
struct
  include Context.Make (IO) (Codec) (Store) (Tree)
  module Return = Conn.Return

  type t = Tree.t

  module Empty = struct
    type req = unit [@@deriving irmin]
    type res = Tree.t [@@deriving irmin]

    let name = "tree.empty"

    let run conn ctx _ () =
      let empty = Store.Tree.empty in
      let id = incr_id () in
      Hashtbl.replace ctx.trees id (empty ());
      Return.v conn res_t (ID id)
  end

  module Clear = struct
    type req = Tree.t [@@deriving irmin]
    type res = unit [@@deriving irmin]

    let name = "tree.clear"

    let run conn ctx _ tree =
      let* _, tree = resolve_tree ctx tree in
      Store.Tree.clear tree;
      Return.v conn res_t ()
  end

  module Of_path = struct
    type req = Store.path [@@deriving irmin]
    type res = Tree.t option [@@deriving irmin]

    let name = "tree.of_path"

    let run conn ctx _ path =
      let* tree = Store.find_tree ctx.store path in
      match tree with
      | None -> Return.v conn res_t None
      | Some tree ->
          let id = incr_id () in
          Hashtbl.replace ctx.trees id tree;
          Return.v conn res_t (Some (ID id))
  end

  module Of_hash = struct
    type req = Store.hash [@@deriving irmin]
    type res = Tree.t option [@@deriving irmin]

    let name = "tree.of_hash"

    let run conn ctx _ hash =
      let* tree = Store.Tree.of_hash ctx.repo (`Node hash) in
      match tree with
      | None -> Return.v conn res_t None
      | Some tree ->
          let id = incr_id () in
          Hashtbl.replace ctx.trees id tree;
          Return.v conn res_t (Some (ID id))
  end

  module Of_commit = struct
    type req = Store.hash [@@deriving irmin]
    type res = Tree.t option [@@deriving irmin]

    let name = "tree.of_commit"

    let run conn ctx _ hash =
      let* commit = Store.Commit.of_hash ctx.repo hash in
      match commit with
      | None -> Return.v conn res_t None
      | Some commit ->
          let tree = Store.Commit.tree commit in
          let id = incr_id () in
          Hashtbl.replace ctx.trees id tree;
          Return.v conn res_t (Some (ID id))
  end

  module Save = struct
    type req = Tree.t [@@deriving irmin]

    type res = [ `Contents of Store.contents_key | `Node of Store.node_key ]
    [@@deriving irmin]

    let name = "tree.save"

    let run conn ctx _ tree =
      let* _, tree = resolve_tree ctx tree in
      let* hash =
        Store.Backend.Repo.batch ctx.repo (fun x y _ ->
            Store.save_tree ctx.repo x y tree)
      in
      Return.v conn res_t hash
  end

  module Add = struct
    type req = Tree.t * Store.path * Store.contents [@@deriving irmin]
    type res = Tree.t [@@deriving irmin]

    let name = "tree.add"

    let run conn ctx _ (tree, path, value) =
      let* _, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.add tree path value in
      let id = incr_id () in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn res_t (ID id)
  end

  module Batch_commit = struct
    type req = (Store.commit_key list * Store.info) * Tree.t [@@deriving irmin]
    type res = Store.commit_key [@@deriving irmin]

    let name = "tree.batch.commit"

    let run conn ctx _ ((parents, info), tree) =
      let* _, tree = resolve_tree ctx tree in
      let* commit = Store.Commit.v ctx.repo ~info ~parents tree in
      let key = Store.Commit.key commit in
      Return.v conn res_t key
  end

  module Batch_apply = struct
    type req =
      Store.path
      * (Store.hash list option * Store.info)
      * (Store.path
        * [ `Contents of
            [ `Hash of Store.Hash.t | `Value of Store.contents ]
            * Store.metadata option
          | `Tree of Tree.t ]
          option)
        list
    [@@deriving irmin]

    type res = unit [@@deriving irmin]

    let name = "tree.batch.apply"

    let mk_parents ctx parents =
      match parents with
      | None -> Lwt.return None
      | Some parents ->
          let* parents =
            Lwt_list.filter_map_s
              (fun hash -> Store.Commit.of_hash ctx.repo hash)
              parents
          in
          Lwt.return_some parents

    let run conn ctx _ (path, (parents, info), l) =
      let* parents = mk_parents ctx parents in
      let* () =
        Store.with_tree_exn ctx.store path ?parents
          ~info:(fun () -> info)
          (fun tree ->
            let tree = Option.value ~default:(Store.Tree.empty ()) tree in
            let* tree =
              Lwt_list.fold_left_s
                (fun tree (path, value) ->
                  match value with
                  | Some (`Contents (`Hash value, metadata)) ->
                      let* value = Store.Contents.of_hash ctx.repo value in
                      Store.Tree.add tree path ?metadata (Option.get value)
                  | Some (`Contents (`Value value, metadata)) ->
                      Store.Tree.add tree path ?metadata value
                  | Some (`Tree t) ->
                      let* _, tree' = resolve_tree ctx t in
                      Store.Tree.add_tree tree path tree'
                  | None -> Store.Tree.remove tree path)
                tree l
            in
            Lwt.return (Some tree))
      in
      Return.v conn res_t ()
  end

  module Batch_tree = struct
    type req =
      Tree.t
      * (Store.path
        * [ `Contents of
            [ `Hash of Store.Hash.t | `Value of Store.contents ]
            * Store.metadata option
          | `Tree of Tree.t ]
          option)
        list
    [@@deriving irmin]

    type res = Tree.t [@@deriving irmin]

    let name = "tree.batch.tree"

    let run conn ctx _ (tree, l) =
      let* _, tree = resolve_tree ctx tree in
      let* tree =
        Lwt_list.fold_left_s
          (fun tree (path, value) ->
            match value with
            | Some (`Contents (`Hash value, metadata)) ->
                let* value = Store.Contents.of_hash ctx.repo value in
                Store.Tree.add tree path ?metadata (Option.get value)
            | Some (`Contents (`Value value, metadata)) ->
                Store.Tree.add tree path ?metadata value
            | Some (`Tree t) ->
                let* _, tree' = resolve_tree ctx t in
                Store.Tree.add_tree tree path tree'
            | None -> Store.Tree.remove tree path)
          tree l
      in
      let id = incr_id () in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn res_t (ID id)
  end

  module Add_tree = struct
    type req = Tree.t * Store.path * Tree.t [@@deriving irmin]
    type res = Tree.t [@@deriving irmin]

    let name = "tree.add_tree"

    let run conn ctx _ (tree, path, tr) =
      let* _, tree = resolve_tree ctx tree in
      let* _, tree' = resolve_tree ctx tr in
      let* tree = Store.Tree.add_tree tree path tree' in
      let id = incr_id () in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn res_t (ID id)
  end

  module Merge = struct
    type req = Tree.t * Tree.t * Tree.t [@@deriving irmin]
    type res = Tree.t [@@deriving irmin]

    let name = "tree.merge"

    let run conn ctx _ (old, tree, tr) =
      let* _, old = resolve_tree ctx old in
      let* _, tree = resolve_tree ctx tree in
      let* _, tree' = resolve_tree ctx tr in
      let* tree =
        Irmin.Merge.f Store.Tree.merge ~old:(Irmin.Merge.promise old) tree tree'
      in
      match tree with
      | Ok tree ->
          let id = incr_id () in
          Hashtbl.replace ctx.trees id tree;
          Return.v conn res_t (ID id)
      | Error e ->
          Return.err conn (Irmin.Type.to_string Irmin.Merge.conflict_t e)
  end

  module Find = struct
    type req = Tree.t * Store.path [@@deriving irmin]
    type res = Store.contents option [@@deriving irmin]

    let name = "tree.find"

    let run conn ctx _ (tree, path) =
      let* _, tree = resolve_tree ctx tree in
      let* contents = Store.Tree.find tree path in
      Return.v conn res_t contents
  end

  module Find_tree = struct
    type req = Tree.t * Store.path [@@deriving irmin]
    type res = Tree.t option [@@deriving irmin]

    let name = "tree.find_tree"

    let run conn ctx _ (tree, path) =
      let* _, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.find_tree tree path in
      let tree =
        Option.map
          (fun tree ->
            let id = incr_id () in
            Hashtbl.replace ctx.trees id tree;
            Tree.ID id)
          tree
      in
      Return.v conn res_t tree
  end

  module Remove = struct
    type req = Tree.t * Store.path [@@deriving irmin]
    type res = Tree.t [@@deriving irmin]

    let name = "tree.remove"

    let run conn ctx _ (tree, path) =
      let* _, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.remove tree path in
      let id = incr_id () in
      Hashtbl.replace ctx.trees id tree;
      Return.v conn res_t (ID id)
  end

  module Cleanup = struct
    type req = Tree.t [@@deriving irmin]
    type res = unit [@@deriving irmin]

    let name = "tree.cleanup"

    let run conn ctx _ tree =
      let () =
        match tree with Tree.ID id -> Hashtbl.remove ctx.trees id | _ -> ()
      in
      Return.ok conn
  end

  module To_local = struct
    type req = Tree.t [@@deriving irmin]
    type res = Tree.concrete [@@deriving irmin]

    let name = "tree.to_local"

    let run conn ctx _ tree =
      let* _, tree = resolve_tree ctx tree in
      let* tree = Store.Tree.to_concrete tree in
      Return.v conn res_t tree
  end

  module Mem = struct
    type req = Tree.t * Store.path [@@deriving irmin]
    type res = bool [@@deriving irmin]

    let name = "tree.mem"

    let run conn ctx _ (tree, path) =
      let* _, tree = resolve_tree ctx tree in
      let* res = Store.Tree.mem tree path in
      Return.v conn res_t res
  end

  module Mem_tree = struct
    type req = Tree.t * Store.path [@@deriving irmin]
    type res = bool [@@deriving irmin]

    let name = "tree.mem_tree"

    let run conn ctx _ (tree, path) =
      let* _, tree = resolve_tree ctx tree in
      let* res = Store.Tree.mem_tree tree path in
      Return.v conn res_t res
  end

  module List = struct
    type req = Tree.t * Store.path [@@deriving irmin]
    type tree = [ `Contents | `Tree ] [@@deriving irmin]
    type res = (Store.Path.step * [ `Contents | `Tree ]) list [@@deriving irmin]

    let name = "tree.list"

    let run conn ctx _ (tree, path) =
      let* _, tree = resolve_tree ctx tree in
      let* l = Store.Tree.list tree path in
      let* x =
        Lwt_list.map_s
          (fun (k, _) ->
            let+ exists = Store.Tree.mem_tree tree (Store.Path.rcons path k) in
            if exists then (k, `Tree) else (k, `Contents))
          l
      in
      Return.v conn res_t x
  end

  module Hash = struct
    type req = Tree.t [@@deriving irmin]
    type res = Store.Hash.t [@@deriving irmin]

    let name = "tree.hash"

    let run conn ctx _ tree =
      let* _, tree = resolve_tree ctx tree in
      let hash = Store.Tree.hash tree in
      Return.v conn res_t hash
  end

  module Key = struct
    type req = Tree.t [@@deriving irmin]
    type res = Store.Tree.kinded_key [@@deriving irmin]

    let name = "tree.key"

    let run conn ctx _ tree =
      let* _, tree = resolve_tree ctx tree in
      let key = Store.Tree.key tree in
      Return.v conn res_t (Option.get key)
  end

  module Cleanup_all = struct
    type req = unit [@@deriving irmin]
    type res = unit [@@deriving irmin]

    let name = "tree.cleanup_all"

    let run conn ctx _ () =
      reset_trees ctx;
      Return.v conn res_t ()
  end

  let commands =
    [
      cmd (module Empty);
      cmd (module Clear);
      cmd (module Add);
      cmd (module Batch_commit);
      cmd (module Batch_apply);
      cmd (module Batch_tree);
      cmd (module Remove);
      cmd (module Cleanup);
      cmd (module Cleanup_all);
      cmd (module Mem);
      cmd (module Mem_tree);
      cmd (module List);
      cmd (module To_local);
      cmd (module Find);
      cmd (module Find_tree);
      cmd (module Add_tree);
      cmd (module Hash);
      cmd (module Merge);
      cmd (module Save);
      cmd (module Of_path);
      cmd (module Of_hash);
      cmd (module Of_commit);
    ]
end
