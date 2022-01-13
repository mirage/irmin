module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "tree_new"
      (repo @-> returning tree)
      (fun (type repo) repo ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        Root.create_tree (module Store) (Store.Tree.empty ()))

  let () =
    fn "tree_of_contents"
      (repo @-> contents @-> metadata @-> returning tree)
      (fun (type repo) repo value metadata ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        let metadata =
          if is_null metadata then None
          else Some (Root.get_metadata (module Store) metadata)
        in
        let value = Root.get_contents (module Store) value in
        Root.create_tree (module Store) (Store.Tree.of_contents ?metadata value))

  let () =
    fn "tree_clone"
      (repo @-> tree @-> returning tree)
      (fun (type repo) repo tree ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        let tree : Store.tree = Root.get_tree (module Store) tree in
        Root.create_tree (module Store) tree)

  let () =
    fn "tree_hash"
      (repo @-> tree @-> returning hash)
      (fun (type repo) repo tree ->
        catch' hash (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree : Store.tree = Root.get_tree (module Store) tree in
            let k = Store.Tree.hash tree in
            Root.create_hash (module Store) k))

  let () =
    fn "tree_of_hash"
      (repo @-> hash @-> returning tree)
      (fun (type repo) repo k ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), repo =
          Root.get_repo repo
        in
        let k = Root.get_hash (module Store) k in
        let t = run (Store.Tree.of_hash repo (`Node k)) in
        match t with
        | Some t -> Root.create_tree (module Store) t
        | None -> null tree)

  let () =
    fn "tree_key"
      (repo @-> tree @-> returning kinded_key)
      (fun (type repo) repo tree ->
        catch' kinded_key (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree : Store.tree = Root.get_tree (module Store) tree in
            let k = Store.Tree.key tree in
            match k with
            | Some k -> Root.create_kinded_key (module Store) k
            | _ -> null kinded_key))

  let () =
    fn "tree_of_key"
      (repo @-> kinded_key @-> returning tree)
      (fun (type repo) repo k ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), repo =
          Root.get_repo repo
        in
        let k = Root.get_kinded_key (module Store) k in
        let t = run (Store.Tree.of_key repo k) in
        match t with
        | Some t -> Root.create_tree (module Store) t
        | None -> null tree)

  let () =
    fn "tree_mem"
      (repo @-> tree @-> path @-> returning bool)
      (fun (type repo) repo tree path ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            run (Store.Tree.mem tree path)))

  let () =
    fn "tree_mem_tree"
      (repo @-> tree @-> path @-> returning bool)
      (fun (type repo) repo tree path ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            run (Store.Tree.mem_tree tree path)))

  let () =
    fn "tree_find"
      (repo @-> tree @-> path @-> returning contents)
      (fun (type repo) repo tree path ->
        catch' contents (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            match run (Store.Tree.find tree path) with
            | None -> null contents
            | Some x -> Root.create_contents (module Store) x))

  let () =
    fn "tree_find_metadata"
      (repo @-> tree @-> path @-> returning metadata)
      (fun (type repo) repo tree path ->
        catch' metadata (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            match run (Store.Tree.find_all tree path) with
            | None -> null metadata
            | Some (_, m) -> Root.create_metadata (module Store) m))

  let () =
    fn "tree_find_tree"
      (repo @-> tree @-> path @-> returning tree)
      (fun (type repo) repo t path ->
        catch' tree (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let t : Store.tree = Root.get_tree (module Store) t in
            let path : Store.path = Root.get_path (module Store) path in
            match run (Store.Tree.find_tree t path) with
            | None -> null tree
            | Some x -> Root.create_tree (module Store) x))

  let () =
    fn "tree_add"
      (repo @-> tree @-> path @-> contents @-> metadata @-> returning bool)
      (fun (type repo) repo tree path value metadata ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree' : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            let value : Store.contents =
              Root.get_contents (module Store) value
            in
            let metadata =
              if is_null metadata then None
              else Some (Root.get_metadata (module Store) metadata)
            in
            let t = run (Store.Tree.add tree' path value ?metadata) in
            Root.set_tree (module Store) tree t;
            true))

  let () =
    fn "tree_add_tree"
      (repo @-> tree @-> path @-> tree @-> returning bool)
      (fun (type repo) repo tree path tr ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree' : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            let value : Store.tree = Root.get_tree (module Store) tr in
            let t = run (Store.Tree.add_tree tree' path value) in
            Root.set_tree (module Store) tree t;
            true))

  let () =
    fn "tree_remove"
      (repo @-> tree @-> path @-> returning bool)
      (fun (type repo) repo tree path ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree' : Store.tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            let t = run (Store.Tree.remove tree' path) in
            Root.set_tree (module Store) tree t;
            true))

  let () =
    fn "tree_equal"
      (repo @-> tree @-> tree @-> returning bool)
      (fun (type repo) repo a b ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        let a = Root.get_tree (module Store) a in
        let b = Root.get_tree (module Store) b in
        Irmin.Type.(unstage (equal Store.tree_t)) a b)

  let () =
    fn "tree_list"
      (repo @-> tree @-> path @-> returning path_list)
      (fun (type repo) repo tree path ->
        catch' path_list (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let tree = Root.get_tree (module Store) tree in
            let path : Store.path = Root.get_path (module Store) path in
            let items = run (Store.Tree.list tree path) in
            let items = List.map (fun (k, _v) -> Store.Path.v [ k ]) items in
            Root.create_path_list (module Store) items))

  let () =
    fn "kinded_key_is_contents"
      (repo @-> kinded_key @-> returning bool)
      (fun (type repo) repo k ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let k = Root.get_kinded_key (module Store) k in
            match k with `Contents _ -> true | _ -> false))

  let () =
    fn "kinded_key_is_node"
      (repo @-> kinded_key @-> returning bool)
      (fun (type repo) repo k ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let k = Root.get_kinded_key (module Store) k in
            match k with `Node _ -> true | _ -> false))

  let () = fn "tree_free" (tree @-> returning void) free
  let () = fn "kinded_key_free" (kinded_key @-> returning void) free
end
