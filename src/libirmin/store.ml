module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "main"
      (repo @-> returning store)
      (fun (type repo) repo ->
        catch' store (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), repo
                =
              Root.get_repo repo
            in
            Root.create_store
              (module Store)
              ( (module Store : Irmin.Generic_key.S with type t = Store.t),
                run (Store.main repo) )))

  let () =
    fn "of_branch"
      (repo @-> string @-> returning store)
      (fun (type repo) repo name ->
        catch' store (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), repo
                =
              Root.get_repo repo
            in
            match Irmin.Type.of_string Store.Branch.t name with
            | Error _ -> null store
            | Ok branch ->
                Root.create_store
                  (module Store)
                  ( (module Store : Irmin.Generic_key.S with type t = Store.t),
                    run (Store.of_branch repo branch) )))

  let () =
    fn "get_head"
      (store @-> returning commit)
      (fun (type t) store ->
        catch' commit (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let c = run (Store.Head.find store) in
            match c with
            | None -> null commit
            | Some x -> Root.create_commit (module Store) x))

  let () =
    fn "set_head"
      (store @-> commit @-> returning void)
      (fun (type t) store commit ->
        catch () (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let commit : Store.commit = Root.get_commit (module Store) commit in
            run (Store.Head.set store commit)))

  let () =
    fn "fast_forward"
      (store @-> commit @-> returning bool)
      (fun (type t) store commit ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let commit : Store.commit = Root.get_commit (module Store) commit in
            let res = run (Store.Head.fast_forward store commit) in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.ff_error_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "merge_with_branch"
      (store @-> string @-> info @-> returning bool)
      (fun (type t) store branch info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let info = Root.get_info (module Store) info in
            let branch =
              Irmin.Type.of_string Store.branch_t branch |> Result.get_ok
            in
            let res =
              run (Store.merge_with_branch store branch ~info:(fun () -> info))
            in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Irmin.Merge.conflict_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "merge_with_commit"
      (store @-> commit @-> info @-> returning bool)
      (fun (type t) store commit info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let info = Root.get_info (module Store) info in
            let commit = Root.get_commit (module Store) commit in
            let res =
              run (Store.merge_with_commit store commit ~info:(fun () -> info))
            in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Irmin.Merge.conflict_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "merge_into"
      (store @-> store @-> info @-> returning bool)
      (fun (type t) store store1 info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let (module Store1 : Irmin.Generic_key.S with type t = t), store1 =
              Root.get_store store1
            in
            let info = Root.get_info (module Store) info in
            let res =
              run (Store.merge_into ~into:store store1 ~info:(fun () -> info))
            in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Irmin.Merge.conflict_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "set"
      (store @-> path @-> contents @-> info @-> returning bool)
      (fun (type t) store path value info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            let value : Store.contents =
              Root.get_contents (module Store) value
            in
            let x = run (Store.set store path value ~info:(fun () -> info)) in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "test_and_set"
      (store @-> path @-> contents @-> contents @-> info @-> returning bool)
      (fun (type t) store path test set info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            let test : Store.contents option =
              if is_null test then None
              else Some (Root.get_contents (module Store) test)
            in
            let set : Store.contents option =
              if is_null set then None
              else Some (Root.get_contents (module Store) set)
            in
            let x =
              run
                (Store.test_and_set store path ~test ~set ~info:(fun () -> info))
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "test_and_set_tree"
      (store @-> path @-> tree @-> tree @-> info @-> returning bool)
      (fun (type t) store path test set info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            let test : Store.tree option =
              if is_null test then None
              else Some (Root.get_tree (module Store) test)
            in
            let set : Store.tree option =
              if is_null set then None
              else Some (Root.get_tree (module Store) set)
            in
            let x =
              run
                (Store.test_and_set_tree store path ~test ~set ~info:(fun () ->
                     info))
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "set_tree"
      (store @-> path @-> tree @-> info @-> returning bool)
      (fun (type t) store path tree info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let info : Store.info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            let tree' : Store.tree = Root.get_tree (module Store) tree in
            let x =
              run (Store.set_tree store path tree' ~info:(fun () -> info))
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "find"
      (store @-> path @-> returning contents)
      (fun (type t) store path ->
        catch' contents (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let path : Store.path = Root.get_path (module Store) path in
            let x = run (Store.find store path) in
            match x with
            | Some x -> Root.create_contents (module Store) x
            | None -> null contents))

  let () =
    fn "find_metadata"
      (store @-> path @-> returning metadata)
      (fun (type t) store path ->
        catch' metadata (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let path : Store.path = Root.get_path (module Store) path in
            let x = run (Store.find_all store path) in
            match x with
            | Some (_, m) -> Root.create_metadata (module Store) m
            | None -> null metadata))

  let () =
    fn "find_tree"
      (store @-> path @-> returning tree)
      (fun (type t) store path ->
        catch' tree (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let path : Store.path = Root.get_path (module Store) path in
            let x : Store.tree option = run (Store.find_tree store path) in
            match x with
            | Some x -> Root.create_tree (module Store) x
            | None -> null tree))

  let () =
    fn "remove"
      (store @-> path @-> info @-> returning bool)
      (fun (type t) store path info ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let module Info = Irmin_unix.Info (Store.Info) in
            let info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            match run (Store.remove store path ~info:(fun () -> info)) with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                let () = Util.error_msg := Some s in
                false))

  let () =
    fn "mem"
      (store @-> path @-> returning bool)
      (fun (type t) store path ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let path : Store.path = Root.get_path (module Store) path in
            run (Store.mem store path)))

  let () =
    fn "mem_tree"
      (store @-> path @-> returning bool)
      (fun (type t) store path ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let path : Store.path = Root.get_path (module Store) path in
            run (Store.mem_tree store path)))

  let () =
    fn "list"
      (store @-> path @-> returning path_list)
      (fun (type t) store path ->
        catch' path_list (fun () ->
            let (module Store : Irmin.Generic_key.S with type t = t), store =
              Root.get_store store
            in
            let path : Store.path = Root.get_path (module Store) path in
            let items = run (Store.list store path) in
            let items = List.map (fun (k, _v) -> Store.Path.v [ k ]) items in
            Root.create_path_list (module Store) items))

  let () =
    fn "path_list_length"
      (repo @-> path_list @-> returning uint64_t)
      (fun (type repo) repo p ->
        catch UInt64.zero (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let arr = Root.get_path_list (module Store) p in
            UInt64.of_int (Array.length arr)))

  let () =
    fn "commit_list_length"
      (repo @-> commit_list @-> returning uint64_t)
      (fun (type repo) repo p ->
        catch UInt64.zero (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let arr = Root.get_commit_list (module Store) p in
            UInt64.of_int (Array.length arr)))

  let () =
    fn "path_list_get"
      (repo @-> path_list @-> uint64_t @-> returning path)
      (fun (type repo) repo p i ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        let i = UInt64.to_int i in
        let arr = Root.get_path_list (module Store) p in
        if i >= Array.length arr then null path
        else
          let x = Array.unsafe_get arr i in
          Root.create_path (module Store) x)

  let () =
    fn "commit_list_get"
      (repo @-> commit_list @-> uint64_t @-> returning commit)
      (fun (type repo) repo p i ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        let i = UInt64.to_int i in
        let arr = Root.get_commit_list (module Store) p in
        if i >= Array.length arr then null commit
        else
          let x = Array.unsafe_get arr i in
          Root.create_commit (module Store) x)

  let () = fn "path_list_free" (path_list @-> returning void) free
  let () = fn "commit_list_free" (commit_list @-> returning void) free
  let () = fn "free" (store @-> returning void) free
end
