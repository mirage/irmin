module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "commit_info"
      (repo @-> commit @-> returning info)
      (fun (type repo) repo commit ->
        with_repo' repo info
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let commit = Root.get_commit (module Store) commit in
            Root.create_info (module Store) (Store.Commit.info commit)))

  let () =
    fn "commit_hash"
      (repo @-> commit @-> returning hash)
      (fun (type repo) repo commit ->
        with_repo' repo hash
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let commit = Root.get_commit (module Store) commit in
            Root.create_hash (module Store) (Store.Commit.hash commit)))

  let () =
    fn "commit_key"
      (repo @-> commit @-> returning commit_key)
      (fun (type repo) repo commit ->
        with_repo' repo commit_key
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let commit = Root.get_commit (module Store) commit in
            Root.create_commit_key (module Store) (Store.Commit.key commit)))

  let () =
    fn "commit_of_hash"
      (repo @-> hash @-> returning commit)
      (fun (type repo) repo hash ->
        with_repo' repo commit
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            let hash = Root.get_hash (module Store) hash in
            let c = run (Store.Commit.of_hash repo hash) in
            match c with
            | Some c -> Root.create_commit (module Store) c
            | None -> null commit))

  let () =
    fn "commit_of_key"
      (repo @-> commit_key @-> returning commit)
      (fun (type repo) repo hash ->
        with_repo' repo commit
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            let hash = Root.get_commit_key (module Store) hash in
            let c = run (Store.Commit.of_key repo hash) in
            match c with
            | Some c -> Root.create_commit (module Store) c
            | None -> null commit))

  let () =
    fn "commit_new"
      (repo @-> ptr commit @-> uint64_t @-> tree @-> info @-> returning commit)
      (fun (type repo) repo parents n tree info ->
        with_repo' repo commit
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            let n = UInt64.to_int n in
            let parents =
              if is_null parents || n = 0 then []
              else
                CArray.from_ptr parents n
                |> CArray.to_list
                |> List.map (Root.get_commit (module Store))
                |> List.map Store.Commit.key
            in
            let tree = Root.get_tree (module Store) tree in
            let info = Root.get_info (module Store) info in
            let commit = run (Store.Commit.v repo ~parents ~info tree) in
            Root.create_commit (module Store) commit))

  let () =
    let open Lwt.Infix in
    fn "commit_parents"
      (repo @-> commit @-> returning commit_array)
      (fun (type repo) repo commit ->
        with_repo' repo commit_array
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            let commit = Root.get_commit (module Store) commit in
            let parents = Store.Commit.parents commit in
            let parents =
              run
                (Lwt_list.filter_map_s
                   (fun x ->
                     Store.Commit.of_key repo x >|= function
                     | None -> None
                     | Some x -> Some x)
                   parents)
            in
            Root.create_commit_array (module Store) parents))

  let () =
    fn "commit_equal"
      (repo @-> commit @-> commit @-> returning bool)
      (fun (type repo) repo a b ->
        with_repo repo false
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            let a = Root.get_commit (module Store) a in
            let b = Root.get_commit (module Store) b in
            Irmin.Type.(unstage (equal (Store.commit_t repo))) a b))

  let () =
    fn "commit_tree"
      (repo @-> commit @-> returning tree)
      (fun (type repo) repo commit ->
        with_repo' repo tree
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let commit = Root.get_commit (module Store) commit in
            Root.create_tree (module Store) (Store.Commit.tree commit)))

  let () =
    fn "commit_array_length"
      (repo @-> commit_array @-> returning uint64_t)
      (fun (type repo) repo p ->
        with_repo repo UInt64.zero
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let arr = Root.get_commit_array (module Store) p in
            UInt64.of_int (Array.length arr)))

  let () =
    fn "commit_array_get"
      (repo @-> commit_array @-> uint64_t @-> returning commit)
      (fun (type repo) repo p i ->
        with_repo' repo commit
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let i = UInt64.to_int i in
            let arr = Root.get_commit_array (module Store) p in
            if i >= Array.length arr then null commit
            else
              let x = Array.unsafe_get arr i in
              Root.create_commit (module Store) x))

  let () = fn "commit_array_free" (commit_array @-> returning void) free
  let () = fn "commit_free" (commit @-> returning void) free
  let () = fn "commit_key_free" (commit_key @-> returning void) free
end
