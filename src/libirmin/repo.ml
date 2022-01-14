module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "repo_new"
      (config @-> returning repo)
      (fun config ->
        catch' repo (fun () ->
            let (s, config) : config = Root.get_config config in
            let (module Store) = Irmin_unix.Resolver.Store.generic_keyed s in
            let repo : Store.repo = run (Store.Repo.v config) in
            Root.create_repo
              (module Store)
              ( (module Store : Irmin.Generic_key.S with type repo = Store.repo),
                repo )))

  let () =
    fn "repo_branches"
      (repo @-> returning branch_array)
      (fun (type repo) repo ->
        catch' branch_array (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), repo
                =
              Root.get_repo repo
            in
            let b = run (Store.Repo.branches repo) in
            Root.create_branch_array (module Store) b))

  let () =
    fn "branch_array_length"
      (repo @-> branch_array @-> returning uint64_t)
      (fun (type repo) repo p ->
        catch UInt64.zero (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let arr = Root.get_branch_array (module Store) p in
            UInt64.of_int (Array.length arr)))

  let () =
    fn "branch_array_get"
      (repo @-> branch_array @-> uint64_t @-> returning irmin_string)
      (fun (type repo) repo p i ->
        let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
          Root.get_repo repo
        in
        let i = UInt64.to_int i in
        let arr = Root.get_branch_array (module Store) p in
        if i >= Array.length arr then null irmin_string
        else
          let x = Array.unsafe_get arr i in
          Root.create_string (Irmin.Type.to_string Store.Branch.t x))

  let () =
    fn "hash_equal"
      (repo @-> hash @-> hash @-> returning bool)
      (fun (type repo) repo a b ->
        catch false (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let a = Root.get_hash (module Store) a in
            let b = Root.get_hash (module Store) b in
            Irmin.Type.(unstage (equal Store.hash_t)) a b))

  let () =
    fn "contents_hash"
      (repo @-> contents @-> returning hash)
      (fun (type repo) repo a ->
        catch' hash (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let a = Root.get_contents (module Store) a in
            Root.create_hash (module Store) (Store.Contents.hash a)))

  let () =
    fn "contents_of_hash"
      (repo @-> hash @-> returning contents)
      (fun (type repo) repo a ->
        catch' contents (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), repo
                =
              Root.get_repo repo
            in
            let a = Root.get_hash (module Store) a in
            let c = run @@ Store.Contents.of_hash repo a in
            match c with
            | Some c -> Root.create_contents (module Store) c
            | None -> null contents))

  let () =
    fn "contents_of_key"
      (repo @-> kinded_key @-> returning contents)
      (fun (type repo) repo a ->
        catch' contents (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), repo
                =
              Root.get_repo repo
            in
            let a = Root.get_kinded_key (module Store) a in
            match a with
            | `Contents (a, _) -> (
                let c = run @@ Store.Contents.of_key repo a in
                match c with
                | Some c -> Root.create_contents (module Store) c
                | None -> null contents)
            | `Node _ -> null contents))

  let () =
    fn "contents_to_string"
      (repo @-> contents @-> returning irmin_string)
      (fun (type repo) repo contents ->
        catch' irmin_string (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let contents = Root.get_contents (module Store) contents in
            let s = Irmin.Type.to_string Store.contents_t contents in
            Root.create_string s))

  let () =
    fn "contents_of_string"
      (repo @-> ptr char @-> int64_t @-> returning contents)
      (fun (type repo) repo s length ->
        catch' contents (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let length = get_length length s in
            let s = string_from_ptr s ~length in
            let hash = Irmin.Type.of_string Store.contents_t s in
            match hash with
            | Ok p -> Root.create_contents (module Store) p
            | Error (`Msg e) ->
                let () = Util.error_msg := Some e in
                null contents))

  let () =
    fn "hash_to_string"
      (repo @-> hash @-> returning irmin_string)
      (fun (type repo) repo hash ->
        catch' irmin_string (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let hash = Root.get_hash (module Store) hash in
            let s = Irmin.Type.to_string Store.hash_t hash in
            Root.create_string s))

  let () =
    fn "hash_of_string"
      (repo @-> ptr char @-> int64_t @-> returning hash)
      (fun (type repo) repo s length ->
        catch' hash (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            let length = get_length length s in
            let s = string_from_ptr s ~length in
            let h = Irmin.Type.of_string Store.Hash.t s in
            match h with
            | Ok p -> Root.create_hash (module Store) p
            | Error (`Msg e) ->
                let () = Util.error_msg := Some e in
                null hash))

  let () =
    fn "metadata_default"
      (repo @-> returning metadata)
      (fun (type repo) repo ->
        catch' metadata (fun () ->
            let (module Store : Irmin.Generic_key.S with type repo = repo), _ =
              Root.get_repo repo
            in
            Root.create_metadata (module Store) Store.Metadata.default))

  let () = fn "hash_free" (hash @-> returning void) free
  let () = fn "branch_array_free" (branch_array @-> returning void) free
  let () = fn "repo_free" (repo @-> returning void) free
  let () = fn "metadata_free" (metadata @-> returning void) free
  let () = fn "contents_free" (contents @-> returning void) free
end
