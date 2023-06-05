(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "main"
      (repo @-> returning store)
      (fun (type repo) repo' ->
        let r = Root.to_voidp repo repo' in
        with_repo' repo' store
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            Root.create_store
              (module Store)
              {
                repo = r;
                store_mod =
                  (module Store : Irmin.Generic_key.S with type t = Store.t);
                store = run (fun () -> Store.main repo);
              }))

  let () =
    fn "of_branch"
      (repo @-> string @-> returning store)
      (fun (type repo) repo' name ->
        let r = Root.to_voidp repo repo' in
        with_repo' repo' store
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) repo
          ->
            match Irmin.Type.of_string Store.Branch.t name with
            | Error (`Msg err) -> failwith err
            | Ok branch ->
                Root.create_store
                  (module Store)
                  {
                    repo = r;
                    store_mod =
                      (module Store : Irmin.Generic_key.S with type t = Store.t);
                    store = run (fun () -> Store.of_branch repo branch);
                  }))

  let () =
    fn "of_commit"
      (repo @-> commit @-> returning store)
      (fun (type repo) repo' commit ->
        let r = Root.to_voidp repo repo' in
        with_repo' repo' store
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let commit = Root.get_commit (module Store) commit in
            Root.create_store
              (module Store)
              {
                repo = r;
                store_mod =
                  (module Store : Irmin.Generic_key.S with type t = Store.t);
                store = run (fun () -> Store.of_commit commit);
              }))

  let () =
    fn "get_head"
      (store @-> returning commit)
      (fun (type t) store ->
        with_store' store commit
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let c = run (fun () -> Store.Head.find store) in
            match c with
            | None -> null commit
            | Some x -> Root.create_commit (module Store) x))

  let () =
    fn "set_head"
      (store @-> commit @-> returning void)
      (fun (type t) store commit ->
        with_store store ()
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let commit : Store.commit = Root.get_commit (module Store) commit in
            run (fun () -> Store.Head.set store commit)))

  let () =
    fn "fast_forward"
      (store @-> commit @-> returning bool)
      (fun (type t) store commit ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let commit : Store.commit = Root.get_commit (module Store) commit in
            let res = run (fun () -> Store.Head.fast_forward store commit) in
            match res with
            | Ok () -> true
            | Error e -> failwith (Irmin.Type.to_string Store.ff_error_t e)))

  let () =
    fn "merge_with_branch"
      (store @-> string @-> info @-> returning bool)
      (fun (type t) store branch info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let info = Root.get_info (module Store) info in
            let branch =
              Irmin.Type.of_string Store.branch_t branch |> Result.get_ok
            in
            let res =
              run (fun () ->
                  Store.merge_with_branch store branch ~info:(fun () -> info))
            in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Irmin.Merge.conflict_t e in
                failwith s))

  let () =
    fn "merge_with_commit"
      (store @-> commit @-> info @-> returning bool)
      (fun (type t) store commit info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let info = Root.get_info (module Store) info in
            let commit = Root.get_commit (module Store) commit in
            let res =
              run (fun () ->
                  Store.merge_with_commit store commit ~info:(fun () -> info))
            in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Irmin.Merge.conflict_t e in
                failwith s))

  let () =
    fn "merge_into"
      (store @-> store @-> info @-> returning bool)
      (fun (type t) store store1 info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let store1 = Root.get_store store1 in
            let info = Root.get_info (module Store) info in
            let res =
              run @@ fun () ->
              Store.merge_into ~into:store store1.store ~info:(fun () -> info)
            in
            match res with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Irmin.Merge.conflict_t e in
                failwith s))

  let () =
    fn "set"
      (store @-> path @-> contents @-> info @-> returning bool)
      (fun (type t) store path value info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            let value : Store.contents =
              Root.get_contents (module Store) value
            in
            let x =
              run @@ fun () -> Store.set store path value ~info:(fun () -> info)
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                failwith s))

  let () =
    fn "test_and_set"
      (store @-> path @-> contents @-> contents @-> info @-> returning bool)
      (fun (type t) store path test set info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
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
              run @@ fun () ->
              Store.test_and_set store path ~test ~set ~info:(fun () -> info)
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                failwith s))

  let () =
    fn "test_and_set_tree"
      (store @-> path @-> tree @-> tree @-> info @-> returning bool)
      (fun (type t) store path test set info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
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
              run @@ fun () ->
              Store.test_and_set_tree store path ~test ~set ~info:(fun () ->
                  info)
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                failwith s))

  let () =
    fn "set_tree"
      (store @-> path @-> tree @-> info @-> returning bool)
      (fun (type t) store path tree info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let info : Store.info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            let tree' : Store.tree = Root.get_tree (module Store) tree in
            let x =
              run @@ fun () ->
              Store.set_tree store path tree' ~info:(fun () -> info)
            in
            match x with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                failwith s))

  let () =
    fn "find"
      (store @-> path @-> returning contents)
      (fun (type t) store path ->
        with_store' store contents
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let path : Store.path = Root.get_path (module Store) path in
            let x = run (fun () -> Store.find store path) in
            match x with
            | Some x -> Root.create_contents (module Store) x
            | None -> null contents))

  let () =
    fn "find_metadata"
      (store @-> path @-> returning metadata)
      (fun (type t) store path ->
        with_store' store metadata
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let path : Store.path = Root.get_path (module Store) path in
            let x = run (fun () -> Store.find_all store path) in
            match x with
            | Some (_, m) -> Root.create_metadata (module Store) m
            | None -> null metadata))

  let () =
    fn "find_tree"
      (store @-> path @-> returning tree)
      (fun (type t) store path ->
        with_store' store tree
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let path : Store.path = Root.get_path (module Store) path in
            let x : Store.tree option =
              run (fun () -> Store.find_tree store path)
            in
            match x with
            | Some x -> Root.create_tree (module Store) x
            | None -> null tree))

  let () =
    fn "remove"
      (store @-> path @-> info @-> returning bool)
      (fun (type t) store path info ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let module Info = Irmin_unix.Info (Store.Info) in
            let info = Root.get_info (module Store) info in
            let path : Store.path = Root.get_path (module Store) path in
            match
              run (fun () -> Store.remove store path ~info:(fun () -> info))
            with
            | Ok () -> true
            | Error e ->
                let s = Irmin.Type.to_string Store.write_error_t e in
                failwith s))

  let () =
    fn "mem"
      (store @-> path @-> returning bool)
      (fun (type t) store path ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let path : Store.path = Root.get_path (module Store) path in
            run (fun () -> Store.mem store path)))

  let () =
    fn "mem_tree"
      (store @-> path @-> returning bool)
      (fun (type t) store path ->
        with_store store false
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let path : Store.path = Root.get_path (module Store) path in
            run (fun () -> Store.mem_tree store path)))

  let () =
    fn "list"
      (store @-> path @-> returning path_array)
      (fun (type t) store path ->
        with_store' store path_array
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let path : Store.path = Root.get_path (module Store) path in
            let items = run (fun () -> Store.list store path) in
            let items = List.map (fun (k, _v) -> Store.Path.v [ k ]) items in
            Root.create_path_array (module Store) items))

  let () =
    fn "path_array_length"
      (repo @-> path_array @-> returning uint64_t)
      (fun (type repo) repo p ->
        with_repo repo UInt64.zero
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let arr = Root.get_path_array (module Store) p in
            UInt64.of_int (Array.length arr)))

  let () =
    fn "path_array_get"
      (repo @-> path_array @-> uint64_t @-> returning path)
      (fun (type repo) repo p i ->
        with_repo' repo path
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let i = UInt64.to_int i in
            let arr = Root.get_path_array (module Store) p in
            if i >= Array.length arr then failwith "index out of bounds"
            else
              let x = Array.unsafe_get arr i in
              Root.create_path (module Store) x))

  let () =
    fn "remote_store"
      (store @-> returning remote)
      (fun (type t) store ->
        with_store' store remote
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            Root.create_remote (Irmin.remote_store (module Store) store)))

  let () =
    fn "remote"
      (repo @-> string @-> returning remote)
      (fun (type repo) repo url ->
        let r = Root.get_repo repo in
        let remote_fn = r.remote in
        with_repo' repo remote
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            match remote_fn with
            | None ->
                failwith "sync is not implemented for the selected backend"
            | Some f -> Root.create_remote (run (fun () -> f url ()))))

  let () =
    fn "remote_with_auth"
      (repo @-> string @-> string @-> string_opt @-> returning remote)
      (fun (type repo) repo url user token ->
        let r = Root.get_repo repo in
        let remote_fn = r.remote in
        with_repo' repo remote
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            match remote_fn with
            | None ->
                failwith "sync is not implemented for the selected backend"
            | Some f ->
                let headers = Cohttp.Header.init () in
                let headers =
                  match token with
                  | Some token ->
                      Cohttp.Header.add_authorization headers
                        (`Basic (user, token))
                  | _ -> Cohttp.Header.add_authorization headers (`Other user)
                in
                Root.create_remote (run (fun () -> f ~headers url ()))))

  let () =
    fn "fetch"
      (store @-> int @-> remote @-> returning commit)
      (fun (type t) store depth remote ->
        with_store' store commit
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let module Sync = Irmin.Sync.Make (Store) in
            let remote =
              if is_null remote then failwith "Invalid remote"
              else Root.get_remote remote
            in
            let depth = if depth <= 0 then None else Some depth in
            match run (fun () -> Sync.fetch_exn ?depth store remote) with
            | `Empty -> null commit
            | `Head head -> Root.create_commit (module Store) head))

  let () =
    fn "pull"
      (store @-> int @-> remote @-> info @-> returning commit)
      (fun (type t) store depth remote info ->
        with_store' store commit
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let module Sync = Irmin.Sync.Make (Store) in
            let remote =
              if is_null remote then failwith "Invalid remote"
              else Root.get_remote remote
            in
            let x =
              if is_null info then `Set
              else `Merge (fun () -> Root.get_info (module Store) info)
            in
            let depth = if depth <= 0 then None else Some depth in
            match run (fun () -> Sync.pull_exn ?depth store remote x) with
            | `Empty -> null commit
            | `Head head -> Root.create_commit (module Store) head))

  let () =
    fn "push"
      (store @-> int @-> remote @-> returning commit)
      (fun (type t) store depth remote ->
        with_store' store commit
          (fun (module Store : Irmin.Generic_key.S with type t = t) store ->
            let module Sync = Irmin.Sync.Make (Store) in
            let remote =
              if is_null remote then failwith "Invalid remote"
              else Root.get_remote remote
            in
            let depth = if depth <= 0 then None else Some depth in
            match run (fun () -> Sync.push_exn ?depth store remote) with
            | `Empty -> null commit
            | `Head head -> Root.create_commit (module Store) head))

  let () = fn "remote_free" (remote @-> returning void) free
  let () = fn "path_array_free" (path_array @-> returning void) free
  let () = fn "free" (store @-> returning void) free
end
