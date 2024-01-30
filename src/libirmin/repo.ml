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

(* TODO: Fix me because this is quite ugly and nothing but a temporary fix *)
type _ Effect.t += Escape : Eio.Switch.t -> unit Effect.t

let the_great_escape =
  let open Effect.Shallow in
  let run () =
    Eio_main.run @@ fun _ ->
    Eio.Switch.run @@ fun sw -> Effect.perform (Escape sw)
  in
  continue_with (fiber run) ()
    {
      retc = (fun _ -> assert false);
      exnc = (fun _ -> assert false);
      effc =
        (fun (type a) (eff : a Effect.t) ->
          match eff with Escape sw -> Some (fun _ -> sw) | _ -> None);
    }

module Make (I : Cstubs_inverted.INTERNAL) = struct
  open Util.Make (I)

  let () =
    fn "repo_new"
      (config @-> returning repo)
      (fun config ->
        let (s, config) : config = Root.get_config config in
        let (module Store) = Irmin_cli.Resolver.Store.generic_keyed s in
        let remote = Irmin_cli.Resolver.Store.remote s in
        let repo : Store.repo =
          run (fun () -> Store.Repo.v ~sw:the_great_escape config)
        in
        Root.create_repo
          (module Store)
          {
            error = None;
            repo_mod =
              (module Store : Irmin.Generic_key.S with type repo = Store.repo);
            repo;
            remote;
          })

  let () =
    fn "repo_branches"
      (repo @-> returning branch_array)
      (fun (type repo) repo ->
        with_repo' repo branch_array
          (fun
            (module Store : Irmin.Generic_key.S with type repo = repo) repo ->
            let b = run (fun () -> Store.Repo.branches repo) in
            Root.create_branch_array (module Store) b))

  let () =
    fn "branch_array_length"
      (repo @-> branch_array @-> returning uint64_t)
      (fun (type repo) repo p ->
        with_repo repo UInt64.zero
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let arr = Root.get_branch_array (module Store) p in
            UInt64.of_int (Array.length arr)))

  let () =
    fn "branch_array_get"
      (repo @-> branch_array @-> uint64_t @-> returning irmin_string)
      (fun (type repo) repo p i ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let i = UInt64.to_int i in
            let arr = Root.get_branch_array (module Store) p in
            if i >= Array.length arr then failwith "index out of bounds"
            else
              let x = Array.unsafe_get arr i in
              Root.create_string (Irmin.Type.to_string Store.Branch.t x)))

  let () =
    fn "hash_equal"
      (repo @-> hash @-> hash @-> returning bool)
      (fun (type repo) repo a b ->
        with_repo repo false
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let a = Root.get_hash (module Store) a in
            let b = Root.get_hash (module Store) b in
            Irmin.Type.(unstage (equal Store.hash_t)) a b))

  let () =
    fn "contents_hash"
      (repo @-> contents @-> returning hash)
      (fun (type repo) repo a ->
        with_repo' repo hash
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let a = Root.get_contents (module Store) a in
            Root.create_hash (module Store) (Store.Contents.hash a)))

  let () =
    fn "contents_of_hash"
      (repo @-> hash @-> returning contents)
      (fun (type repo) repo a ->
        with_repo' repo contents
          (fun
            (module Store : Irmin.Generic_key.S with type repo = repo) repo ->
            let a = Root.get_hash (module Store) a in
            let c = run @@ fun () -> Store.Contents.of_hash repo a in
            match c with
            | Some c -> Root.create_contents (module Store) c
            | None -> null contents))

  let () =
    fn "contents_of_key"
      (repo @-> kinded_key @-> returning contents)
      (fun (type repo) repo a ->
        with_repo' repo contents
          (fun
            (module Store : Irmin.Generic_key.S with type repo = repo) repo ->
            let a = Root.get_kinded_key (module Store) a in
            match a with
            | `Contents (a, _) -> (
                let c = run @@ fun () -> Store.Contents.of_key repo a in
                match c with
                | Some c -> Root.create_contents (module Store) c
                | None -> null contents)
            | `Node _ -> null contents))

  let () =
    fn "contents_to_string"
      (repo @-> contents @-> returning irmin_string)
      (fun (type repo) repo contents ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let contents = Root.get_contents (module Store) contents in
            let s = Irmin.Type.to_string Store.contents_t contents in
            Root.create_string s))

  let () =
    fn "contents_of_string"
      (repo @-> ptr char @-> int64_t @-> returning contents)
      (fun (type repo) repo s length ->
        with_repo' repo contents
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let length = get_length length s in
            let s = string_from_ptr s ~length in
            let hash = Irmin.Type.of_string Store.contents_t s in
            match hash with
            | Ok hash -> Root.create_contents (module Store) hash
            | Error (`Msg e) -> failwith e))

  let () =
    fn "hash_to_string"
      (repo @-> hash @-> returning irmin_string)
      (fun (type repo) repo hash ->
        with_repo' repo irmin_string
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let hash = Root.get_hash (module Store) hash in
            let s = Irmin.Type.to_string Store.hash_t hash in
            Root.create_string s))

  let () =
    fn "hash_of_string"
      (repo @-> ptr char @-> int64_t @-> returning hash)
      (fun (type repo) repo s length ->
        with_repo' repo hash
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            let length = get_length length s in
            let s = string_from_ptr s ~length in
            let h = Irmin.Type.of_string Store.Hash.t s in
            match h with
            | Ok h -> Root.create_hash (module Store) h
            | Error (`Msg e) -> failwith e))

  let () =
    fn "metadata_default"
      (repo @-> returning metadata)
      (fun (type repo) repo ->
        with_repo' repo metadata
          (fun (module Store : Irmin.Generic_key.S with type repo = repo) _ ->
            Root.create_metadata (module Store) Store.Metadata.default))

  let () =
    fn "repo_has_error"
      (repo @-> returning bool)
      (fun repo ->
        let r = Root.get_repo repo in
        Option.is_some r.error)

  let () =
    fn "repo_get_error"
      (repo @-> returning irmin_string)
      (fun repo ->
        let r = Root.get_repo repo in
        match r.error with
        | Some x -> Root.create_string x
        | None -> null irmin_string)

  let () = fn "hash_free" (hash @-> returning void) free
  let () = fn "branch_array_free" (branch_array @-> returning void) free
  let () = fn "repo_free" (repo @-> returning void) free
  let () = fn "metadata_free" (metadata @-> returning void) free
  let () = fn "contents_free" (contents @-> returning void) free
end
