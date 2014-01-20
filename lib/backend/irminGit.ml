(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Core_kernel.Std

module L = Log.Make(struct let section = "GIT" end)

module Make (G: GitTypes.S) (K: IrminKey.S) (B: IrminBlob.S) (R: IrminReference.S) = struct

  let git_of_key key =
    GitTypes.SHA1.of_string (K.to_string key)

  let key_of_git key =
    K.of_string (GitTypes.SHA1.to_string key)

  module XInternal = struct

    module type V = sig
      type t
      val type_eq: GitTypes.object_type -> bool
      val to_git: t -> [`Value of GitTypes.value | `Key of GitTypes.sha1]
      val of_git: GitTypes.sha1 -> GitTypes.value -> t option
    end

    module AO (V: V) = struct

      type t = G.t

      type key = K.t

      type value = V.t

      let create () =
        G.create ()

      let mem t key =
        let key = git_of_key key in
        G.mem t key >>= function
        | false    -> return false
        | true     ->
          G.type_of t key >>= function
          | None   -> return false
          | Some t -> return (V.type_eq t)

      let read t key =
        let key = git_of_key key in
        G.read t key >>= function
        | None   -> return_none
        | Some v -> return (V.of_git key v)

      let read_exn t key =
        read t key >>= function
        | None   -> fail Not_found
        | Some v -> return v

      let list t k =
        return [k]

      let contents t =
        G.list t >>= fun keys ->
        Lwt_list.fold_left_s (fun acc k ->
            G.read_exn t k >>= fun v ->
            match V.of_git k v with
            | None   -> return acc
            | Some v -> return ((key_of_git k, v) :: acc)
          ) [] keys

      let add t v =
        match V.to_git v with
        | `Key k   -> return (key_of_git k)
        | `Value v ->
          G.write t v >>= fun k ->
          return (key_of_git k)

    end

    module XBlob = AO (struct

        type t = B.t

        let type_eq = function
          | `Blob | `Tag -> true
          | _ -> false

        let mk_blob v =
          let buf = Git.output v in
          B.of_string (Mstruct.to_string buf)

        let of_git k b =
          match b with
          | GitTypes.Value.Blob _
          | GitTypes.Value.Tag _  -> Some (mk_blob b)
          | _                     -> None

        let to_git b =
          let buf = Mstruct.of_string (B.to_string b) in
          `Value (Git.input buf)

      end)

    module XTree = AO(struct

        type t = K.t IrminTree.t

        let type_eq = function
          | `Blob | `Tree -> true
          | _ -> false

        let of_git k = function
          | GitTypes.Value.Blob _ ->
            (* Create a dummy leaf node to hold blobs. *)
            let key = key_of_git k in
            Some { IrminTree.blob = Some key; children = [] }
          | GitTypes.Value.Tree t ->
            let entries = GitTypes.Tree.entries t in
            let children = List.map ~f:(fun e -> GitTypes.Tree.(e.name, key_of_git e.node)) entries in
            let blob = None in
            Some { IrminTree.blob; children }
          | _ -> None

        let to_git = function
          | { IrminTree.blob = Some key; children = [] } ->
            (* This is a dummy leaf node. Do nothing. *)
            `Key (git_of_key key)
          | { IrminTree.blob = None; children } ->
            let entries = List.map ~f:(fun (name, key) ->
                { GitTypes.Tree.perm = `dir; name; node = git_of_key key }
              ) children in
            let tree = GitTypes.Tree.create entries in
            `Value (GitTypes.Value.Tree tree)
          | _ -> failwith "not supported"

      end)

    module XCommit = AO(struct

        type t = K.t IrminCommit.t

        let type_eq = function
          | `Commit -> true
          | _ -> false

        let of_git k = function
          | GitTypes.Value.Commit { GitTypes.Commit.tree; parents } ->
            let commit_key_of_git k = key_of_git (GitTypes.SHA1.of_commit k) in
            let tree_key_of_git k = key_of_git (GitTypes.SHA1.of_tree k) in
            let parents = List.map ~f:commit_key_of_git parents in
            let tree = Some (tree_key_of_git tree) in
            Some { IrminCommit.tree; parents }
          | _ -> None

        let to_git { IrminCommit.tree; parents } =
          match tree with
          | None      -> failwith "not supported"
          | Some tree ->
            let git_of_commit_key k = GitTypes.SHA1.to_commit (git_of_key tree) in
            let git_of_tree_key k = GitTypes.SHA1.to_tree (git_of_key tree) in
            let tree = git_of_tree_key tree in
            let parents = List.map ~f:git_of_commit_key parents in
            let date = Float.to_string (Unix.gettimeofday ()) in
            let author = GitTypes.User.({ name = "irminsule"; email = ""; date }) in
            let message = "Created by Irminsule" in
            let commit = {
              GitTypes.Commit.tree; parents;
              author; committer = author;
              message } in
            `Value (GitTypes.Value.Commit commit)

      end)

    include IrminValue.Mux(K)(B)(XBlob)(XTree)(XCommit)

  end

  module XReference = struct

    type t = G.t

    type key = R.t

    type value = K.t

    let create () =
      G.create ()

    let ref_of_git r =
      R.of_string (GitTypes.Reference.to_string r)

    let git_of_ref r =
      GitTypes.Reference.of_string (R.to_string r)

    let mem t r =
      G.mem_reference t (git_of_ref r)

    let read t r =
      G.read_reference t (git_of_ref r) >>= function
      | None   -> return_none
      | Some k -> return (Some (key_of_git k))

    let read_exn t r =
      G.read_reference_exn t (git_of_ref r) >>= fun k ->
      return (key_of_git k)

    let list t _ =
      G.references t >>= fun refs ->
      return (List.map ~f:ref_of_git refs)

    let contents t =
      G.references t >>= fun refs ->
      Lwt_list.map_p (fun r ->
          G.read_reference_exn t r >>= fun k ->
          return (ref_of_git r, key_of_git k)
        ) refs

    let update t r k =
      G.write_reference t (git_of_ref r) (git_of_key k)

    let remove t r =
      G.remove_reference t (git_of_ref r)

    module Key = R

    module Value = K

  end

  include Irmin.Make(K)(B)(R)(XInternal)(XReference)

end

module Simple = Make(GitLocal)(IrminKey.SHA1)(IrminBlob.Simple)(IrminReference.Simple)
