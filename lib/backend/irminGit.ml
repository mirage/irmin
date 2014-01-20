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

module Make (G: GitTypes.S) (K: IrminKey.S) (B: IrminBlob.S) = struct

  module V = IrminValue.S(K)(B)

  module RO = struct

    type t = G.t

    type key = K.t

    type value = V.t

    let create () =
      G.create ()

    let git_of_key key =
      GitTypes.SHA1.of_string (K.to_string key)

    let key_of_git key =
      K.of_string (GitTypes.SHA1.to_string key)

    let mem t key =
      G.mem t (git_of_key key)

    let blob_of_git b =
      B.of_string (GitTypes.Blob.to_string b)

    let tag_of_git t =
      B.of_string (GitTypes.Tag.to_string t)

    let tree_of_git t =
      let open GitTypes.Tree in
      let entries = GitTypes.Tree.entries t in
      let children =
        List.map ~f:(fun e -> e.name, key_of_git e.node) entries in
      let blob = match entries with
        | [ { perm = `normal | `exec } as b ] -> Some (key_of_git b.node)
        | _ -> None in
      { IrminTree.blob; children }

    let commit_of_git c =
      let open GitTypes.Commit in
      let c_of_git k = key_of_git (GitTypes.SHA1.of_commit k) in
      let t_of_git k = key_of_git (GitTypes.SHA1.of_tree k) in
      let parents = List.map ~f:c_of_git c.parents in
      let tree = Some (t_of_git c.tree) in
      { IrminCommit.tree; parents }

    let value_of_git v: V.t =
      let open GitTypes.Value in
      match v with
      | Blob b   -> IrminValue.Blob (blob_of_git b)
      | Tag t    -> IrminValue.Blob (tag_of_git t)
      | Tree t   -> IrminValue.Tree (tree_of_git t)
      | Commit c -> IrminValue.Commit (commit_of_git c)

    let read t key =
      G.read t (git_of_key key) >>= function
      | None   -> return_none
      | Some v -> return (Some (value_of_git v))

    let read_exn t key =
      read t key >>= function
      | None   -> fail Not_found
      | Some v -> return v

    let list t k =
      return [k]

    let contents t =
      G.list t >>= fun keys ->
      Lwt_list.map_s (fun k ->
          G.read_exn t k >>= fun v ->
          return (key_of_git k, value_of_git v)
        ) keys

  end


(*

  let blob_key_of_git t key =

  (* XXX: add a cache of git keys -> irmin keys *)
  let rec tree_of_git t tree =
    let open G.Tree in
    let entry_of_git e =
      match e.perm with
      | `dir ->
        let tree = tree_key_of_git t e.node in
        return (e.name, tree)
      | _    ->
        let blob = blob_key_of_git t e.node in
        Tree.tree t ~blob:e.node [] >>= fun blob ->
        return (e.name, blob) in
    Lwt_list.map entry_of_git t >>=
    Tree.t t children

  let git_of_key k =
    GitTypes.SHA1.of_string (K.to_string k)

  let value_of_git = function
    | GitTypes.Value.Blob b   -> IrminValue.Blob ()
    | GitTypes.Value.Tree t   -> assert false
    | GitTypes.Value.Commit c -> assert false
*)
end
