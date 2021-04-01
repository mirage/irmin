(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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
module Conf = Conf
module Metadata = Metadata
module Branch = Branch
module Reference = Reference

let config = Conf.v

type reference = Reference.t [@@deriving irmin]

module type S = sig
  module Git : Git.S
  include Irmin.S with type metadata = Metadata.t and type hash = Git.Hash.t

  val git_commit : Repo.t -> commit -> Git.Value.Commit.t option Lwt.t
  val git_of_repo : Repo.t -> Git.t

  val repo_of_git :
    ?head:Git.Reference.t ->
    ?bare:bool ->
    ?lock:Lwt_mutex.t ->
    Git.t ->
    Repo.t Lwt.t
end

module type G = Private.G

module Make_ext
    (G : G)
    (S : Git.Sync.S with type hash := G.hash and type store := G.t)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Branch.S) =
struct
  module P = Private.Make (G) (S) (C) (P) (B)
  include Irmin.Of_private (P)

  let git_of_repo = P.git_of_repo
  let repo_of_git = P.repo_of_git

  let git_commit (repo : Repo.t) (h : commit) : G.Value.Commit.t option Lwt.t =
    let h = Commit.hash h in
    G.read (git_of_repo repo) h >|= function
    | Ok (Git.Value.Commit c) -> Some c
    | _ -> None

  module Git = G
end

module Mem = struct
  include Git.Mem.Store

  let confs = Hashtbl.create 10
  let find_conf c = Hashtbl.find_opt confs c

  let add_conf c t =
    Hashtbl.replace confs c t;
    t

  let v' ?dotgit root = v ?dotgit root

  let v ?dotgit root =
    let conf = (dotgit, root) in
    match find_conf conf with
    | Some x -> Lwt.return x
    | None -> v' ?dotgit root >|= add_conf conf
end

module Make
    (G : G)
    (S : Git.Sync.S with type hash = G.hash and type store = G.t)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
  Make_ext (G) (S) (C) (P) (Branch.Make (B))

module No_sync (G : Git.S) = struct
  type hash = G.hash
  type store = G.t

  type error =
    [ `Not_found | `Msg of string | `Exn of exn | `Cycle | `Invalid_flow ]

  let pp_error _ _ = assert false

  let fetch ?push_stdout:_ ?push_stderr:_ ~ctx:_ _ _ ?version:_ ?capabilities:_
      ?deepen:_ _ =
    assert false

  let push ~ctx:_ _ _ ?version:_ ?capabilities:_ _ = assert false
end

module Content_addressable (G : Git.S) = struct
  module type S = Irmin.Content_addressable.S with type key = G.Hash.t

  module Make (V : Irmin.Type.S) = struct
    module G = struct
      include G

      let v ?dotgit:_ _root = assert false
    end

    module V = struct
      include V

      let merge = Irmin.Merge.default Irmin.Type.(option V.t)
    end

    module M =
      Make_ext (G) (No_sync (G)) (V) (Irmin.Path.String_list) (Reference)

    module X = M.Private.Contents

    let state t =
      let+ r = M.repo_of_git t in
      M.Private.Repo.contents_t r

    type 'a t = G.t
    type key = X.key
    type value = X.value

    let with_state0 f t =
      let* t = state t in
      f t

    let with_state1 f t x =
      let* t = state t in
      f t x

    let with_state1_rw f t x =
      with_state1 (fun t v -> X.batch t (fun t -> f t v)) t x

    let add = with_state1_rw X.add
    let pp_key = Irmin.Type.pp X.Key.t
    let equal_key = Irmin.Type.(unstage (equal X.Key.t))

    let unsafe_add t k v =
      let+ k' = with_state1_rw X.add t v in
      if equal_key k k' then ()
      else
        Fmt.failwith
          "[Git.unsafe_append] %a is not a valid key. Expecting %a instead.\n"
          pp_key k pp_key k'

    let find = with_state1 X.find
    let mem = with_state1 X.mem
    let clear = with_state0 X.clear
    let close = with_state0 X.close
    let batch t f = f t
  end
end

module Atomic_write (G : Git.S) = struct
  module type S = Irmin.Atomic_write.S with type value = G.Hash.t

  module Make (K : Irmin.Branch.S) = struct
    module K = struct
      include K

      let master =
        match Irmin.Type.of_string K.t "master" with
        | Ok x -> x
        | Error (`Msg e) -> failwith e
    end

    module AW = Atomic_write.Make (Branch.Make (K)) (G)
    include Irmin.Atomic_write.Wrap_close (AW)
  end
end

module KV
    (G : G)
    (S : Git.Sync.S with type hash = G.hash and type store = G.t)
    (C : Irmin.Contents.S) =
  Make (G) (S) (C) (Irmin.Path.String_list) (Irmin.Branch.String)

module Ref
    (G : G)
    (S : Git.Sync.S with type hash = G.hash and type store = G.t)
    (C : Irmin.Contents.S) =
  Make_ext (G) (S) (C) (Irmin.Path.String_list) (Reference)

module type Maker = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  (P : Irmin.Path.S)
  (B : Irmin.Branch.S)
  ->
  S
    with type key = P.t
     and type step = P.step
     and module Key = P
     and type contents = C.t
     and type branch = B.t
     and module Git = G
     and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module type KV_maker = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = string
     and module Git = G
     and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

module type Ref_maker = functor
  (G : G)
  (S : Git.Sync.S with type hash = G.hash and type store = G.t)
  (C : Irmin.Contents.S)
  ->
  S
    with type key = string list
     and type step = string
     and type contents = C.t
     and type branch = Reference.t
     and module Git = G
     and type Private.Remote.endpoint = Mimic.ctx * Smart_git.Endpoint.t

include Conf

module Generic
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S) =
struct
  (* We use a dummy store to get the serialisation functions. This is
     probably not necessary and we could use Git.Value.Raw instead. *)
  module G = Mem
  module S = Make (G) (No_sync (G)) (C) (P) (B)

  include
    Irmin.Make_ext (CA) (AW) (S.Private.Node.Metadata) (S.Private.Contents.Val)
      (S.Private.Node.Path)
      (S.Branch)
      (S.Private.Hash)
      (S.Private.Node.Val)
      (S.Private.Commit.Val)
end

module Generic_KV
    (CA : Irmin.Content_addressable.Maker)
    (AW : Irmin.Atomic_write.Maker)
    (C : Irmin.Contents.S) =
  Generic (CA) (AW) (C) (Irmin.Path.String_list) (Irmin.Branch.String)
