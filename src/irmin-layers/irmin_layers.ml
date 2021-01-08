(*
 * Copyright (c) 2013-2020 Ioana Cristescu <ioana@tarides.com>
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

open Lwt.Infix
include Irmin_layers_intf

module Layer_id = struct
  type t = layer_id [@@deriving irmin]

  let to_string = function
    | `Upper0 -> "upper0"
    | `Upper1 -> "upper1"
    | `Lower -> "lower"

  let pp = Fmt.of_to_string to_string
end

module Make_ext
    (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : Irmin.ATOMIC_WRITE_STORE_MAKER)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (N : Irmin.Private.Node.S
           with type metadata = M.t
            and type hash = H.t
            and type step = P.step)
    (CT : Irmin.Private.Commit.S with type hash = H.t) =
struct
  (* TODO: add check_closed *)

  module X = struct
    module Hash = H

    module Contents = struct
      module CA = struct
        module Key = Hash
        module Val = C
        module CA = CA (Key) (Val)
        module Layered_CA = Layered_store.Content_addressable (Key) (Val) (CA)
        include Layered_CA
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module CA = struct
        module Key = Hash
        module Val = N
        module CA = CA (Key) (Val)
        module Layered_CA = Layered_store.Content_addressable (Key) (Val) (CA)
        include Layered_CA
      end

      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = Hash
        module Val = CT
        module CA = CA (Key) (Val)
        module Layered_CA = Layered_store.Content_addressable (Key) (Val) (CA)
        include Layered_CA
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      include AW (Key) (Val)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.t;
        nodes : [ `Read ] Node.t;
        commits : [ `Read ] Commit.t;
        branch : Branch.t;
      }

      let contents_t t = t.contents
      let node_t t = t.nodes
      let commit_t t = t.commits
      let branch_t t = t.branch

      let batch t f =
        Contents.CA.batch t.contents @@ fun c ->
        Node.CA.batch (snd t.nodes) @@ fun n ->
        Commit.CA.batch (snd t.commits) @@ fun ct ->
        let contents_t = c in
        let node_t = (contents_t, n) in
        let commit_t = (node_t, ct) in
        f contents_t node_t commit_t

      let v config =
        Contents.CA.v config >>= fun contents ->
        Node.CA.v config >>= fun nodes ->
        Commit.CA.v config >>= fun commits ->
        let nodes = (contents, nodes) in
        let commits = (nodes, commits) in
        Branch.v config >|= fun branch ->
        { contents; nodes; commits; branch; config }

      let close t =
        Contents.CA.close t.contents >>= fun () ->
        Node.CA.close (snd t.nodes) >>= fun () ->
        Commit.CA.close (snd t.commits) >>= fun () -> Branch.close t.branch
    end
  end

  include Irmin.Of_private (X)

  let freeze ?min:_ ?max:_ ?squash:_ ?copy_in_upper:_ ?min_upper:_ ?recovery:_
      _repo =
    Lwt.fail_with "not implemented"

  type store_handle =
    | Commit_t : hash -> store_handle
    | Node_t : hash -> store_handle
    | Content_t : hash -> store_handle

  let layer_id _repo _store_handle = Lwt.fail_with "not implemented"
  let async_freeze _ = failwith "not implemented"
  let upper_in_use _repo = failwith "not implemented"
  let self_contained ?min:_ ~max:_ _repo = failwith "not implemented"
  let check_self_contained ?heads:_ _ = failwith "not implemented"
  let needs_recovery _ = failwith "not implemented"

  module PrivateLayer = struct
    module Hook = struct
      type 'a t = unit

      let v _ = failwith "not implemented"
    end

    let wait_for_freeze _ = Lwt.fail_with "not implemented"

    let freeze' ?min:_ ?max:_ ?squash:_ ?copy_in_upper:_ ?min_upper:_
        ?recovery:_ ?hook:_ _repo =
      Lwt.fail_with "not implemented"

    let upper_in_use = upper_in_use
  end
end

module Make
    (CA : Irmin.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW : Irmin.ATOMIC_WRITE_STORE_MAKER)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module N = Irmin.Private.Node.Make (H) (P) (M)
  module CT = Irmin.Private.Commit.Make (H)
  include Make_ext (CA) (AW) (M) (C) (P) (B) (H) (N) (CT)
end

module Stats = Stats
