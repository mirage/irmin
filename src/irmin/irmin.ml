(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Type = Type
module Diff = Diff

module Contents = struct
  include Contents
  module type S = S.CONTENTS
  module type STORE = S.CONTENTS_STORE
end
module Merge = Merge
module Branch = struct
  include Branch
  module type S = S.BRANCH
  module type STORE = S.BRANCH_STORE
end
module Info = Info
module Dot = Dot.Make
module Hash = struct
  include Hash
  module type S = S.HASH
end
module Path = struct
  include Path
  module type S = S.PATH
end

module Make_ext
    (CA: S.CONTENT_ADDRESSABLE_MAKER)
    (RW: S.READ_WRITE_MAKER)
    (M: S.METADATA)
    (C: Contents.S)
    (P: Path.S)
    (B: Branch.S)
    (H: Hash.S)
    (N: S.NODE with type metadata = M.t
                and type contents = H.t
                and type node = H.t
                and type step = P.step)
    (CT: S.COMMIT with type node = H.t and type commit = H.t)
=
struct

  module X = struct
    module Hash = H
    module Value = struct
      type t =
        | Contents of C.t
        | Node of N.t
        | Commit of CT.t
      let t =
        let open Type in
        variant "value" (fun c n ct -> function
            | Contents x -> c x
            | Node x -> n x
            | Commit x -> ct x)
        |~ case1 "contents" C.t (fun x -> Contents x)
        |~ case1 "node" N.t (fun x -> Node x)
        |~ case1 "commit" CT.t (fun x -> Commit x)
        |> sealv
    end
    module Values = CA(Hash)(Value)
    module Contents = struct
      module AO = struct
        module Key = H
        module Val = C

        type t = Values.t
        type key = Key.t
        type value = Val.t
        type batch = Values.batch

        let add t k v = Values.add t k (Contents v)
        let find t k = Values.find t k >|= function
          | Some (Contents c) -> Some c
          | _ -> None
        let mem t k =
          Values.mem t k >>= function
          | false -> Lwt.return false
          | true  -> find t k >|= function
            | Some _ -> true
            | None -> false
      end
      include Contents.Store(AO)
    end
    module Node = struct
      module AO = struct
        module Key = H
        module Val = N

        type t = Values.t
        type key = Key.t
        type value = Val.t
        type batch = Values.batch

        let add t k v = Values.add t k (Node v)
        let find t k = Values.find t k >|= function
          | Some (Node c) -> Some c
          | _ -> None
        let mem t k =
          Values.mem t k >>= function
          | false -> Lwt.return false
          | true  -> find t k >|= function
            | Some _ -> true
            | None -> false
      end
      include Node.Store(Contents)(P)(M)(AO)
    end
    module Commit = struct
      module AO = struct
        module Key = H
        module Val = CT

        type t = Values.t
        type key = Key.t
        type value = Val.t
        type batch = Values.batch

        let add t k v = Values.add t k (Commit v)
        let find t k = Values.find t k >|= function
          | Some (Commit c) -> Some c
          | _ -> None
        let mem t k =
          Values.mem t k >>= function
          | false -> Lwt.return false
          | true  -> find t k >|= function
            | Some _ -> true
            | None -> false
      end
      include Commit.Store(Node)(AO)
    end
    module Branch = struct
      module Key = B
      module Val = H
      include RW (Key)(Val)
    end
    module Slice = Slice.Make(Contents)(Node)(Commit)
    module Sync = Sync.None(H)(B)
    module Repo = struct
      type t = {
        config : Conf.t;
        values : Values.t;
        branch : Branch.t;
      }

      let contents_t t = t.values
      let node_t t = t.values, t.values
      let commit_t t = (t.values, t.values), t.values
      let branch_t t = t.branch

      let v config =
        Values.v config >>= fun values ->
        Branch.v config >|= fun branch ->
        { values; branch; config }

      let batch t f = Values.batch t.values (fun t ->
          let contents_t = t in
          let node_t = contents_t, t in
          let commit_t = node_t, t in
          f contents_t node_t commit_t)

    end
  end
  include Store.Make(X)
end


module Make
    (CA: S.CONTENT_ADDRESSABLE_MAKER)
    (RW: S.READ_WRITE_MAKER)
    (M: S.METADATA)
    (C: S.CONTENTS)
    (P: S.PATH)
    (B: S.BRANCH)
    (H: S.HASH) =
struct
  module N = Node.Make(H)(H)(P)(M)
  module CT = Commit.Make(H)(H)
  include Make_ext(CA)(RW)(M)(C)(P)(B)(H)(N)(CT)
end

module Of_private = Store.Make

module type READ_ONLY = S.READ_ONLY
module type CONTENT_ADDRESSABLE = S.CONTENT_ADDRESSABLE
module type READ_WRITE = S.READ_WRITE
module type TREE = S.TREE
module type S = S.STORE

type config = Conf.t
type 'a diff = 'a Diff.t

module type CONTENT_ADDRESSABLE_MAKER = S.CONTENT_ADDRESSABLE_MAKER
module type READ_WRITE_MAKER = S.READ_WRITE_MAKER
module type S_MAKER = S.MAKER

module type KV =
  S with type key = string list
     and type step = string
     and type branch = string

module type KV_MAKER = functor (C: Contents.S) -> KV with type contents = C.t

module Private = struct
  module Conf = Conf
  module Node = struct
    include Node
    module type S = S.NODE
    module type GRAPH = S.NODE_GRAPH
    module type STORE = S.NODE_STORE
  end
  module Commit = struct
    include Commit
    module type S = S.COMMIT
    module type STORE = S.COMMIT_STORE
    module type HISTORY = S.COMMIT_HISTORY
  end
  module Slice = struct
    include Slice
    module type S = S.SLICE
  end
  module Sync = struct
    include Sync
    module type S = S.SYNC
  end
  module type S = S.PRIVATE
  module Watch = Watch
  module Lock = Lock
end

let version = Version.current

module type SYNC = S.SYNC_STORE
module Sync = Sync_ext.Make

type remote = S.remote = ..

let remote_store (type t) (module M: S with type t = t) (t:t) =
  let module X = (M: S.STORE with type t = t) in
  Sync_ext.remote_store (module X) t

module Metadata = struct
  module type S = S.METADATA
  module None = Node.No_metadata
end

module Json_tree = Contents.Json_tree
