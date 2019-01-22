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


module type APPEND_ONLY_STORE = sig
  type 'a t
  type key
  type value
  val mem : [> `Read] t -> key -> bool Lwt.t
  val find: [> `Read] t -> key -> value option Lwt.t
  val add: [> `Write] t -> key -> value -> unit Lwt.t
end

module type APPEND_ONLY_STORE_MAKER = functor (K: Type.S) (V: Type.S) ->
sig
  include APPEND_ONLY_STORE with type key = K.t and type value = V.t
  val batch: [`Read] t -> ([`Read | `Write] t -> 'a Lwt.t) -> 'a Lwt.t
  val v: Conf.t -> [`Read] t Lwt.t
end

module Content_addressable (AO: APPEND_ONLY_STORE_MAKER)
    (K: S.HASH) (V: Type.S) =
struct
  include AO(K)(V)

  let pp_key = Type.pp K.t

  let digest v = K.digest (Type.encode_bin V.t v)

  let find t k =
    find t k >>= function
    | None        -> Lwt.return None
    | Some v as r ->
      let k' = digest v in
      if Type.equal K.t k k' then Lwt.return r
      else
        Fmt.kstrf Lwt.fail_invalid_arg
          "corrupted value: got %a, expecting %a" pp_key k' pp_key k

  let add t v =
    let k = digest v in
    add t k v >|= fun () ->
    k

end

module Make_ext
    (CA: S.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW: S.ATOMIC_WRITE_STORE_MAKER)
    (M: S.METADATA)
    (C: Contents.S)
    (P: Path.S)
    (B: Branch.S)
    (H: Hash.S)
    (N: S.NODE with type metadata = M.t
                and type hash = H.t
                and type step = P.step)
    (CT: S.COMMIT with type hash = H.t)
=
struct

  module X = struct
    module Hash = H
    module Val = Contents.String
    module Values = CA (Hash)(Val)
    module Make (V: Type.S) = struct
      module Key = H
      type 'a t = 'a Values.t
      type key = Key.t
      type value = V.t
      let add t v = Values.add t (Type.encode_bin V.t v)
      let find t k = Values.find t k >|= function
        | None -> None
        | Some v -> match Type.decode_bin V.t v with
          | Ok v -> Some v
          | _ -> None
      let mem t k =
        (* normally we should also check that [find] returns a value
           of the right type, but type mismatch here is not supposed to
           happen. *)
        Values.mem t k
    end
    module Contents = struct
      module CA = struct
        module Val = C
        include Make(Val)
      end
      include Contents.Store(CA)
    end

    module Node = struct
      module CA = struct
        module Val = N
        include Make(Val)
      end
      include Node.Store(Contents)(P)(M)(CA)
    end

    module Commit = struct
      module CA = struct
        module Val = CT
        include Make(Val)
      end
      include Commit.Store(Node)(CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      include AW (Key)(Val)
    end

    module Slice = Slice.Make(Contents)(Node)(Commit)
    module Sync = Sync.None(H)(B)
    module Repo = struct

      type t = {
        config: Conf.t;
        values: [`Read] Values.t;
        branch: Branch.t;
      }

      let contents_t t = t.values
      let node_t t = t.values, t.values
      let commit_t t = (t.values, t.values), t.values
      let branch_t t = t.branch

      let batch t f =
        Values.batch t.values @@ fun t ->
        let contents_t = t in
        let node_t = contents_t, t in
        let commit_t = node_t, t in
        f contents_t node_t commit_t

      let v config =
        Values.v config >>= fun values ->
        Branch.v config >|= fun branch ->
        { values; branch; config }

    end
  end
  include Store.Make(X)
end

module Make
    (CA: S.CONTENT_ADDRESSABLE_STORE_MAKER)
    (AW: S.ATOMIC_WRITE_STORE_MAKER)
    (M: S.METADATA)
    (C: S.CONTENTS)
    (P: S.PATH)
    (B: S.BRANCH)
    (H: S.HASH) =
struct
  module N = Node.Make(H)(P)(M)
  module CT = Commit.Make(H)
  include Make_ext(CA)(AW)(M)(C)(P)(B)(H)(N)(CT)
end

module Of_private = Store.Make

module type CONTENT_ADDRESSABLE_STORE = S.CONTENT_ADDRESSABLE_STORE
module type ATOMIC_WRITE_STORE = S.ATOMIC_WRITE_STORE
module type TREE = S.TREE
module type S = S.STORE

type config = Conf.t
type 'a diff = 'a Diff.t

module type CONTENT_ADDRESSABLE_STORE_MAKER = S.CONTENT_ADDRESSABLE_STORE_MAKER
module type ATOMIC_WRITE_STORE_MAKER = S.ATOMIC_WRITE_STORE_MAKER
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
