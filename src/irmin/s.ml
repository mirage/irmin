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

(** Irmin signatures *)

open Result

module type S0 = sig
  type t
  val t: t Type.t
end

module type CONV = sig
  include S0
  val pp: t Fmt.t
  val of_string: string -> (t, [`Msg of string]) result
end

module type PATH = sig
  type t
  val pp: t Fmt.t
  val of_string: string -> (t, [`Msg of string]) result
  type step
  val empty: t
  val v: step list -> t
  val is_empty: t -> bool
  val cons: step -> t -> t
  val rcons: t -> step -> t
  val decons: t -> (step * t) option
  val rdecons: t -> (t * step) option
  val map: t -> (step -> 'a) -> 'a list
  val pp_step: step Fmt.t
  val step_of_string: string -> (step, [`Msg of string]) result
  val t: t Type.t
  val step_t: step Type.t
end

module type HASH = sig
  type t
  val pp: t Fmt.t
  val of_string: string -> (t, [`Msg of string]) result
  val digest: 'a Type.t -> 'a -> t
  val has_kind: [> `SHA1] -> bool
  val to_raw: t -> Cstruct.t
  val of_raw: Cstruct.t -> t
  val to_raw_int: t -> int
  val digest_size: int
  val t: t Type.t
end

module type CONTENTS = sig
  include CONV
  val merge: t option Merge.t
end

module type RO = sig
  type t
  type key
  type value
  val mem: t -> key -> bool Lwt.t
  val find: t -> key -> value option Lwt.t
end

module type RO_MAKER =
  functor (K: S0) ->
  functor (V: S0) ->
    RO with type key = K.t and type value = V.t

module type AO = sig
  include RO
  val add: t -> value -> key Lwt.t
end

module type AO_MAKER = functor (K: HASH) -> functor (V: CONV) ->
sig
  include AO with type key = K.t and type value = V.t
  val v: Conf.t -> t Lwt.t
end

module type METADATA = sig
  include S0
  val merge: t Merge.t
  val default: t
end

module type CONTENTS_STORE = sig
  include AO
  val merge: t -> key option Merge.t
  module Key: HASH with type t = key
  module Val: CONTENTS with type t = value
end

module type NODE = sig
  type t
  type metadata
  type contents
  type node
  type step
  type value = [ `Node of node | `Contents of contents * metadata ]
  val v: (step * value) list -> t
  val list: t -> (step * value) list
  val empty: t
  val is_empty: t -> bool
  val find: t -> step -> value option
  val update: t -> step -> value -> t
  val remove: t -> step -> t
  val t: t Type.t
  val metadata_t: metadata Type.t
  val contents_t: contents Type.t
  val node_t: node Type.t
  val step_t: step Type.t
  val value_t: value Type.t
end

module type NODE_GRAPH = sig
  type t
  type metadata
  type contents
  type node
  type step
  type path
  type value = [ `Node of node | `Contents of contents * metadata ]
  val empty: t -> node Lwt.t
  val v: t -> (step * value) list -> node Lwt.t
  val list: t -> node -> (step * value) list Lwt.t
  val find: t -> node -> path -> value option Lwt.t
  val update: t -> node -> path -> value -> node Lwt.t
  val remove: t -> node -> path -> node Lwt.t
  val closure: t -> min:node list -> max:node list -> node list Lwt.t
  val metadata_t: metadata Type.t
  val contents_t: contents Type.t
  val node_t: node Type.t
  val step_t: step Type.t
  val path_t: path Type.t
  val value_t: value Type.t
end

module type NODE_STORE = sig
  include AO
  module Path: PATH
  val merge: t -> key option Merge.t
  module Key: HASH with type t = key
  module Metadata: METADATA
  module Val: NODE
    with type t = value
     and type node = key
     and type metadata = Metadata.t
     and type step = Path.step
  module Contents: CONTENTS_STORE with type key = Val.contents
end

type config = Conf.t
type 'a diff = 'a Diff.t

module type COMMIT = sig
  type t
  type commit
  type node
  val v: info:Info.t -> node:node -> parents:commit list -> t
  val node: t -> node
  val parents: t -> commit list
  val info: t -> Info.t
  val t: t Type.t
  val commit_t: commit Type.t
  val node_t: node Type.t
end

module type COMMIT_STORE = sig
  include AO
  val merge: t -> info:Info.f -> key option Merge.t
  module Key: HASH with type t = key
  module Val: COMMIT
    with type t = value
     and type commit = key
  module Node: NODE_STORE with type key = Val.node
end

module type COMMIT_HISTORY = sig
  type t
  type node
  type commit
  type v
  val v: t -> node:node -> parents:commit list -> info:Info.t -> (commit * v) Lwt.t
  val parents: t -> commit -> commit list Lwt.t
  val merge: t -> info:Info.f -> commit Merge.t
  val lcas: t -> ?max_depth:int -> ?n:int -> commit -> commit ->
    (commit list, [`Max_depth_reached | `Too_many_lcas]) result Lwt.t
  val lca: t -> info:Info.f -> ?max_depth:int -> ?n:int -> commit list ->
    (commit option, Merge.conflict) result Lwt.t
  val three_way_merge: t -> info:Info.f -> ?max_depth:int -> ?n:int ->
    commit -> commit -> (commit, Merge.conflict) result Lwt.t
  val closure: t -> min:commit list -> max:commit list -> commit list Lwt.t
  val commit_t: commit Type.t
end

module type LINK = sig
  include AO
  val add: t -> key -> value -> unit Lwt.t
end

module type LINK_MAKER =
  functor (K: HASH) -> sig
    include LINK with type key = K.t and type value = K.t
    val v: Conf.t -> t Lwt.t
  end

module type SLICE = sig
  type t
  type contents
  type node
  type commit
  type value = [ `Contents of contents | `Node of node | `Commit of commit ]
  val empty: unit -> t Lwt.t
  val add: t -> value -> unit Lwt.t
  val iter: t -> (value -> unit Lwt.t) -> unit Lwt.t
  val t: t Type.t
  val contents_t: contents Type.t
  val node_t: node Type.t
  val commit_t: commit Type.t
  val value_t: value Type.t
end

module type BRANCH = sig
  include CONV
  val master: t
  val is_valid: t -> bool
end

(** Read-write stores. *)
module type RW = sig
  include RO
  val set: t -> key -> value -> unit Lwt.t
  val test_and_set:
    t -> key -> test:value option -> set:value option -> bool Lwt.t
  val remove: t -> key -> unit Lwt.t
  val list: t -> key list Lwt.t
  type watch
  val watch:
    t -> ?init:(key * value) list -> (key -> value Diff.t -> unit Lwt.t)
    -> watch Lwt.t
  val watch_key: t -> key -> ?init:value -> (value Diff.t -> unit Lwt.t)
    -> watch Lwt.t
  val unwatch: t -> watch -> unit Lwt.t
end

module type RW_MAKER = functor (K: CONV) -> functor (V: CONV) ->
sig
  include RW with type key = K.t and type value = V.t
  val v: Conf.t -> t Lwt.t
end

module type BRANCH_STORE = sig
  include RW
  val list: t -> key list Lwt.t
  module Key: BRANCH with type t = key
  module Val: HASH with type t = value
end

module type SYNC = sig
  type t
  type commit
  type branch
  val fetch: t -> ?depth:int -> uri:string -> branch ->
    (commit, [`No_head | `Not_available | `Msg of string]) result Lwt.t
  val push: t -> ?depth:int -> uri:string -> branch ->
    (unit, [`No_head | `Not_available | `Msg of string | `Detached_head])
      result Lwt.t
end

module type PRIVATE = sig
  module Contents: CONTENTS_STORE
  module Node: NODE_STORE with type Val.contents = Contents.key
  module Commit: COMMIT_STORE with type Val.node = Node.key
  module Branch: BRANCH_STORE with type value = Commit.key
  module Slice: SLICE
     with type contents = Contents.key * Contents.value
     and type node = Node.key * Node.value
     and type commit = Commit.key * Commit.value
  module Repo: sig
    type t
    val v: Conf.t -> t Lwt.t
    val contents_t: t -> Contents.t
    val node_t: t -> Node.t
    val commit_t: t -> Commit.t
    val branch_t: t -> Branch.t
  end
  module Sync: sig
    include SYNC
      with type commit = Commit.key and type branch = Branch.key
    val v: Repo.t -> t Lwt.t
  end
end

module type TREE = sig
  type key
  type step
  type metadata
  type contents
  type node
  type tree = [ `Node of node | `Contents of contents * metadata ]
  val empty: tree
  val of_contents: ?metadata:metadata -> contents -> tree
  val of_node: node -> tree
  val kind: tree -> key -> [`Contents | `Node] option Lwt.t
  val list: tree -> key -> (step * [`Contents | `Node]) list Lwt.t
  val diff: tree -> tree -> (key * (contents * metadata) diff) list Lwt.t
  val mem: tree -> key -> bool Lwt.t
  val find_all: tree -> key -> (contents * metadata) option Lwt.t
  val find: tree -> key -> contents option Lwt.t
  val get_all: tree -> key -> (contents * metadata) Lwt.t
  val get: tree -> key -> contents Lwt.t
  val add: tree -> key -> ?metadata:metadata -> contents -> tree Lwt.t
  val remove: tree -> key -> tree Lwt.t
  val mem_tree: tree -> key -> bool Lwt.t
  val find_tree: tree -> key -> tree option Lwt.t
  val get_tree: tree -> key -> tree Lwt.t
  val add_tree: tree -> key -> tree -> tree Lwt.t
  val merge: tree Merge.t

  type concrete =
    [ `Tree of (step * concrete) list
    | `Contents of contents * metadata ]
  val of_concrete: concrete -> tree
  val to_concrete: tree -> concrete Lwt.t
end

module type STORE = sig
  type repo
  type t
  type step
  type key
  type metadata
  type contents
  type node
  type tree = [`Node of node | `Contents of contents * metadata]
  type commit
  type branch
  type slice
  type lca_error = [`Max_depth_reached | `Too_many_lcas]
  type ff_error = [`No_change | `Rejected | lca_error]
  module Repo: sig
    type t = repo
    val v: config -> t Lwt.t
    val heads: t -> commit list Lwt.t
    val branches: t -> branch list Lwt.t
    val export: ?full:bool -> ?depth:int ->
      ?min:commit list -> ?max:commit list ->
      t -> slice Lwt.t
    val import: t -> slice -> (unit, [`Msg of string]) result Lwt.t
  end
  val empty: Repo.t -> t Lwt.t
  val master: Repo.t -> t Lwt.t
  val of_branch: Repo.t -> branch -> t Lwt.t
  val of_commit: commit -> t Lwt.t
  val repo: t -> Repo.t
  val tree: t -> tree Lwt.t
  module Status: sig
    type t = [ `Empty | `Branch of branch | `Commit of commit ]
    val t: Repo.t -> t Type.t
    val pp: t Fmt.t
    val of_string: Repo.t -> string -> (t, [`Msg of string]) result
  end
  val status: t -> Status.t
  module Head: sig
    val list: Repo.t -> commit list Lwt.t
    val find: t -> commit option Lwt.t
    val get: t -> commit Lwt.t
    val set: t -> commit -> unit Lwt.t
    val fast_forward: t -> ?max_depth:int -> ?n:int -> commit ->
      (unit, ff_error) result Lwt.t
    val test_and_set:
      t -> test:commit option -> set:commit option -> bool Lwt.t
    val merge: into:t -> info:Info.f -> ?max_depth:int -> ?n:int -> commit ->
      (unit, Merge.conflict) result Lwt.t
  end
  module Commit: sig
    type t = commit
    val t: Repo.t -> t Type.t
    val pp: t Fmt.t
    val of_string: Repo.t -> string -> (t, [`Msg of string]) result
    val v: Repo.t -> info:Info.t -> parents:commit list -> tree -> commit Lwt.t
    val tree: commit -> tree Lwt.t
    val parents: commit -> commit list Lwt.t
    val info: commit -> Info.t
    module Hash: HASH
    type hash = Hash.t
    val hash: commit -> hash
    val of_hash: Repo.t -> hash -> commit option Lwt.t
  end
  module Contents: sig
    include CONTENTS with type t = contents
    module Hash: HASH
    type hash = Hash.t
    val hash: Repo.t -> contents -> hash Lwt.t
    val of_hash: Repo.t -> hash -> contents option Lwt.t
  end
  module Tree: sig
    include TREE with type step := step
                  and type key := key
                  and type metadata := metadata
                  and type contents := contents
                  and type node := node
                  and type tree := tree
    module Hash: HASH
    type hash = [`Node of Hash.t | `Contents of Contents.Hash.t * metadata]
    val hash_t: hash Type.t
    val hash: Repo.t -> tree -> hash Lwt.t
    val of_hash: Repo.t -> hash -> tree option Lwt.t
  end

  val kind: t -> key -> [`Contents | `Node] option Lwt.t
  val list: t -> key -> (step * [`Contents | `Node]) list Lwt.t
  val mem: t -> key -> bool Lwt.t
  val mem_tree: t -> key -> bool Lwt.t
  val find_all: t -> key -> (contents * metadata) option Lwt.t
  val find: t -> key -> contents option Lwt.t
  val get_all: t -> key -> (contents * metadata) Lwt.t
  val get: t -> key -> contents Lwt.t
  val find_tree: t -> key -> tree option Lwt.t
  val get_tree: t -> key -> tree Lwt.t
  type 'a transaction =
    ?allow_empty:bool -> ?strategy:[`Set | `Test_and_set | `Merge] ->
    ?max_depth:int -> ?n:int -> info:Info.f -> 'a -> unit Lwt.t
  val with_tree: t -> key -> (tree option -> tree option Lwt.t) transaction
  val set: t -> key -> ?metadata:metadata -> contents transaction
  val set_tree: t -> key -> tree transaction
  val remove: t -> key transaction
  val clone: src:t -> dst:branch -> t Lwt.t
  type watch
  val watch:
    t -> ?init:commit -> (commit diff -> unit Lwt.t) -> watch Lwt.t
  val watch_key: t -> key -> ?init:commit ->
    ((commit * tree) diff -> unit Lwt.t) -> watch Lwt.t
  val unwatch: watch -> unit Lwt.t

  type 'a merge = info:Info.f -> ?max_depth:int -> ?n:int -> 'a ->
    (unit, Merge.conflict) result Lwt.t
  val merge: into:t -> t merge
  val merge_with_branch: t -> branch merge
  val merge_with_commit: t -> commit merge

  val lcas: ?max_depth:int -> ?n:int -> t -> t ->
    (commit list, lca_error) result Lwt.t
  val lcas_with_branch: t -> ?max_depth:int -> ?n:int -> branch ->
    (commit list, lca_error) result Lwt.t
  val lcas_with_commit: t -> ?max_depth:int -> ?n:int -> commit ->
    (commit list, lca_error) result Lwt.t
  module History: Graph.Sig.P with type V.t = commit
  val history:
    ?depth:int -> ?min:commit list -> ?max:commit list -> t ->
    History.t Lwt.t
  module Branch: sig
    val mem: Repo.t -> branch -> bool Lwt.t
    val find: Repo.t -> branch -> commit option Lwt.t
    val get: Repo.t -> branch -> commit Lwt.t
    val set: Repo.t -> branch -> commit -> unit Lwt.t
    val remove: Repo.t -> branch -> unit Lwt.t
    val list: Repo.t -> branch list Lwt.t
    val watch:
      Repo.t -> branch -> ?init:commit -> (commit diff -> unit Lwt.t)
      -> watch Lwt.t
    val watch_all:
      Repo.t ->
      ?init:(branch * commit) list -> (branch -> commit diff -> unit Lwt.t)
      -> watch Lwt.t
    include BRANCH with type t = branch
  end
  module Key: PATH with type t = key and type step = step
  module Metadata: METADATA with type t = metadata

  val step_t: step Type.t
  val key_t: key Type.t
  val metadata_t: metadata Type.t
  val contents_t: contents Type.t
  val node_t: node Type.t
  val tree_t: tree Type.t
  val commit_t: Repo.t -> commit Type.t
  val branch_t: branch Type.t
  val slice_t: slice Type.t
  val kind_t: [`Contents | `Node] Type.t
  val lca_error_t: lca_error Type.t
  val ff_error_t: ff_error Type.t

  module Private: sig
    include PRIVATE
      with type Contents.value = contents
       and module Node.Path = Key
       and type Commit.key = Commit.Hash.t
       and type Node.Metadata.t = metadata
       and type Node.key = Tree.Hash.t
       and type Contents.key = Contents.Hash.t
       and type Branch.key = branch
       and type Slice.t = slice
       and type Repo.t = repo
  end
end

module type MAKER =
  functor (M: METADATA) ->
  functor (C: CONTENTS) ->
  functor (P: PATH) ->
  functor (B: BRANCH) ->
  functor (H: HASH) ->
    STORE
  with type key = P.t
   and type step = P.step
   and module Key = P
   and type metadata = M.t
   and type contents = C.t
   and type branch = B.t
   and type Commit.Hash.t = H.t
   and type Tree.Hash.t = H.t
   and type Contents.Hash.t = H.t

type remote =
  | Store: (module STORE with type t = 'a) * 'a -> remote
  | URI of string

module type SYNC_STORE = sig
  type db
  type commit
  type fetch_error = [
    | `No_head
    | `Not_available
    | `Msg of string
  ]
  val pp_fetch_error: fetch_error Fmt.t
  type push_error = [ fetch_error | `Detached_head ]
  val fetch: db -> ?depth:int -> remote ->
    (commit, fetch_error) result Lwt.t
  val fetch_exn: db -> ?depth:int -> remote -> commit Lwt.t
  val pull: db -> ?depth:int -> remote -> [`Merge of Info.f|`Set] ->
    (unit, [fetch_error | Merge.conflict]) result Lwt.t
  val pull_exn: db -> ?depth:int -> remote -> [`Merge of Info.f|`Set] ->
    unit Lwt.t
  val pp_push_error: push_error Fmt.t
  val push: db -> ?depth:int -> remote -> (unit, push_error) result Lwt.t
  val push_exn: db -> ?depth:int -> remote -> unit Lwt.t
end
