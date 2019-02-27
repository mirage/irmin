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

module type PATH = sig
  type t
  type step
  val empty: t
  val v: step list -> t
  val is_empty: t -> bool
  val cons: step -> t -> t
  val rcons: t -> step -> t
  val decons: t -> (step * t) option
  val rdecons: t -> (t * step) option
  val map: t -> (step -> 'a) -> 'a list
  val t: t Type.t
  val step_t: step Type.t
end

module type HASH = sig
  type t
  val digest: string -> t
  val hash: t -> int
  val digest_size: int
  val t: t Type.t
end

module type CONTENTS = sig
  include Type.S
  val merge: t option Merge.t
end

module type CONTENT_ADDRESSABLE_STORE = sig
  type 'a t
  type key
  type value
  val mem : [> `Read] t -> key -> bool Lwt.t
  val find: [> `Read] t -> key -> value option Lwt.t
  val add : [> `Write] t -> value -> key Lwt.t
end

module type CONTENT_ADDRESSABLE_STORE_MAKER = functor (K: HASH) (V: Type.S) ->
sig
  include CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t
  val batch: [`Read] t -> ([`Read | `Write] t -> 'a Lwt.t) -> 'a Lwt.t
  val v: Conf.t -> [`Read] t Lwt.t
end

module type METADATA = sig
  include Type.S
  val merge: t Merge.t
  val default: t
end

module type CONTENTS_STORE = sig
  include CONTENT_ADDRESSABLE_STORE
  val merge: [`Read | `Write] t -> key option Merge.t
  module Key: sig
    include HASH with type t = key
    val digest: value -> t
  end
  module Val: CONTENTS with type t = value
end

module type NODE = sig
  type t
  type metadata
  type hash
  type step
  type value = [ `Node of hash | `Contents of hash * metadata ]
  val v: (step * value) list -> t
  val list: t -> (step * value) list
  val empty: t
  val is_empty: t -> bool
  val find: t -> step -> value option
  val update: t -> step -> value -> t
  val remove: t -> step -> t
  val t: t Type.t
  val metadata_t: metadata Type.t
  val hash_t: hash Type.t
  val step_t: step Type.t
  val value_t: value Type.t
end

module type NODE_GRAPH = sig
  type 'a t
  type metadata
  type contents
  type node
  type step
  type path
  type value = [ `Node of node | `Contents of contents * metadata ]
  val empty: [> `Write] t -> node Lwt.t
  val v: [> `Write] t -> (step * value) list -> node Lwt.t
  val list: [> `Read] t -> node -> (step * value) list Lwt.t
  val find: [> `Read] t -> node -> path -> value option Lwt.t
  val update: [`Read | `Write] t -> node -> path -> value -> node Lwt.t
  val remove: [`Read | `Write] t -> node -> path -> node Lwt.t
  val closure: [> `Read] t -> min:node list -> max:node list -> node list Lwt.t
  val metadata_t: metadata Type.t
  val contents_t: contents Type.t
  val node_t: node Type.t
  val step_t: step Type.t
  val path_t: path Type.t
  val value_t: value Type.t
end

module type NODE_STORE = sig
  include CONTENT_ADDRESSABLE_STORE
  module Path: PATH
  val merge: [`Read | `Write] t -> key option Merge.t
  module Key: sig
    include HASH with type t = key
    val digest: value -> t
  end
  module Metadata: METADATA
  module Val: NODE
    with type t = value
     and type hash = key
     and type metadata = Metadata.t
     and type step = Path.step
  module Contents: CONTENTS_STORE with type key = Val.hash
end

type config = Conf.t
type 'a diff = 'a Diff.t

module type COMMIT = sig
  type t
  type hash
  val v: info:Info.t -> node:hash -> parents:hash list -> t
  val node: t -> hash
  val parents: t -> hash list
  val info: t -> Info.t
  val t: t Type.t
  val hash_t: hash Type.t
end

module type COMMIT_STORE = sig
  include CONTENT_ADDRESSABLE_STORE
  val merge: [`Read| `Write] t -> info:Info.f -> key option Merge.t
  module Key: sig
    include HASH with type t = key
    val digest: value -> t
  end
  module Val: COMMIT
    with type t = value
     and type hash = key
  module Node: NODE_STORE with type key = Val.hash
end

module type COMMIT_HISTORY = sig
  type 'a t
  type node
  type commit
  type v
  val v: [> `Write] t -> node:node -> parents:commit list -> info:Info.t ->
    (commit * v) Lwt.t
  val parents: [> `Read] t -> commit -> commit list Lwt.t
  val merge: [`Read | `Write] t -> info:Info.f -> commit Merge.t
  val lcas: [> `Read] t -> ?max_depth:int -> ?n:int -> commit -> commit ->
    (commit list, [`Max_depth_reached | `Too_many_lcas]) result Lwt.t
  val lca: [`Read | `Write] t -> info:Info.f -> ?max_depth:int -> ?n:int -> commit list ->
    (commit option, Merge.conflict) result Lwt.t
  val three_way_merge: [`Read | `Write] t -> info:Info.f -> ?max_depth:int -> ?n:int ->
    commit -> commit -> (commit, Merge.conflict) result Lwt.t
  val closure: [> `Read] t -> min:commit list -> max:commit list -> commit list Lwt.t
  val commit_t: commit Type.t
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
  include Type.S
  val master: t
  val is_valid: t -> bool
end

(** Read-write stores. *)
module type ATOMIC_WRITE_STORE = sig
  type t
  type key
  type value
  val mem: t -> key -> bool Lwt.t
  val find: t -> key -> value option Lwt.t
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

module type ATOMIC_WRITE_STORE_MAKER = functor (K: Type.S) (V: Type.S) ->
sig
  include ATOMIC_WRITE_STORE with type key = K.t and type value = V.t
  val v: Conf.t -> t Lwt.t
end

module type BRANCH_STORE = sig
  include ATOMIC_WRITE_STORE
  module Key: BRANCH with type t = key
  module Val: HASH with type t = value
end

type remote = ..

module type SYNC = sig
  type t
  type commit
  type branch
  type endpoint
  val fetch: t -> ?depth:int -> endpoint -> branch ->
    (commit option, [`No_head | `Not_available | `Msg of string]) result Lwt.t
  val push: t -> ?depth:int -> endpoint -> branch ->
    (unit, [`No_head | `Not_available | `Msg of string | `Detached_head])
      result Lwt.t
end

module type PRIVATE = sig
  module Hash: HASH
  module Contents: CONTENTS_STORE
    with type key = Hash.t
  module Node: NODE_STORE
    with type key = Hash.t and type Val.hash = Contents.key
  module Commit: COMMIT_STORE
    with type key = Hash.t and type Val.hash = Node.key
  module Branch: BRANCH_STORE
    with type value = Commit.key
  module Slice: SLICE
     with type contents = Contents.key * Contents.value
     and type node = Node.key * Node.value
     and type commit = Commit.key * Commit.value
  module Repo: sig
    type t
    val v: Conf.t -> t Lwt.t
    val contents_t: t -> [`Read] Contents.t
    val node_t: t -> [`Read] Node.t
    val commit_t: t -> [`Read] Commit.t
    val branch_t: t -> Branch.t
    val batch: t ->
      ([`Read | `Write] Contents.t ->
       [`Read | `Write] Node.t ->
       [`Read | `Write] Commit.t
       -> 'a Lwt.t) -> 'a Lwt.t
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

  val clear_caches: tree -> unit

  type marks
  val empty_marks: unit -> marks
  type 'a force = [`True | `False of (key -> 'a -> 'a Lwt.t)]
  type uniq = [`False | `True | `Marks of marks]
  type 'a node_fn = key -> step list -> 'a -> 'a Lwt.t

  val fold:
    ?force:'a force -> ?uniq: uniq -> ?pre:'a node_fn -> ?post:'a node_fn ->
    (key -> contents -> 'a -> 'a Lwt.t) -> tree -> 'a -> 'a Lwt.t

  type stats = { nodes: int; leafs: int; skips: int; depth: int; width: int }
  val pp_stats: stats Fmt.t
  val stats: ?force:bool -> tree -> stats Lwt.t

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
  type hash
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
  module Hash: HASH with type t = hash
  module Commit: sig
    type t = commit
    val t: Repo.t -> t Type.t
    val pp_hash: t Fmt.t
    val v: Repo.t -> info:Info.t -> parents:commit list -> tree -> commit Lwt.t
    val tree: commit -> tree Lwt.t
    val parents: commit -> commit list Lwt.t
    val info: commit -> Info.t
    val hash: commit -> hash
    val of_hash: Repo.t -> hash -> commit option Lwt.t
  end
  module Contents: sig
    include CONTENTS with type t = contents
    val hash: contents -> hash
    val of_hash: Repo.t -> hash -> contents option Lwt.t
  end
  module Tree: sig
    include TREE with type step := step
                  and type key := key
                  and type metadata := metadata
                  and type contents := contents
                  and type node := node
                  and type tree := tree
    val hash: tree -> hash
    val of_hash: Repo.t -> hash -> [`Node of node] option Lwt.t
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
  val hash: t -> key -> hash option Lwt.t

  type write_error = [
    | Merge.conflict
    | `Too_many_retries of int
    | `Test_was of tree option
  ]

  val set:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> contents -> (unit, write_error) result Lwt.t

  val set_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> contents -> unit Lwt.t

  val set_tree:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> tree -> (unit, write_error) result Lwt.t

  val set_tree_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> tree -> unit Lwt.t

  val remove:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> (unit, write_error) result Lwt.t

  val remove_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> unit Lwt.t

  val test_and_set:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> test:contents option -> set:contents option ->
    (unit, write_error) result Lwt.t

  val test_and_set_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> test:contents option -> set:contents option -> unit Lwt.t

  val test_and_set_tree:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> test:tree option -> set:tree option ->
    (unit, write_error) result Lwt.t

  val test_and_set_tree_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    t -> key -> test:tree option -> set:tree option -> unit Lwt.t

  val merge:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option ->
    t -> key -> contents option -> (unit, write_error) result Lwt.t

  val merge_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:contents option -> t -> key -> contents option -> unit Lwt.t

  val merge_tree:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option -> t -> key -> tree option -> (unit, write_error) result Lwt.t

  val merge_tree_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    info:Info.f ->
    old:tree option -> t -> key -> tree option -> unit Lwt.t

  val with_tree:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[`Set | `Test_and_set | `Merge] ->
    info:Info.f ->
    t -> key -> (tree option -> tree option Lwt.t) ->
    (unit, write_error) result Lwt.t

  val with_tree_exn:
    ?retries:int ->
    ?allow_empty:bool ->
    ?parents:commit list ->
    ?strategy:[`Set | `Test_and_set | `Merge] ->
    info:Info.f ->
    t -> key -> (tree option -> tree option Lwt.t) ->
    unit Lwt.t

  val clone: src:t -> dst:branch -> t Lwt.t
  type watch
  val watch:
    t -> ?init:commit -> (commit diff -> unit Lwt.t) -> watch Lwt.t
  val watch_key: t -> key -> ?init:commit ->
    ((commit * tree) diff -> unit Lwt.t) -> watch Lwt.t
  val unwatch: watch -> unit Lwt.t

  type 'a merge = info:Info.f -> ?max_depth:int -> ?n:int -> 'a ->
    (unit, Merge.conflict) result Lwt.t
  val merge_into: into:t -> t merge
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
  val write_error_t: write_error Type.t

  module Private: sig
    include PRIVATE
      with type Contents.value = contents
       and module Hash = Hash
       and module Node.Path = Key
       and type Node.Metadata.t = metadata
       and type Branch.key = branch
       and type Slice.t = slice
       and type Repo.t = repo
  end

  type remote += E of Private.Sync.endpoint

  val to_private_node: node -> Private.Node.value option Lwt.t
  val of_private_node: repo -> Private.Node.value -> node

  val to_private_commit: commit -> Private.Commit.value
  val of_private_commit: repo -> Private.Commit.value -> commit
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
   and type hash = H.t

type remote += Store: (module STORE with type t = 'a) * 'a -> remote

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
