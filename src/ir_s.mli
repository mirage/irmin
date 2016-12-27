(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type STEP = Ir_hum.S

module type PATH = sig
  include Ir_hum.S
  type step
  val empty: t
  val create: step list -> t
  val is_empty: t -> bool
  val cons: step -> t -> t
  val rcons: t -> step -> t
  val decons: t -> (step * t) option
  val rdecons: t -> (t * step) option
  val map: t -> (step -> 'a) -> 'a list
  module Step: STEP with type t = step
end

module type HASH = sig
  include Ir_hum.S
  val digest: Cstruct.t -> t
  val has_kind: [> `SHA1] -> bool
  val to_raw: t -> Cstruct.t
  val of_raw: Cstruct.t -> t
  val digest_size: int
end

module type RO_STORE = sig
  type t
  type key
  type value
  val read: t -> key -> value option Lwt.t
  val read_exn: t -> key -> value Lwt.t
  val mem: t -> key -> bool Lwt.t
  val iter: t -> (key -> (unit -> value Lwt.t) -> unit Lwt.t) -> unit Lwt.t
end

module type RO_MAKER =
  functor (K: Ir_hum.S) ->
  functor (V: Tc.S0) ->
    RO_STORE with type key = K.t
              and type value = V.t

module type AO_STORE = sig
  include RO_STORE
  val add: t -> value -> key Lwt.t
end

module type AO_MAKER =
  functor (K: HASH) ->
  functor (V: Tc.S0) -> sig
    include AO_STORE with type key = K.t and type value = V.t
    val create: Ir_conf.t -> t Lwt.t
  end

module type METADATA = sig
  include Ir_hum.S
  val merge: t Ir_merge.t
  val default: t
end

module type CONTENTS = sig
  include Tc.S0
  module Path: PATH
  val merge: Path.t -> t option Ir_merge.t
end

module type CONTENTS_STORE = sig
  include AO_STORE
  module Path: PATH
  val merge: Path.t -> t -> key option Ir_merge.t
  module Key: HASH with type t = key
  module Val: CONTENTS with type t = value and module Path = Path
end

module type NODE = sig
  module Metadata: METADATA

  include Tc.S0
  type raw_contents
  type contents = raw_contents * Metadata.t
  type node
  type step

  val create: (step * [`Contents of contents | `Node of node]) list -> t
  val alist: t -> (step * [`Contents of contents | `Node of node]) list

  val empty: t
  val is_empty: t -> bool

  val contents: t -> step -> contents option
  val iter_contents: t -> (step -> contents -> unit) -> unit
  val with_contents: t -> step -> contents option -> t

  val succ: t -> step -> node option
  val iter_succ: t -> (step -> node -> unit) -> unit
  val with_succ: t -> step -> node option -> t
end

module type NODE_STORE = sig
  include AO_STORE
  module Path: PATH
  val merge: Path.t -> t -> key option Ir_merge.t
  module Key: HASH with type t = key
  module Val: NODE
    with type t = value
     and type node = key
     and type step = Path.step
  module Contents: CONTENTS_STORE with type key = Val.raw_contents
end

module type COMMIT = sig
  include Tc.S0
  type commit
  type node
  val create: Ir_task.t -> node:node -> parents:commit list -> t
  val node: t -> node
  val parents: t -> commit list
  val task: t -> Ir_task.t
end

module type COMMIT_STORE = sig
  include AO_STORE
  val merge: Ir_task.t -> t -> key option Ir_merge.t
  module Key: HASH with type t = key
  module Val: COMMIT
    with type t = value
     and type commit = key
  module Node: NODE_STORE with type key = Val.node
end

module type LINK_STORE = sig
  include AO_STORE
  val add: t -> key -> value -> unit Lwt.t
end

module type LINK_MAKER =
  functor (K: HASH) -> sig
    include LINK_STORE with type key = K.t and type value = K.t
    val create: Ir_conf.t -> t Lwt.t
  end

module type SLICE = sig
  include Tc.S0
  type contents
  type node
  type commit
  val create: unit -> t Lwt.t
  val add_contents: t -> contents -> unit Lwt.t
  val add_node: t -> node -> unit Lwt.t
  val add_commit: t -> commit -> unit Lwt.t
  val iter_contents: t -> (contents -> unit Lwt.t) -> unit Lwt.t
  val iter_nodes: t -> (node -> unit Lwt.t) -> unit Lwt.t
  val iter_commits: t -> (commit -> unit Lwt.t) -> unit Lwt.t
end

module type REF = sig
  include Ir_hum.S
  val master: t
  val is_valid: t -> bool
end

module type RW = sig
  include RO_STORE
  val update: t -> key -> value -> unit Lwt.t
  val compare_and_set: t -> key -> test:value option -> set:value option -> bool Lwt.t
  val remove: t -> key -> unit Lwt.t
end

module type REACTIVE = sig
  include RW
  type watch
  val watch_key: t -> key -> ?init:value -> (value Ir_watch.diff -> unit Lwt.t) ->
    watch Lwt.t
  val watch: t -> ?init:(key * value) list ->
    (key -> value Ir_watch.diff -> unit Lwt.t) -> watch Lwt.t
  val unwatch: t -> watch -> unit Lwt.t
end

module type RW_MAKER =
  functor (K: Ir_hum.S) ->
  functor (V: Tc.S0) -> sig
    include REACTIVE with type key = K.t and type value = V.t
    val create: Ir_conf.t -> t Lwt.t
  end

module type REF_STORE = sig
  include REACTIVE
  module Key: REF with type t = key
  module Val: HASH with type t = value
end

module type HIERARCHICAL = sig
  include RW
  val list      : t -> key -> key list Lwt.t
  val remove_rec: t -> key -> unit Lwt.t
end

module type SYNC = sig
  type t
  type commit_id
  type branch_id
  val fetch: t -> ?depth:int -> uri:string -> branch_id ->
    [`Head of commit_id | `No_head | `Error] Lwt.t
  val push : t -> ?depth:int -> uri:string -> branch_id -> [`Ok | `Error] Lwt.t
end

module type STORE = sig
  type commit_id
  type branch_id
  type slice
  module Repo: sig
    type t
    val create: Ir_conf.t -> t Lwt.t
    val branches: t -> branch_id list Lwt.t
    val remove_branch: t -> branch_id -> unit Lwt.t
    val heads: t -> commit_id list Lwt.t
    val watch_branches: t -> ?init:(branch_id * commit_id) list ->
      (branch_id -> commit_id Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
    val export: ?full:bool -> ?depth:int -> ?min:commit_id list -> ?max:commit_id list ->
      t -> slice Lwt.t
    val import: t -> slice -> [`Ok | `Error] Lwt.t
    val task_of_commit_id: t -> commit_id -> Ir_task.t Lwt.t
  end
  include HIERARCHICAL
  val master: ('a -> Ir_task.t) -> Repo.t -> ('a -> t) Lwt.t
  val repo: t -> Repo.t
  val task: t -> Ir_task.t
  val of_branch_id: 'a Ir_task.f -> branch_id -> Repo.t -> ('a -> t) Lwt.t
  val name: t -> branch_id option Lwt.t
  val name_exn: t -> branch_id Lwt.t
  val update_branch: t -> branch_id -> unit Lwt.t
  val merge_branch: t -> ?max_depth:int -> ?n:int -> branch_id -> unit Ir_merge.result Lwt.t
  val merge_branch_exn: t -> ?max_depth:int -> ?n:int -> branch_id -> unit Lwt.t
  val empty: 'a Ir_task.f -> Repo.t -> ('a -> t) Lwt.t
  val of_commit_id: ('a -> Ir_task.t) -> commit_id -> Repo.t -> ('a -> t) Lwt.t
  val head: t -> commit_id option Lwt.t
  val head_exn: t -> commit_id Lwt.t
  val head_ref: t -> [`Branch of branch_id | `Head of commit_id | `Empty]
  val update_head: t -> commit_id -> unit Lwt.t
  val fast_forward_head: t -> ?max_depth:int -> ?n:int -> commit_id -> bool Lwt.t
  val compare_and_set_head: t -> test:commit_id option -> set:commit_id option -> bool Lwt.t
  val merge_head: t -> ?max_depth:int -> ?n:int -> commit_id -> unit Ir_merge.result Lwt.t
  val merge_head_exn: t -> ?max_depth:int -> ?n:int -> commit_id -> unit Lwt.t
  val watch_head: t -> ?init:commit_id -> (commit_id Ir_watch.diff -> unit Lwt.t) ->
    (unit -> unit Lwt.t) Lwt.t
  val watch_key: t -> key -> ?init:(commit_id * value) ->
    ((commit_id * value) Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
  val clone: 'a Ir_task.f -> t -> branch_id -> [`Ok of ('a -> t) | `Duplicated_branch | `Empty_head] Lwt.t
  val clone_force: 'a Ir_task.f -> t -> branch_id -> ('a -> t) Lwt.t
  val merge: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Ir_merge.result Lwt.t
  val merge_exn: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> into:('a -> t) ->
    unit Lwt.t
  val lcas: 'a -> ?max_depth:int -> ?n:int -> ('a -> t) -> ('a -> t) ->
    [`Ok of commit_id list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lcas_branch: t -> ?max_depth:int -> ?n:int -> branch_id ->
    [`Ok of commit_id list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  val lcas_head: t -> ?max_depth:int -> ?n:int -> commit_id ->
    [`Ok of commit_id list | `Max_depth_reached | `Too_many_lcas ] Lwt.t
  module History: Graph.Sig.P with type V.t = commit_id
  val history: ?depth:int -> ?min:commit_id list -> ?max:commit_id list -> t -> History.t Lwt.t
end

module type PRIVATE = sig
  module Contents: CONTENTS_STORE
  module Node: NODE_STORE
    with type Val.raw_contents = Contents.key and module Path = Contents.Path
  module Commit: COMMIT_STORE
    with type Val.node = Node.key
  module Ref: REF_STORE
    with type value = Commit.key
  module Slice: SLICE
    with type contents = Contents.key * Contents.value
     and type node = Node.key * Node.value
     and type commit = Commit.key * Commit.value
  module Repo: sig
    type t
    val create: Ir_conf.t -> t Lwt.t
    val contents_t: t -> Contents.t
    val node_t: t -> Node.t
    val commit_t: t -> Commit.t
    val ref_t: t -> Ref.t
  end
  module Sync: sig
    include SYNC
      with type commit_id = Commit.key and type branch_id = Ref.key
    val create: Repo.t -> t Lwt.t
  end
end

module type STORE_EXT = sig
  include STORE

  module Key: PATH with type t = key
  module Val: CONTENTS with type t = value
  module Ref: REF with type t = branch_id
  module Hash: HASH with type t = commit_id

  module Private: sig
    include PRIVATE
      with type Contents.value = value
       and module Contents.Path = Key
       and type Commit.key = commit_id
       and type Ref.key = branch_id
       and type Slice.t = slice
       and type Repo.t = Repo.t
    val read_node: t -> key -> Node.key option Lwt.t
    val mem_node: t -> key -> bool Lwt.t
    val update_node: t -> key -> Node.key -> unit Lwt.t
    val merge_node: t -> key -> (commit_id * Node.key) -> unit Ir_merge.result Lwt.t
    val remove_node: t -> key -> unit Lwt.t
    val iter_node: t -> Node.key ->
      (key -> (unit -> value Lwt.t) -> unit Lwt.t) -> unit Lwt.t
  end

end

module type MAKER =
  functor (C: CONTENTS) ->
  functor (R: REF) ->
  functor (H: HASH) ->
    STORE_EXT
      with type key = C.Path.t
       and module Key = C.Path
       and type value = C.t
       and type branch_id = R.t
       and type commit_id = H.t
