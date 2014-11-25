(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Nodes represent structured values serialized in the block
    store. *)

(** The blocks form a labeled directed acyclic graph (DAG). For
    instance, using the following API it is possible to go from one
    node to another following a path in the graph. Every node of the
    graph might carry some optional contents. *)

module type S = sig

  (** Node values. *)

  include Ir_contents.S

  type contents
  (** Type for contents. *)

  type node
  (** Type for nodes. *)

  type 'a step_map
  (** A map of steps. *)

  val contents: t -> contents option
  (** [contents t] is the (optional) key of the node contents. *)

  val contents_exn: t -> contents
  (** Same as [contents], but raise [Not_found] if it is [None]. *)

  val with_contents: t -> contents option -> t
  (** Replace the optional contents. *)

  val succ: t -> node step_map
  (** Extract the successors of a node. *)

  val with_succ: t -> node step_map -> t
  (** Replace the list of successors. *)

  val edges: t -> [> `Contents of contents | `Node of node] list
  (** Return the list of successor vertices. *)

  val empty: t
  (** The empty node. *)

  val leaf: contents -> t
  (** Create a leaf node, with some contents and no successors. *)

  val create: ?contents:contents -> node step_map -> t
  (** [create ~contents succ] is the node with contents [contents] and
      successors [succs]. *)

  val is_empty: t -> bool
  (** Is the node empty. *)

  val is_leaf: t -> bool
  (** Is it a leaf node (see [leaf]) ? *)

end

module Node (C: Tc.I0) (N: Tc.I0) (S: Ir_misc.MAP):
  S with type contents = C.t and type node = N.t and type 'a step_map = 'a S.t

module type RAW_STORE = sig

  include Ir_ao.STORE

  module Step: Tc.I0
  (** Base functions over steps. *)

  module StepMap: Map.S with type key = Step.t
  (** Base functions over step maps. *)

  module Key: Ir_hash.S with type t = key
  (** Base functions for keys. *)

  module Val: S
    with type t = value
     and type node = key
     and type 'a step_map = 'a StepMap.t
  (** Base functions for values. *)

end

module type STORE = sig

  (** The node store encodes a labeled DAG where every node might hold
      some contents. *)

  type step
  (** A step is used to pass from one node to an other. A list of
      steps forms a path. *)

  module Contents: Ir_contents.STORE
  (** The contents store. *)

  include RAW_STORE
    with type Step.t = step
     and type Val.contents = Contents.key

  type contents = Contents.value
  (** Node contents. *)

  val empty: value
  (** The empty node. *)

  val node: t -> ?contents:contents -> ?succ:(step * value) list ->
    unit -> (key * value) Lwt.t
  (** Create a new node. *)

  val contents: t -> value -> contents Lwt.t option
  (** Return the node contents. *)

  val succ: t -> value -> value Lwt.t StepMap.t
  (** Return the node successors. *)

  val sub: t -> value -> step list -> value option Lwt.t
  (** Find a subvalue. *)

  val sub_exn: t -> value -> step list -> value Lwt.t
  (** Find a subvalue. Raise [Not_found] if it does not exist. *)

  val map: t -> value -> step list -> (value -> value) -> value Lwt.t
  (** Modify a subtree. *)

  val update: t -> value -> step list -> contents -> value Lwt.t
  (** Add a value by recusively saving subvalues into the
      corresponding stores. *)

  val find: t -> value -> step list -> contents option Lwt.t
  (** Find a value. *)

  val find_exn: t -> value -> step list -> contents Lwt.t
  (** Find a value. Raise [Not_found] is [path] is not defined. *)

  val remove: t -> value -> step list -> value Lwt.t
  (** Remove a value. *)

  val valid: t -> value -> step list -> bool Lwt.t
  (** Is a path valid. *)

  val merge: t -> key Ir_merge.t
  (** Merge two nodes together. *)

  val contents_t: t -> Contents.t
  (** An handler to the contents database. *)

end

module Make
    (C: Ir_contents.RAW_STORE)
    (S: RAW_STORE with type Val.contents = C.key)
  : STORE with type t = C.t * S.t
           and type key = S.key
           and type value = S.value
           and type step = S.Step.t
           and module Step = S.Step
           and module Contents = Ir_contents.Make(C)
(** Create a node store from an append-only database. *)
