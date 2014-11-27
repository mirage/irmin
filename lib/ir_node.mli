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

module type S = sig
  include Ir_contents.S
  type contents
  type node
  type step
  type 'a step_map
  val contents: t -> step -> contents
  val with_contents: t -> contents step_map -> t
  val succ: t -> node step_map
  val with_succ: t -> node step_map -> t
  val edges: t -> [> `Contents of contents | `Node of node] list
  val empty: t
  val create: contents step_map -> node step_map -> t
  val is_empty: t -> bool
end

module Make (C: Tc.S0) (N: Tc.S0) (S: Tc.S0):
  S with type contents = C.t
     and type node := N.t
     and type 'a step_map := 'a Map.Make(S).t

module type STORE = sig
  include Ir_ao.STORE
  module Step: Tc.S0
  module StepMap: Map.S with type key = Step.t
  module Key: Ir_hash.S with type t = key
  module Val: S
    with type t = value
     and type node := key
     and type 'a step_map := 'a StepMap.t
end

module type STORE_EXT = sig

  (** The node store encodes a labeled DAG where every node might hold
      some contents. *)

  type step
  (** A step is used to pass from one node to an other. A list of
      steps forms a path. *)

  module Contents: Ir_contents.STORE_EXT
  (** The contents store. *)

  include STORE
    with type Step.t = step
     and type Val.contents = Contents.key

  type contents = Contents.value
  (** Node contents. *)

  val empty: value
  (** The empty node. *)

  val node: t ->
    ?contents:(step * contents) list ->
    ?succ:(step * value) list ->
    unit -> (key * value) Lwt.t
  (** Create a new node. *)

  val contents: t -> value -> step -> contents Lwt.t
  (** Return the node contents. *)

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

module Store
    (C: Ir_contents.STORE)
    (S: STORE with type Val.contents = C.key)
  : STORE_EXT with type t = C.t * S.t
               and type key = S.key
               and type value = S.value
               and type step = S.Step.t
               and module Step = S.Step
               and module Contents = Ir_contents.Store(C)
(** Create a node store from an append-only database. *)
