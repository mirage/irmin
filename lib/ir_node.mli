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
  include Tc.S0
  type contents
  type node
  type step
  val contents: t -> step -> contents option
  val all_contents: t -> (step * contents) list
  val with_contents: t -> step -> contents option -> t
  val succ: t -> step -> node option
  val all_succ: t -> (step * node) list
  val with_succ: t -> step -> node option -> t
  val steps: t -> step list
  val edges: t -> [> `Contents of contents | `Node of node] list
  val empty: t
  val create: contents:(step * contents) list -> succ:(step * node) list -> t
  val is_empty: t -> bool
end
  module Make (C: Tc.S0) (N: Tc.S0) (P: Ir_path.S):
    S with type contents = C.t
       and type node = N.t
       and type step = P.step

module type STORE = sig
  include Ir_ao.STORE
  module Path: Ir_path.S
  module Key: Ir_hash.S with type t = key
  module Val: S
    with type t = value
     and type node = key
     and type step = Path.step
end

module type STORE_EXT = sig
  type step
  module Contents: Ir_contents.STORE_EXT
  include STORE
    with type Path.step = step
     and type Val.contents = Contents.key
  type contents = Contents.value
  val empty: value
  val node: t ->
    ?contents:(step * contents) list ->
    ?succ:(step * value) list ->
    unit -> (key * value) Lwt.t
  val contents: t -> value -> step -> contents Lwt.t option
  val succ: t -> value -> step -> value Lwt.t option
  val all_succ: t -> value -> (step * value Lwt.t) list
  val sub: t -> value -> step list -> value option Lwt.t
  val sub_exn: t -> value -> step list -> value Lwt.t
  val map: t -> value -> step list -> (value -> value) -> value Lwt.t
  val update: t -> value -> step list -> contents -> value Lwt.t
  val find: t -> value -> step list -> contents option Lwt.t
  val find_exn: t -> value -> step list -> contents Lwt.t
  val remove: t -> value -> step list -> value Lwt.t
  val valid: t -> value -> step list -> bool Lwt.t
  val merge: t -> key Ir_merge.t
  val contents_t: t -> Contents.t
  val rec_list: t -> key list -> key list Lwt.t
end

module Make_ext
    (C: Ir_contents.STORE)
    (S: STORE with type Val.contents = C.key)
  : STORE_EXT with type t = C.t * S.t
               and type key = S.key
               and type value = S.value
               and type step = S.Path.step
               and module Path = S.Path
               and module Contents = Ir_contents.Make_ext(C)
               and type Contents.t = C.t
(** Create a node store from an append-only database. *)
