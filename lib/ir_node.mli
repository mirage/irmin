(*
 * Copyright (c) 2013      Louis Gesbert     <louis.gesbert@ocamlpro.com>
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

(** Nodes represent structured values serialized in the block
    store. *)

module No_metadata: Ir_s.METADATA with type t = unit

module Make (C: Tc.S0) (N: Tc.S0) (P: Ir_s.PATH) (M: Ir_s.METADATA):
  Ir_s.NODE with type raw_contents = C.t
             and type node = N.t
             and type step = P.step
             and module Metadata = M

module Store
    (C: Ir_s.CONTENTS_STORE)
    (S: sig
       include Ir_s.AO_STORE
       module Key: Ir_s.HASH with type t = key
       module Val: Ir_s.NODE with type t = value
                              and type node = key
                              and type raw_contents = C.key
                              and type step = C.Path.step
     end):
  Ir_s.NODE_STORE
    with  type t = C.t * S.t
      and type key = S.key
      and type value = S.value
      and module Path = C.Path
      and module Key = S.Key
      and module Val = S.Val

module type GRAPH = sig
  type t
  type contents
  type node
  type step
  type path

  val empty: t -> node Lwt.t
  val create: t -> (step * [`Contents of contents | `Node of node]) list -> node Lwt.t

  val contents: t -> node -> step -> contents option Lwt.t
  val succ: t -> node -> step -> node option Lwt.t
  val steps: t -> node -> step list Lwt.t

  val iter_contents: t -> node -> (step -> contents -> unit) -> unit Lwt.t
  val iter_succ: t -> node -> (step -> node -> unit) -> unit Lwt.t

  val mem_contents: t -> node -> path -> bool Lwt.t
  val read_contents: t -> node -> path -> contents option Lwt.t
  val read_contents_exn: t -> node -> path -> contents Lwt.t
  val add_contents: t -> node -> path -> contents -> node Lwt.t
  val remove_contents: t -> node -> path -> node Lwt.t

  val mem_node: t -> node -> path -> bool Lwt.t
  val read_node: t -> node -> path -> node option Lwt.t
  val read_node_exn: t -> node -> path -> node Lwt.t
  val add_node: t -> node -> path -> node -> node Lwt.t
  val remove_node: t -> node -> path -> node Lwt.t

  val closure: t -> min:node list -> max:node list -> node list Lwt.t
end

module Graph (S: Ir_s.NODE_STORE):
  GRAPH with type t = S.t
         and type contents = S.Contents.key * S.Val.Metadata.t
         and type node = S.key
         and type step = S.Path.step
         and type path = S.Path.t
