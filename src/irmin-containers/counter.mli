(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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

(** The implementation of an {!int64} counter. This module supports operations
    to increment, decrement and read the value of the counter.

    Merge semantics is as follows: if [old] is the value of the LCA and [v1] and
    [v2] are the current values, then the merged value is [v1 + v2 - old]. *)

(** Counter signature *)
module type S = sig
  module Store : Irmin.S
  (** Content store of counter. All store related operations like branching,
      cloning, merging, etc are done through this module. *)

  val inc :
    ?by:int64 -> ?info:Store.Info.f -> path:Store.key -> Store.t -> unit Lwt.t
  (** Increment the counter by the amount specified using [by]. If no value is
      specified, then [by] is assigned the value 1L. *)

  val dec :
    ?by:int64 -> ?info:Store.Info.f -> path:Store.key -> Store.t -> unit Lwt.t
  (** Decrement the counter by the amount specified using [by]. If no value is
      specified, then [by] is assigned the value 1L. *)

  val read : path:Store.key -> Store.t -> int64 Lwt.t
  (** Read the value of the counter *)
end

(** [Make] returns a mergeable counter using the backend and other parameters as
    specified by the user. *)
module Make (Backend : Irmin.KV_maker) :
  S
    with type Store.Schema.branch = string
     and type Store.Schema.path = string list
     and type Store.Schema.step = string

(** Counter instantiated using the {{!Irmin_unix.FS} FS backend} provided by
    [Irmin_unix] *)
module FS :
  S
    with type Store.Schema.branch = string
     and type Store.Schema.path = string list
     and type Store.Schema.step = string

(** Counter instantiated using the {{!Irmin_mem} in-memory backend} provided by
    [Irmin_mem] *)
module Mem :
  S
    with type Store.Schema.branch = string
     and type Store.Schema.path = string list
     and type Store.Schema.step = string
