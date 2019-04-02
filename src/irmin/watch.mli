(*
 * Copyright (c) 2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Watches *)

module type S = sig
  type key

  type value

  type watch

  type t

  val stats : t -> int * int

  val notify : t -> key -> value option -> unit Lwt.t

  val v : unit -> t

  val clear : t -> unit Lwt.t

  val watch_key :
    t -> key -> ?init:value -> (value Diff.t -> unit Lwt.t) -> watch Lwt.t

  val watch :
    t ->
    ?init:(key * value) list ->
    (key -> value Diff.t -> unit Lwt.t) ->
    watch Lwt.t

  val unwatch : t -> watch -> unit Lwt.t

  val listen_dir :
    t ->
    string ->
    key:(string -> key option) ->
    value:(key -> value option Lwt.t) ->
    (unit -> unit Lwt.t) Lwt.t
end

module Make (K : sig
  type t

  val t : t Type.t
end) (V : sig
  type t

  val t : t Type.t
end) : S with type key = K.t and type value = V.t

type hook =
  int -> string -> (string -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t

val set_listen_dir_hook : hook -> unit

val none : hook

val workers : unit -> int
