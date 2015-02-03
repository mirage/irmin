(*
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  type t
  val notify: t -> key -> value option -> unit
  val create: unit -> t
  val clear: t -> unit
  val watch: t -> key -> value option -> value option Lwt_stream.t
  val watch_all: t -> (key * value option) Lwt_stream.t
  val listen_dir: t -> string
    -> key:(string -> key option)
    -> value:(key -> value option Lwt.t)
    -> unit
  (** Register a fsevents/inotify thread to look for changes in the
      given directory. *)

end

module Make(K: Tc.S0) (V: Tc.S0): S with type key = K.t and type value = V.t

val set_listen_dir_hook: (int -> string -> (string -> unit Lwt.t) -> unit) -> unit

val lwt_stream_lift: 'a Lwt_stream.t Lwt.t -> 'a Lwt_stream.t
