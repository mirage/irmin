(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type t = ..

module type S = sig
  (** {1 Remote synchronization} *)

  type t
  (** The type for store handles. *)

  type commit
  (** The type for store heads. *)

  type branch
  (** The type for branch IDs. *)

  type endpoint
  (** The type for sync endpoints. *)

  val fetch :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (commit option, [ `Msg of string ]) result Lwt.t
  (** [fetch t uri] fetches the contents of the remote store located at [uri]
      into the local store [t]. Return the head of the remote branch with the
      same name, which is now in the local store. [No_head] means no such branch
      exists. *)

  val push :
    t ->
    ?depth:int ->
    endpoint ->
    branch ->
    (unit, [ `Msg of string | `Detached_head ]) result Lwt.t
  (** [push t uri] pushes the contents of the local store [t] into the remote
      store located at [uri]. *)
end

module type Sigs = sig
  module type S = S

  type nonrec t = t = ..

  (** Provides stub implementations of the {!S} that always returns [Error] when
      push/pull operations are attempted. *)
  module None (H : Type.S) (R : Type.S) : sig
    include
      S with type commit = H.t and type branch = R.t and type endpoint = unit

    val v : 'a -> t Lwt.t
    (** Create a remote store handle. *)
  end
end
