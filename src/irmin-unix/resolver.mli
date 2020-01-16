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

(** Irmin store resolver. *)

val global_option_section : string

val branch : string option Cmdliner.Term.t

(** {1 Hash} *)
module Hash : sig
  type t = (module Irmin.Hash.S)

  val add : string -> ?default:bool -> (module Irmin.Hash.S) -> unit

  val find : string -> t

  val term : t option Cmdliner.Term.t
end

type hash = Hash.t

(** {1 Contents} *)
module Contents : sig
  type t = (module Irmin.Contents.S)

  val add : string -> ?default:bool -> (module Irmin.Contents.S) -> unit

  val find : string -> t

  val term : string option Cmdliner.Term.t
end

type contents = Contents.t

(** {1 Global Configuration} *)

module Store : sig
  type t
  (** The type for store configurations. A configuration value contains: the
      store implementation a creator of store's state and endpoint. *)

  (** The type of constructors of a store configuration. Depending on the
      backend, a store may require a hash function. *)
  type store_functor =
    | Fixed_hash of (contents -> t)
    | Variable_hash of (hash -> contents -> t)

  type remote_fn = ?headers:Cohttp.Header.t -> string -> Irmin.remote

  val v : ?remote:remote_fn -> (module Irmin.S) -> t

  val mem : hash -> contents -> t

  val irf : hash -> contents -> t

  val http : t -> t

  val git : contents -> t

  val find : string -> store_functor

  val add : string -> ?default:bool -> store_functor -> unit
end

type Irmin.remote += R of Cohttp.Header.t option * string

val remote : Irmin.remote Lwt.t Cmdliner.Term.t
(** Parse a remote store location. *)

(** {1 Stores} *)

type store =
  | S :
      (module Irmin.S with type t = 'a) * 'a Lwt.t * Store.remote_fn option
      -> store

val store : store Cmdliner.Term.t
(** Parse the command-line arguments and then the config file. *)
