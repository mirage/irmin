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

(** Irmin store resolver. *)

val global_option_section : string
val branch : string option Cmdliner.Term.t

(** {1 Hash} *)
module Hash : sig
  type t = (module Irmin.Hash.S)

  val add : string -> ?default:bool -> (module Irmin.Hash.S) -> unit
  val find : string -> t
  val term : unit -> t option Cmdliner.Term.t
end

type hash = Hash.t

(** {1 Contents} *)
module Contents : sig
  type t = (module Irmin.Contents.S)

  val add : string -> ?default:bool -> (module Irmin.Contents.S) -> unit
  val find : string -> t
  val term : unit -> string option Cmdliner.Term.t
end

type contents = Contents.t

(** {1 Global Configuration} *)

type eio := Import.eio

module Store : sig
  module Impl : sig
    (** The type of {i implementations} of an Irmin store.

        Stores can be either keyed by hashes or by some other abstract type. In
        the latter case, the store implementation cannot be used to build HTTP /
        GraphQL servers using [irmin-unix]. This limitation may be lifted in a
        future version of [irmin-unix]. *)
    type 'a t =
      | Hash_keyed : (module Irmin.S with type t = 'a) -> 'a t
      | Generic_keyed : (module Irmin.Generic_key.S with type t = 'a) -> 'a t

    val generic_keyed : 'a t -> (module Irmin.Generic_key.S with type t = 'a)
    val hash_keyed : 'a t -> (module Irmin.S with type t = 'a) option
  end

  type remote_fn =
    ?ctx:Mimic.ctx -> ?headers:Cohttp.Header.t -> string -> unit -> Irmin.remote

  type t
  (** The type for store configurations. A configuration value contains: the
      store implementation a creator of store's state and endpoint. *)

  type store_functor =
    | Fixed_hash of (contents -> t)
    | Variable_hash of (hash -> contents -> t)
    | Fixed of t
        (** The type of constructors of a store configuration. Depending on the
            backend, a store may require a hash function. *)

  val v :
    ?remote:remote_fn ->
    Irmin.Backend.Conf.Spec.t ->
    (module Irmin.S with type t = _) ->
    t

  val v_generic :
    ?remote:remote_fn ->
    Irmin.Backend.Conf.Spec.t ->
    (module Irmin.Generic_key.S with type t = _) ->
    t

  val mem : hash -> contents -> t
  val fs : eio -> hash -> contents -> t
  val git : contents -> t
  val pack : eio -> hash -> contents -> t
  val find : string -> eio -> store_functor
  val add : string -> ?default:bool -> store_functor -> unit
  val spec : t -> Irmin.Backend.Conf.Spec.t
  val generic_keyed : t -> (module Irmin.Generic_key.S)
  val hash_keyed : t -> (module Irmin.S) option
  val remote : t -> remote_fn option

  val term :
    unit -> (string option * hash option * string option) Cmdliner.Term.t
end

(** {1 Stores} *)

val load_config :
  env:eio ->
  ?plugin:string ->
  ?root:string ->
  ?config_path:string ->
  ?store:string ->
  ?hash:hash ->
  ?contents:string ->
  unit ->
  Store.t * Irmin.config
(** Load config file from disk

    [plugin] is the path to an OCaml plugin in cmxs format to be loaded at
    runtime

    [config_path] can be used to specify the location of a configuration file.

    [root] is used to specify the path of the store.

    The values provided for [store], [hash] and [contents] will be used by
    default if no other value is found in the config file *)

type store =
  | S : 'a Store.Impl.t * (unit -> 'a) * Store.remote_fn option -> store

val store : env:eio -> store Cmdliner.Term.t
(** Parse the command-line arguments and then the config file. *)

type Irmin.remote += R of Cohttp.Header.t option * string

val remote : env:eio -> (store * (unit -> Irmin.remote)) Cmdliner.Term.t
(** Parse a remote store location. *)
