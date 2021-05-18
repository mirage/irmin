(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

open Irmin.Perms
module Int63 = Optint.Int63
module Dict : Irmin_pack.Dict.S
module H = Irmin.Hash.SHA1
module I = Index

module Filename : sig
  include module type of Filename

  val quote_command :
    string ->
    ?stdin:string ->
    ?stdout:string ->
    ?stderr:string ->
    string list ->
    string
end

module Alcotest : sig
  include module type of Alcotest

  val int63 : Int63.t testable
  val check_raises_lwt : string -> exn -> (unit -> _ Lwt.t) -> unit Lwt.t
  val check_repr : 'a Irmin.Type.t -> string -> 'a -> 'a -> unit
end

module Index : Irmin_pack.Index.S with type key = H.t

module Pack :
  Irmin_pack.Pack_store.S
    with type key = H.t
     and type value = string
     and type index = Index.t

module P :
  Irmin_pack.Pack_store.Maker
    with type key = H.t
     and type index = Irmin_pack.Index.Make(H).t

(** Helper constructors for fresh pre-initialised dictionaries and packs *)
module Make_context (Config : sig
  val root : string
end) : sig
  val fresh_name : string -> string
  (** [fresh_name typ] is a clean directory for a resource of type [typ]. *)

  type d = { dict : Dict.t; clone : readonly:bool -> Dict.t }

  val get_dict : unit -> d
  (** Fresh, empty dict. *)

  type t = {
    index : Index.t;
    pack : read Pack.t;
    clone_pack : readonly:bool -> read Pack.t Lwt.t;
    clone_index_pack : readonly:bool -> (Index.t * read Pack.t) Lwt.t;
  }

  val get_pack : ?lru_size:int -> unit -> t Lwt.t
  (** Fresh, empty index and pack. [clone_pack] opens a clone of the pack at the
      same location, [clone_index_pack] opens a clone of the index and the pack. *)

  val close : Index.t -> read Pack.t -> unit Lwt.t
end

val get : 'a option -> 'a
val sha1 : string -> H.t
val rm_dir : string -> unit
val index_log_size : int option

module Conf : sig
  val entries : int
  val stable_hash : int
end

val random_string : int -> string
val random_letters : int -> string
