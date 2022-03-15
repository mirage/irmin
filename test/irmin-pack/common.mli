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
module Dict : Irmin_pack_unix.Dict.S
module I = Irmin_pack_unix.Index
module Conf : Irmin_pack.Conf.S

module Schema :
  Irmin.Schema.Extended
    with type Hash.t = Irmin.Hash.SHA1.t
     and type Path.step = string
     and type Path.t = string list
     and type Branch.t = string
     and type Contents.t = string
     and type Metadata.t = unit

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

  val check_repr :
    ?pos:Source_code_position.pos ->
    'a Irmin.Type.t ->
    string ->
    'a ->
    'a ->
    unit
end

module Index : module type of Irmin_pack_unix.Index.Make (Schema.Hash)
module Key : Irmin_pack.Pack_key.S with type hash = Schema.Hash.t

module Pack :
  Irmin_pack.Pack_store_intf.S
    with type hash = Schema.Hash.t
     and type key = Key.t
     and type value = string
     and type index := Index.t

module P :
  Irmin_pack.Pack_store_intf.Maker
    with type hash = Schema.Hash.t
     and type index := Irmin_pack_unix.Index.Make(Schema.Hash).t

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
val sha1 : string -> Schema.Hash.t
val sha1_contents : string -> Schema.Hash.t
val rm_dir : string -> unit
val index_log_size : int option
val random_string : int -> string
val random_letters : int -> string

val exec_cmd : string -> (unit, int) result
(** Exec a command, and return [Ok ()] or [Error n] if return code is n <> 0 *)
