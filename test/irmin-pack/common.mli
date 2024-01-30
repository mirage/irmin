(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module File_manager :
  Irmin_pack_unix.File_manager.S with module Io = Irmin_pack_unix.Io.Unix

module Errs :
  Irmin_pack_unix.Io_errors.S with module Io = Irmin_pack_unix.Io.Unix

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
  val kind : Irmin_pack.Pack_value.Kind.t testable
  val hash : Schema.Hash.t testable

  val check_raises_pack_error :
    string -> (Irmin_pack_unix.Errors.base_error -> bool) -> (unit -> _) -> unit

  val check_raises : string -> exn -> (unit -> _) -> unit

  val check_repr :
    ?pos:Source_code_position.pos ->
    'a Irmin.Type.t ->
    string ->
    'a ->
    'a ->
    unit

  val testable_repr : 'a Irmin.Type.t -> 'a Alcotest.testable

  val quick_tc : string -> (unit -> unit) -> unit test_case
  (** Convenience to create a `Quick test_case *)
end

module Index : module type of Irmin_pack_unix.Index.Make (Schema.Hash)
module Key : Irmin_pack_unix.Pack_key.S with type hash = Schema.Hash.t

module Pack :
  Irmin_pack_unix.Pack_store.S
    with type hash = Schema.Hash.t
     and type key = Key.t
     and type value = string

(** Helper constructors for fresh pre-initialised dictionaries and packs *)
module Make_context (Config : sig
  val root : string
end) : sig
  val fresh_name : string -> string
  (** [fresh_name typ] is a clean directory for a resource of type [typ]. *)

  type d = { name : string; fm : File_manager.t; dict : Dict.t }

  val get_dict :
    sw:Eio.Switch.t -> ?name:string -> readonly:bool -> fresh:bool -> unit -> d

  val close_dict : d -> unit

  type t = {
    name : string;
    fm : File_manager.t;
    index : Index.t;
    pack : read Pack.t;
    dict : Dict.t;
  }

  val get_rw_pack : sw:Eio.Switch.t -> t
  val get_ro_pack : sw:Eio.Switch.t -> string -> t
  val reopen_rw : sw:Eio.Switch.t -> string -> t
  val close_pack : t -> unit
end

val get : 'a option -> 'a
val sha1 : string -> Schema.Hash.t
val sha1_contents : string -> Schema.Hash.t
val rm_dir : string -> unit
val index_log_size : int option
val random_string : int -> string
val random_letters : int -> string
val unlink_path : string -> unit
val create_lower_root : ?mkdir:bool -> unit -> string

val exec_cmd : string -> (unit, int) result
(** Exec a command, and return [Ok ()] or [Error n] if return code is n <> 0 *)

val setup_test_env : root_archive:string -> root_local_build:string -> unit
(** [setup_test_env ~root_archive ~root_local_build] copies an existing store to
    a temporary location, to be used by the test. *)
