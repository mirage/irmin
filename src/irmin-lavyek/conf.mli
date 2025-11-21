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

val spec : sw:Eio.Switch.t -> fs:_ Eio.Path.t -> Irmin.Backend.Conf.Spec.t

module Key : sig
  val fresh : bool Irmin.Backend.Conf.key
  val root : string Irmin.Backend.Conf.key
  val sync : bool Irmin.Backend.Conf.key
end

val fresh : Irmin.Backend.Conf.t -> bool
(** Flag to indicate that the store will start with fresh data on disk. Warning:
    setting this to [true] will delete existing data. Default is [false]. *)

val root : Irmin.Backend.Conf.t -> string
(** Location of directory for saving data on disk. *)

val sync : Irmin.Backend.Conf.t -> bool
(** Flag to indicate that sync should be used to enforce lavyek's flush to disk.
    Default [false]. *)

val switch : Irmin.Backend.Conf.t -> Eio.Switch.t
(** Eio switch *)

val fs : Irmin.Backend.Conf.t -> Eio.Fs.dir_ty Eio.Path.t
(** Eio filesystem *)

val init :
  sw:Eio.Switch.t ->
  fs:_ Eio.Path.t ->
  ?fresh:bool ->
  ?sync:bool ->
  Eio.Fs.dir_ty Eio.Path.t ->
  Irmin.config
(** [init root] creates a backend configuration for storing data with default
    configuration parameters and stored at [root]. *)
