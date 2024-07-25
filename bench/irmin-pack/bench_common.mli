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

module Mtime : module type of Import.Mtime

val default_artefacts_dir : Eio.Fs.dir_ty Eio.Path.t -> Eio.Fs.dir_ty Eio.Path.t
val prepare_artefacts_dir : Eio.Fs.dir_ty Eio.Path.t -> unit
val reporter : ?prefix:string -> unit -> Logs.reporter
val setup_log : Fmt.style_renderer option -> Logs.level option -> unit
val reset_stats : unit -> unit
val with_timer : (unit -> 'a) -> float * 'a

val with_progress_bar :
  message:string -> n:int -> unit:string -> ((int -> unit) -> 'a) -> 'a

val random_blob : unit -> bytes

module Info (I : Irmin.Info.S) : sig
  val f : I.f
end

module Conf : Irmin_pack.Conf.S
module Schema : Irmin.Schema.S

module FSHelper : sig
  val rm_dir : Eio.Fs.dir_ty Eio.Path.t -> unit
  val get_size : Eio.Fs.dir_ty Eio.Path.t -> int
end

module Generate_trees
    (Store : Irmin.Generic_key.KV with type Schema.Contents.t = bytes) : sig
  val add_chain_trees : int -> int -> Store.tree -> Store.tree
  (** [add_chain_trees depth nb tree] adds [nb] random contents to [tree],
      depthwise. *)

  val add_large_trees : int -> int -> Store.tree -> Store.tree
  (** [add_large_trees width nb tree] adds [nb] random contents to [tree],
      breadthwise. *)
end
