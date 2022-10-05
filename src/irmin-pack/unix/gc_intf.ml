(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

open! Import

module type Args = sig
  module Fm : File_manager.S with module Io = Io.Unix
  module Async : Async.S
  module Dict : Dict.S with module Fm = Fm
  module Errs : Io_errors.S with module Io = Fm.Io
  module Dispatcher : Dispatcher.S with module Fm = Fm

  type hash
  type key = hash Pack_key.t [@@deriving irmin]

  module Hash : sig
    val hash_size : int
  end

  module Contents_store : sig
    type 'a t

    val purge_lru : 'a t -> unit
  end

  module Node_value : sig
    type t
    type step

    val pred :
      t ->
      (step option * [ `Contents of key | `Inode of key | `Node of key ]) list
  end

  module Node_store : sig
    type 'a t

    val v :
      config:Irmin.Backend.Conf.t ->
      fm:Fm.t ->
      dict:Dict.t ->
      dispatcher:Dispatcher.t ->
      read t

    val unsafe_find :
      check_integrity:bool -> [< read ] t -> key -> Node_value.t option

    val purge_lru : 'a t -> unit
  end

  module Commit_value : sig
    type t

    val node : t -> key
    val parents : t -> key list
  end

  module Commit_store :
    Pack_store.S
      with type value = Commit_value.t
       and type key = key
       and type file_manager = Fm.t
       and type dict = Dict.t
       and type dispatcher = Dispatcher.t
       and type hash = hash
end

module type S = sig
  type t
  (** A running GC process. *)

  module Args : Args

  val v :
    root:string ->
    generation:int ->
    unlink:bool ->
    offset:int63 ->
    dispatcher:Args.Dispatcher.t ->
    fm:Args.Fm.t ->
    contents:read Args.Contents_store.t ->
    node:read Args.Node_store.t ->
    commit:read Args.Commit_store.t ->
    Args.key ->
    t
  (** Creates and starts a new GC process. *)

  val finalise :
    wait:bool ->
    t ->
    ([> `Running | `Finalised of Stats.Latest_gc.stats ], Args.Errs.t) result
    Lwt.t
  (** [finalise ~wait t] returns the state of the GC process.

      If [wait = true], the call will block until GC finishes. *)

  val on_finalise :
    t -> ((Stats.Latest_gc.stats, Args.Errs.t) result -> unit Lwt.t) -> unit
  (** Attaches a callback to the GC process, which will be called when the GC
      finalises. *)

  val cancel : t -> bool
end

module type Sigs = sig
  module type Args = Args
  module type S = S
end
