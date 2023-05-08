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

open! Import

(** A [Pack_store.S] is a closeable, persistent implementation of {!Indexable.S}
    that uses an append-only file of variable-length data blocks.

    Certain values in the data file are indexed by hash via a {!Pack_index.S}
    implementation, but not all of them need be. *)
module type S = sig
  include Irmin_pack.Indexable.S

  type file_manager
  type dict
  type dispatcher

  val v :
    config:Irmin.Backend.Conf.t ->
    fm:file_manager ->
    dict:dict ->
    dispatcher:dispatcher ->
    lru:Lru.t ->
    read t

  val cast : read t -> read_write t

  (** @inline *)
  include Irmin_pack.Checkable with type 'a t := 'a t and type hash := hash

  module Entry_prefix : sig
    type t = {
      hash : hash;
      kind : Pack_value.Kind.t;
      size_of_value_and_length_header : int option;
          (** Remaining bytes in the entry after reading the hash and the kind
              (i.e. the length of the length header + the value of the length
              header), if the entry has a length header (otherwise [None]).

              NOTE: the length stored in the index and in direct pack keys is
              the {!total_entry_length} (including the hash and the kind). *)
    }

    val total_entry_length : t -> int option
  end

  val read_and_decode_entry_prefix : off:int63 -> dispatcher -> Entry_prefix.t
  (** Read the entry prefix at offset [off]. *)

  val index_direct_with_kind : 'a t -> hash -> (key * Pack_value.Kind.t) option
  (** Returns the key and the kind of an object indexed by hash. *)

  val purge_lru : 'a t -> unit

  val key_of_offset : 'a t -> int63 -> key
  (** Returns the key associated with the offset. *)

  val unsafe_find_no_prefetch : 'a t -> key -> value option
  (** Similar to [unsafe_find], returns the value associated with the [key] in
      the store but without prefetching the [hash] and [length] of the children
      (or doing any integrity check). As a result, the produced children keys
      only contain their [offset] and are not usable without calling
      [key_of_offset] first. This function only exists to optimize the GC
      reachability traversal. *)

  val get_offset : 'a t -> key -> int63
  (** Returns the offset associated with the key. *)

  val get_length : 'a t -> key -> int
  (** Returns the length of the object associated with the key. *)
end

module type Sigs = sig
  exception Invalid_read of string
  exception Dangling_hash

  module type S = S

  module Make
      (Fm : File_manager.S)
      (Dict : Dict.S with module Fm = Fm)
      (Dispatcher : Dispatcher.S with module Fm = Fm)
      (Hash : Irmin.Hash.S with type t = Fm.Index.key)
      (Val : Pack_value.Persistent
               with type hash := Hash.t
                and type key := Hash.t Pack_key.t)
      (Errs : Io_errors.S with module Io = Fm.Io) :
    S
      with type key = Hash.t Pack_key.t
       and type hash = Hash.t
       and type value = Val.t
       and type file_manager = Fm.t
       and type dispatcher = Dispatcher.t
       and type dict = Dict.t
end
