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

open Import
open Irmin_pack.Inode

module type Persistent = sig
  include S

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

  include Irmin_pack.Checkable with type 'a t := 'a t and type hash := hash

  (* val reload : 'a t -> unit *)
  val integrity_check_inodes : [ `Read ] t -> key -> (unit, string) result Lwt.t

  module Pack :
    Pack_store.S
      with type file_manager = file_manager
       and type dict = dict
       and type dispatcher = dispatcher
       and type key := hash Pack_key.t
       and type hash := hash
       and type 'a t = 'a t

  module Raw :
    Raw
      with type t = Pack.value
       and type hash := hash
       and type key := hash Pack_key.t

  module Snapshot :
    Snapshot with type hash = hash and type metadata = Val.metadata

  val to_snapshot : Raw.t -> Snapshot.inode
  val of_snapshot : 'a t -> index:(hash -> key) -> Snapshot.inode -> value
  val purge_lru : 'a t -> unit
  val key_of_offset : 'a t -> int63 -> key
  val unsafe_find_no_prefetch : 'a t -> key -> value option
  val get_offset : 'a t -> key -> int63
  val get_length : 'a t -> key -> int
end

module type Sigs = sig
  module type S = S
  module type Persistent = Persistent

  module Make_persistent
      (H : Irmin.Hash.S)
      (Node : Irmin.Node.Generic_key.S
                with type hash = H.t
                 and type contents_key = H.t Pack_key.t
                 and type node_key = H.t Pack_key.t)
      (Inter : Internal
                 with type hash = H.t
                  and type key = H.t Pack_key.t
                  and type Snapshot.metadata = Node.metadata
                  and type Val.step = Node.step)
      (Pack : Pack_store.S
                with type hash = H.t
                 and type key = H.t Pack_key.t
                 and type value = Inter.Raw.t) :
    Persistent
      with type key = H.t Pack_key.t
       and type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step
       and type file_manager = Pack.file_manager
       and type dict = Pack.dict
       and type dispatcher = Pack.dispatcher
       and type value = Inter.Val.t
end
