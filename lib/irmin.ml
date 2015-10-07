(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt

module Contents = Ir_contents
module Merge = Ir_merge
module Ref = Ir_tag
module Task = Ir_task
module View = Ir_view.Make
module type VIEW = Ir_view.S
module Dot = Ir_dot.Make
module type S = Ir_bc.STORE_EXT

module Hash = Ir_hash
module Path = Ir_path
module Make = Ir_s.Make
module Make_ext = Ir_bc.Make_ext

module type RO = Ir_ro.STORE
module type AO = Ir_ao.STORE
module type LINK = Ir_link.STORE
module type RW = Ir_rw.STORE
module type RRW = Ir_rw.REACTIVE
module type HRW = Ir_rw.HIERARCHICAL
module type BC = Ir_bc.STORE
module Hum = Ir_hum

type task = Task.t
type config = Ir_conf.t
type 'a diff = 'a Ir_watch.diff

module type AO_MAKER = Ir_ao.MAKER

module type LINK_MAKER = Ir_link.MAKER

module type RAW = Tc.S0 with type t = Cstruct.t
module type AO_MAKER_RAW =
  functor (K: Ir_hash.S) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

module type RW_MAKER = Ir_rw.MAKER
module type S_MAKER = Ir_s.MAKER

module Private = struct
  module Conf = Ir_conf
  module Node = Ir_node
  module Commit = Ir_commit
  module Slice = Ir_slice
  module Sync = Ir_sync
  module type S = Ir_bc.PRIVATE
  module Watch = Ir_watch
  module Lock = Ir_lock
end

let version = Ir_version.current

module History = Graph.Persistent.Digraph.ConcreteBidirectional(Hash.SHA1)

module type SYNC = Ir_sync_ext.STORE
module Sync = Ir_sync_ext.Make

type remote = Ir_sync_ext.remote

let remote_store (type t) (module M: S with type t = t) (t:t) =
  let module X = (M: Ir_bc.STORE_EXT with type t = t) in
  Ir_sync_ext.remote_store (module X) t

let remote_uri = Ir_sync_ext.remote_uri

module type BASIC = S with type branch_id = string and type head = Hash.SHA1.t

module type T = S with type branch_id = string and type head = Hash.SHA1.t

let with_hrw_view (type store) (type path) (type view)
  (module V : VIEW with type t = view and type db = store and type key = path)
  (t:store) ~path strat (ops: view -> unit Lwt.t) =
  V.of_path t path >>= fun view ->
  ops view >>= fun () ->
  match strat with
  | `Update -> V.update_path t path view >>= fun () -> Merge.OP.ok ()
  | `Rebase -> V.rebase_path t path view
  | `Merge  -> V.merge_path t path view
