(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Merge = Ir_merge
module Contents = Ir_contents
module Tag = Ir_tag
module Task = Ir_task
module Conf = Ir_conf
module View = Ir_view.Make
module Snapshot = Ir_snapshot.Make
module Dot = Ir_dot.Make
module type S = Ir_s.STORE

module Hash = Ir_hash
module Path = Ir_path
module Make = Ir_s.Make
module Make_ext = Ir_s.Make_ext

module Node = Ir_node
module Commit = Ir_commit

module type RO = Ir_ro.STORE
module type AO = Ir_ao.STORE
module type RW = Ir_rw.STORE
module type HRW = Ir_rw.HIERARCHICAL
module type BC = Ir_bc.STORE
module Hum = Ir_hum

type task = Task.t
type config = Ir_conf.t

module type AO_MAKER = Ir_ao.MAKER
module type RW_MAKER = Ir_rw.MAKER
module type BC_MAKER = Ir_bc.MAKER
module type S_MAKER = Ir_s.MAKER

module Private = struct
  module Slice = Ir_slice
  module Make = Ir_bc.Make
  module Sync = Ir_sync
  module type S = Ir_bc.PRIVATE
end

module Watch = Ir_watch

let version = Ir_version.current

module Sync (S: Ir_s.STORE) = struct
  (* XXX: cannot "include Ir_sync_ext.Make" because equality does not
     lift to signatures in pack, see below. *)
  include Ir_sync_ext.Make(S)
  let store (type t) (module M: S with type t = t) (t:t) =
    let module X = (M: Ir_s.STORE with type t = t) in
    store (module X) t
end
