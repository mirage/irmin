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

module Contents = struct
  include Ir_contents
  module type S = Ir_s.CONTENTS
  module type STORE = Ir_s.CONTENTS_STORE
end
module Merge = Ir_merge
module Ref = struct
  include Ir_ref
  module type S = Ir_s.REF
  module type STORE = Ir_s.REF_STORE
end
module Task = Ir_task
module View = Ir_view.Make
module type VIEW = Ir_view.S
module Dot = Ir_dot.Make
module type S = Ir_s.STORE_EXT

module Hash = struct
  include Ir_hash
  module type S = Ir_s.HASH
end
module Path = struct
  include Ir_path
  module type STEP = Ir_s.STEP
  module type S = Ir_s.PATH
end

module Make_with_metadata
    (M: Ir_s.METADATA)
    (AO: Ir_s.AO_MAKER)
    (RW: Ir_s.RW_MAKER)
    (C: Ir_s.CONTENTS)
    (R: Ir_s.REF)
    (H: Ir_s.HASH) =
struct
  module X = struct
    module XContents = struct
      include AO(H)(C)
      module Key = H
      module Val = C
    end
    module Contents = Ir_contents.Store(XContents)
    module Node = struct
      module AO = struct
        module Key = H
        module Val = Ir_node.Make (H)(H)(C.Path)(M)
        module Path = C.Path
        include AO (Key)(Val)
      end
      include Ir_node.Store(Contents)(AO)
      let create = AO.create
    end
    module Commit = struct
      module AO = struct
        module Key = H
        module Val = Ir_commit.Make (H)(H)
        include AO (Key)(Val)
      end
      include Ir_commit.Store(Node)(AO)
      let create = AO.create
    end
    module Ref = struct
      module Key = R
      module Val = H
      include RW (Key)(Val)
    end
    module Slice = Ir_slice.Make(Contents)(Node)(Commit)
    module Sync = Ir_sync.None(H)(R)
    module Repo = struct
      type t = {
        config: Ir_conf.t;
        contents: Contents.t;
        node: Node.t;
        commit: Commit.t;
        ref_store: Ref.t;
      }
      let ref_t t = t.ref_store
      let commit_t t = t.commit
      let node_t t = t.node
      let contents_t t = t.contents

      let create config =
        XContents.create config >>= fun contents ->
        Node.create config      >>= fun node ->
        Commit.create config    >>= fun commit ->
        Ref.create config       >>= fun ref_store ->
        let node = contents, node in
        let commit = node, commit in
        return { contents; node; commit; ref_store; config }
    end
  end
  include Ir_bc.Make(X)
end

module Make = Make_with_metadata(Ir_node.No_metadata)
module Make_ext = Ir_bc.Make

module type RO = Ir_s.RO_STORE
module type AO = Ir_s.AO_STORE
module type LINK = Ir_s.LINK_STORE
module type RW = Ir_s.RW
module type RRW = Ir_s.REACTIVE
module type HRW = Ir_s.HIERARCHICAL
module type BC = Ir_s.STORE
module Hum = Ir_hum

type task = Task.t
type config = Ir_conf.t
type 'a diff = 'a Ir_watch.diff

module type AO_MAKER = Ir_s.AO_MAKER

module type LINK_MAKER = Ir_s.LINK_MAKER

module type RAW = Tc.S0 with type t = Cstruct.t
module type AO_MAKER_RAW =
  functor (K: Ir_s.HASH) ->
  functor (V: RAW) ->
  AO with type key = K.t and type value = V.t

module type RW_MAKER = Ir_s.RW_MAKER
module type S_MAKER = Ir_s.MAKER

module Private = struct
  module Conf = Ir_conf
  module Node = struct
    include Ir_node
    module type S = Ir_s.NODE
    module type STORE = Ir_s.NODE_STORE
  end
  module Commit = struct
    include Ir_commit
    module type S = Ir_s.COMMIT
    module type STORE = Ir_s.COMMIT_STORE
  end
  module Slice = struct
    include Ir_slice
    module type S = Ir_s.SLICE
  end
  module Sync = struct
    include Ir_sync
    module type S = Ir_s.SYNC
  end
  module type S = Ir_s.PRIVATE
  module Watch = Ir_watch
  module Lock = Ir_lock
end

let version = Ir_version.current

module History = Graph.Persistent.Digraph.ConcreteBidirectional(Hash.SHA1)

module type SYNC = Ir_sync_ext.STORE
module Sync = Ir_sync_ext.Make

type remote = Ir_sync_ext.remote

let remote_store (type t) (module M: S with type t = t) (t:t) =
  let module X = (M: Ir_s.STORE_EXT with type t = t) in
  Ir_sync_ext.remote_store (module X) t

let remote_uri = Ir_sync_ext.remote_uri

module type T = S with type branch_id = string and type commit_id = Hash.SHA1.t

let with_hrw_view (type store) (type path) (type view)
  (module V : VIEW with type t = view and type db = store and type key = path)
  (t:store) ~path strat (ops: view -> unit Lwt.t) =
  V.of_path t path >>= fun view ->
  ops view >>= fun () ->
  match strat with
  | `Update -> V.update_path t path view >>= fun () -> Merge.OP.ok ()
  | `Rebase -> V.rebase_path t path view
  | `Merge  -> V.merge_path t path view

module Metadata = struct
  module type S = Ir_s.METADATA
  module None = Ir_node.No_metadata
end
