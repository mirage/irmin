(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

module Type = Ir_type
module Diff = Ir_diff

module Contents = struct
  include Ir_contents
  module type S0 = Ir_s.S0
  module type Raw = Ir_s.RAW
  module type Conv = Ir_s.CONV
  module type S = Ir_s.CONTENTS
  module type STORE = Ir_s.CONTENTS_STORE
end
module Merge = Ir_merge
module Branch = struct
  include Ir_branch
  module type S = Ir_s.BRANCH
  module type STORE = Ir_s.BRANCH_STORE
end
module Task = Ir_task
module Dot = Ir_dot.Make
module Hash = struct
  include Ir_hash
  module type S = Ir_s.HASH
end
module Path = struct
  include Ir_path
  module type S = Ir_s.PATH
end

module Conv2Raw (C: Contents.Conv): Contents.Raw with type t = C.t =
struct
  include C
  let raw t = Cstruct.of_string (Fmt.to_to_string C.pp t)
end

module S02Raw (C: Contents.S0): Contents.Raw with type t = C.t =
struct
  module C = struct
    include C
    let pp = Type.pp_json C.t

    let of_string j =
      match Type.decode_json C.t (Jsonm.decoder (`String j)) with
      | Ok t    -> `Ok t
      | Error e -> `Error e
  end
  include Conv2Raw(C)
end

module Make_with_metadata
    (M: Ir_s.METADATA)
    (AO: Ir_s.AO_MAKER)
    (RW: Ir_s.RW_MAKER)
    (C: Ir_s.CONTENTS)
    (P: Ir_s.PATH)
    (B: Ir_s.BRANCH)
    (H: Ir_s.HASH) =
struct

  module X = struct
    module XContents = struct
      include AO(H)(Conv2Raw(C))
      module Key = H
      module Val = C
    end
    module Contents = Ir_contents.Store(XContents)
    module Node = struct
      module AO = struct
        module Key = H
        module Val = Ir_node.Make (H)(H)(P)(M)
        include AO (Key)(S02Raw(Val))
      end
      include Ir_node.Store(Contents)(P)(M)(AO)
      let v = AO.v
    end
    module Commit = struct
      module AO = struct
        module Key = H
        module Val = Ir_commit.Make (H)(H)
        include AO (Key)(S02Raw(Val))
      end
      include Ir_commit.Store(Node)(AO)
      let v = AO.v
    end
    module Branch = struct
      module Key = B
      module Val = H
      include RW (Key)(Val)
    end
    module Slice = Ir_slice.Make(Contents)(Node)(Commit)
    module Sync = Ir_sync.None(H)(B)
    module Repo = struct
      type t = {
        config: Ir_conf.t;
        contents: Contents.t;
        node: Node.t;
        commit: Commit.t;
        branch: Branch.t;
      }
      let branch_t t = t.branch
      let commit_t t = t.commit
      let node_t t = t.node
      let contents_t t = t.contents

      let v config =
        XContents.v config >>= fun contents ->
        Node.v config      >>= fun node ->
        Commit.v config    >>= fun commit ->
        Branch.v config    >|= fun branch ->
        let node = contents, node in
        let commit = node, commit in
        { contents; node; commit; branch; config }
    end
  end
  include Ir_store.Make(X)
end

module Make = Make_with_metadata(Ir_node.No_metadata)
module Make_ext = Ir_store.Make

module type RO = Ir_s.RO
module type AO = Ir_s.AO
module type LINK = Ir_s.LINK
module type RW = Ir_s.RW
module type TREE = Ir_s.TREE
module type S = Ir_s.STORE

type task = Task.t
type config = Ir_conf.t
type 'a diff = 'a Ir_diff.t

module type AO_MAKER = Ir_s.AO_MAKER

module type LINK_MAKER = Ir_s.LINK_MAKER

module type RW_MAKER = Ir_s.RW_MAKER
module type S_MAKER = Ir_s.MAKER

module Private = struct
  module Conf = Ir_conf
  module Node = struct
    include Ir_node
    module type S = Ir_s.NODE
    module type GRAPH = Ir_s.NODE_GRAPH
    module type STORE = Ir_s.NODE_STORE
  end
  module Commit = struct
    include Ir_commit
    module type S = Ir_s.COMMIT
    module type STORE = Ir_s.COMMIT_STORE
    module type HISTORY = Ir_s.COMMIT_HISTORY
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

module type SYNC = Ir_s.SYNC_STORE
module Sync = Ir_sync_ext.Make

type remote = Ir_s.remote

let remote_store (type t) (module M: S with type t = t) (t:t) =
  let module X = (M: Ir_s.STORE with type t = t) in
  Ir_sync_ext.remote_store (module X) t

let remote_uri = Ir_sync_ext.remote_uri

module Metadata = struct
  module type S = Ir_s.METADATA
  module None = Ir_node.No_metadata
end
