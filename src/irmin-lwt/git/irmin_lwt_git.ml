(*
 * Copyright (c) 2026 Tarides
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

(* Lwt-flavoured shim over [Irmin_git_unix]. The user provides a Lwt-typed
   [Contents.S]; we bridge it to Eio, apply [Irmin_git_unix]'s git Maker, and
   wrap the resulting Eio store back to a Lwt-typed [Irmin_lwt.Generic_key.S]
   via [Wrap_store.Make]. The git-specific extras ([module Git], [git_commit],
   [git_of_repo], [repo_of_git], [remote]) are passed through. *)

let config = Irmin_git.config

module Maker (G : Irmin_git.G) = struct
  module G = G

  type endpoint = Mimic.ctx * Smart_git.Endpoint.t

  module Inner_maker = Irmin_git_unix.Maker (G)

  module KV (V : Irmin_lwt.Contents.S) = struct
    module V_eio = Irmin_lwt.Lwt_to_eio.Contents (V)
    module Inner = Inner_maker.KV (V_eio)

    (* Build a Lwt-typed [Schema.S] reusing the pure modules from Inner's
       Eio Schema (Hash, Branch, Info, Path are all module types without
       Lwt.t in either direction; same module value satisfies both Lwt
       and Eio Schema.S constraints). Metadata's [merge] is bridged. *)
    module Schema_lwt = struct
      module Hash = Inner.Schema.Hash
      module Branch = Inner.Schema.Branch
      module Info = Inner.Schema.Info
      module Path = Inner.Schema.Path

      module Metadata = struct
        type t = Inner.Schema.Metadata.t

        let t = Inner.Schema.Metadata.t
        let default = Inner.Schema.Metadata.default

        let merge =
          Irmin_lwt.Lwt_to_eio.merge_of_eio Inner.Schema.Metadata.t
            Inner.Schema.Metadata.merge
      end

      module Contents = V
    end

    include Irmin_lwt.Wrap_store.Make (Schema_lwt) (Inner.Schema) (Inner)

    (* Pass-through git-specific extras. [git_commit] returns [Lwt.t]
       upstream because ocaml-git is still Lwt-typed; we keep the same
       shape, no extra wrap. *)
    module Git = G

    let git_commit = Inner.git_commit
    let git_of_repo = Inner.git_of_repo
    let repo_of_git = Inner.repo_of_git
    let remote = Inner.remote
  end

  module Ref (V : Irmin_lwt.Contents.S) = struct
    module V_eio = Irmin_lwt.Lwt_to_eio.Contents (V)
    module Inner = Inner_maker.Ref (V_eio)

    module Schema_lwt = struct
      module Hash = Inner.Schema.Hash
      module Branch = Inner.Schema.Branch
      module Info = Inner.Schema.Info
      module Path = Inner.Schema.Path

      module Metadata = struct
        type t = Inner.Schema.Metadata.t

        let t = Inner.Schema.Metadata.t
        let default = Inner.Schema.Metadata.default

        let merge =
          Irmin_lwt.Lwt_to_eio.merge_of_eio Inner.Schema.Metadata.t
            Inner.Schema.Metadata.merge
      end

      module Contents = V
    end

    include Irmin_lwt.Wrap_store.Make (Schema_lwt) (Inner.Schema) (Inner)
    module Git = G

    let git_commit = Inner.git_commit
    let git_of_repo = Inner.git_of_repo
    let repo_of_git = Inner.repo_of_git
    let remote = Inner.remote
  end
end

module FS = struct
  include Maker (Git_unix.Store)
  module G = Git_unix.Store
end

module Mem = struct
  include Maker (Irmin_git.Mem)
  module G = Irmin_git.Mem
end
