(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

(** Lwt-typed Tezos schema + pack store.

    The Tezos schema ([Irmin_tezos.Schema]) is already Eio-typed and includes
    Tezos-specific customizations (BLAKE2B hash with Base58 prefix, V1
    pre-hashing for Node / Commit / Contents). We use it directly as
    [Schema_eio] in [Wrap_store.Make] -- bypassing [Lwt_to_eio.Schema_extended],
    which would otherwise replace the V1 pre-hashing with the default
    [Generic_key.Make] -- and construct a parallel Lwt-side [Schema] reusing the
    pure modules and bridging the Metadata / Contents merges. *)

module Conf = Irmin_tezos.Conf

(* The Tezos Eio-side Schema_eio_tezos. Aliased as a private module here; the
   re-export at the bottom of the file (after [Wrap_store.Make]'s [Schema =
   Schema_lwt]) cannot live alongside it. *)
module Schema_eio_tezos = Irmin_tezos.Schema
module Inner_maker = Irmin_pack_unix.Maker (Conf)
module Inner = Inner_maker.Make (Schema_eio_tezos)

(* Lwt-side Schema_eio_tezos.S parallel to [Irmin_tezos.Schema]. The pure modules
   (Hash / Branch / Info / Path) are reused directly because their module
   types have no Lwt.t. Metadata and Contents are bridged. *)
module Schema_lwt = struct
  module Hash = Schema_eio_tezos.Hash
  module Branch = Schema_eio_tezos.Branch
  module Info = Schema_eio_tezos.Info
  module Path = Schema_eio_tezos.Path

  module Metadata = struct
    type t = Schema_eio_tezos.Metadata.t

    let t = Schema_eio_tezos.Metadata.t
    let default = Schema_eio_tezos.Metadata.default

    let merge =
      Irmin_lwt.Lwt_to_eio.merge_of_eio Schema_eio_tezos.Metadata.t
        Schema_eio_tezos.Metadata.merge
  end

  module Contents = struct
    type t = Schema_eio_tezos.Contents.t

    let t = Schema_eio_tezos.Contents.t

    let merge =
      Irmin_lwt.Lwt_to_eio.merge_of_eio
        Irmin.Type.(option Schema_eio_tezos.Contents.t)
        Schema_eio_tezos.Contents.merge
  end
end

include Irmin_lwt.Wrap_store.Make (Schema_lwt) (Schema_eio_tezos) (Inner)
