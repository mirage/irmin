(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

open Irmin.Export_for_backends
module Hash = Irmin.Hash.SHA1

module Key = struct
  include Irmin.Hash.SHA1

  let pp = Irmin.Type.pp t
  let equal = Irmin.Type.(unstage (equal t))
end

module Value = struct
  include Irmin.Contents.String

  let pp = Fmt.string
  let equal = String.equal

  type hash = Key.t

  module H = Irmin.Hash.Typed (Key) (Irmin.Contents.String)

  let hash = H.hash
end

module type S = sig
  include
    Irmin.Content_addressable.S with type key = Key.t and type value = Value.t

  val v : unit -> read t Lwt.t
end

module Append_only = Irmin_mem.Append_only
module Content_addressable = Irmin.Content_addressable.Make (Append_only)

module Mem = struct
  include Content_addressable.Make (Key) (Value)

  let v () = v @@ Irmin_mem.config ()
end

module MemChunk = struct
  include Content_addressable.Make (Key) (Value)

  let small_config = Irmin_chunk.config ~min_size:44 ~size:44 ()
  let v () = v small_config
end

let init () = Lwt.return_unit

let store =
  Irmin_test.store
    (module Irmin.Maker
              (Irmin_chunk.Content_addressable
                 (Append_only))
                 (Irmin_mem.Atomic_write))
    (module Irmin.Metadata.None)

let config = Irmin_chunk.config ()

let clean () =
  let (module S : Irmin_test.S) = store in
  let module P = S.Private in
  let clear repo =
    Lwt.join
      [
        P.Commit.clear (P.Repo.commit_t repo);
        P.Node.clear (P.Repo.node_t repo);
        P.Contents.clear (P.Repo.contents_t repo);
        P.Branch.clear (P.Repo.branch_t repo);
      ]
  in
  let* repo = S.Repo.v config in
  let* () = clear repo in
  S.Repo.close repo

let suite =
  { Irmin_test.name = "CHUNK"; init; store; config; clean; stats = None }
