(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

  val v : sw:Eio.Switch.t -> read t
end

module Append_only = Irmin_mem.Append_only

module Content_addressable =
  Irmin.Content_addressable.Make (Append_only) (Key) (Value)

module Mem = struct
  include Content_addressable

  let v ~sw = v ~sw @@ Irmin_mem.config ()
end

module MemChunk = struct
  include Content_addressable

  let small_config =
    Irmin_chunk.config ~min_size:44 ~size:44 (Irmin_mem.config ())

  let v ~sw = v ~sw small_config
end

let store =
  Irmin_test.store
    (module Irmin.Maker
              (Irmin_chunk.Content_addressable
                 (Append_only))
                 (Irmin_mem.Atomic_write))
    (module Irmin.Metadata.None)

let config = Irmin_chunk.config (Irmin_mem.config ())
let suite = Irmin_test.Suite.create ~name:"CHUNK" ~store ~config ()
