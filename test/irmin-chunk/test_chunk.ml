 (*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Hash = Irmin.Hash.SHA1

module Key = struct
  include Irmin.Hash.SHA1
  let equal x y =
    Cstruct.equal (to_raw x) (to_raw y)
end

module Value = struct
  include Irmin.Contents.Cstruct
  let equal x y =
    Cstruct.equal x y
end

module type S = sig
  include Irmin.AO with type key = Key.t and type value = Value.t
  val create: unit -> t Lwt.t
end

module Mem = struct
  include Irmin_mem.AO(Key)(Value)
  let create () = v @@ Irmin_mem.config ()
end

module MemChunk = struct
  include Irmin_chunk.AO(Irmin_mem.AO)(Key)(Value)
  let small_config = Irmin_chunk.config ~min_size:44 ~size:44 ()
  let create () = v small_config
end

module MemChunkStable = struct
  include Irmin_chunk.AO_stable(Irmin_mem.Link)(Irmin_mem.AO)(Key)(Value)
  let small_config = Irmin_chunk.config ~min_size:44 ~size:44 ()
  let create () = v small_config
end

