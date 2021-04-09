(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

module type Maker = functor (Config : Config.S) -> sig
  module Make
      (M : Irmin.Metadata.S)
      (C : Irmin.Contents.S)
      (P : Irmin.Path.S)
      (B : Irmin.Branch.S)
      (H : Irmin.Hash.S) : sig
    include
      Irmin.S
        with type key = P.t
         and type step = P.step
         and type metadata = M.t
         and type contents = C.t
         and type branch = B.t
         and type hash = H.t
         and type Private.Remote.endpoint = unit

    include Store.S with type repo := repo

    val reconstruct_index : ?output:string -> Irmin.config -> unit

    val integrity_check_inodes :
      ?heads:commit list ->
      repo ->
      ([> `Msg of string ], [> `Msg of string ]) result Lwt.t
  end
end

module type Sigs = sig
  module type Maker = Maker
end
