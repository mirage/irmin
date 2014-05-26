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

open Core_kernel.Std
open Lwt
open IrminMerge.OP

type origin = IrminOrigin.t

module Log = Log.Make(struct let section = "SNAPSHOT" end)

module type STORE = sig
  type t
  type path
  type state
  val create: t -> state Lwt.t
  val revert: t -> state -> unit Lwt.t
  val merge: t -> ?origin:IrminOrigin.t -> state -> unit IrminMerge.result Lwt.t
  val merge_exn: t -> ?origin:IrminOrigin.t -> state -> unit Lwt.t
  val watch: t -> path -> (path * state) Lwt_stream.t
end

module Make (S: IrminBranch.STORE) = struct

  module Block = S.Block
  module Tag = S.Tag

  module K = Block.Key
  type state = Block.key

  type t = S.t
  type path = S.key

  let create t =
    Tag.read_exn (S.tag_t t) (S.branch t)

  let revert t r =
    Tag.update (S.tag_t t) (S.branch t) r

  let merge t ?origin c1 =
    let origin = match origin with
      | None   -> IrminOrigin.create "Merge snapshot %s"
                    (K.to_string c1)
      | Some o -> o in
    S.merge_commit t ~origin c1

  let merge_exn t ?origin c1 =
    merge ?origin t c1 >>=
    IrminMerge.exn

  let watch =
    S.watch_node

end
