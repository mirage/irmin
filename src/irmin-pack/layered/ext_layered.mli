(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESIrmin. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

val config :
  ?conf:Irmin.config ->
  ?lower_root:string ->
  ?upper_root1:string ->
  ?upper_root0:string ->
  ?copy_in_upper:bool ->
  ?with_lower:bool ->
  ?blocking_copy_size:int ->
  unit ->
  Irmin.config

module Make
    (Config : Irmin_pack.Config.S)
    (Metadata : Irmin.Metadata.S)
    (Contents : Irmin.Contents.S)
    (Path : Irmin.Path.S)
    (Branch : Irmin.Branch.S)
    (Hash : Irmin.Hash.S)
    (N : Irmin.Private.Node.S
           with type metadata = Metadata.t
            and type hash = Hash.t
            and type step = Path.step)
    (CT : Irmin.Private.Commit.S with type hash = Hash.t) :
  S.STORE
    with type key = Path.t
     and type contents = Contents.t
     and type branch = Branch.t
     and type hash = Hash.t
     and type step = Path.step
     and type metadata = Metadata.t
     and type Key.step = Path.step
