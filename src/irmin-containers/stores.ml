(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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

(** Same as Irmin.Contents.S but with an explicit IO kind. *)
module type Contents = sig
  type io
  type t

  val t : t Irmin.Type.t
  val merge : unit -> (t option, io) Irmin.Merge.DSL.t
end

(** The signature for the backend input to the data structures. The Irmin stores
    of the data structures are constructed using modules of this type *)
module type Store_maker = functor
  (IO : Irmin.IO.S with type 'a t = 'a Lwt.t)
  (C : Contents with type io := Irmin.IO.Higher(IO).io)
  ->
  Irmin.S
    with type contents = C.t
     and type branch = string
     and type key = string list
     and type step = string

module type Cas_maker = sig
  module IO : Irmin.IO.S

  module CAS_Maker :
    Irmin.CONTENT_ADDRESSABLE_STORE_MAKER with type 'a io = 'a IO.t

  val config : Irmin.config
end
