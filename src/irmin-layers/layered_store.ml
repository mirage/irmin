(*
 * Copyright (c) 2013-2020 Ioana Cristescu <ioana@tarides.com>
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

open! Import
open Store_properties

let src = Logs.Src.create "irmin.layers" ~doc:"Irmin layered store"

module Log = (val Logs.src_log src : Logs.LOG)

module Content_addressable = struct
  module type Store = sig
    include Irmin.Content_addressable.S
    include Batch with type 'a t := 'a t
    include Of_config with type 'a t := 'a t
    include Closeable with type 'a t := 'a t
  end

  module Make
      (K : Irmin.Type.S)
      (V : Irmin.Type.S)
      (CA : Store with type key = K.t and type value = V.t) :
    Store with type key = CA.key and type value = CA.value = struct
    include CA
  end
end
