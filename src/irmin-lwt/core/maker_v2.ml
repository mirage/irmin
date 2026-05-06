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

(** Lwt-typed [Store.S] built on top of Irmin 4 (Eio).

    [Make (CA) (AW) (S)] builds an inner [Irmin.Generic_key.S] by feeding
    Lwt-to-Eio adapted backend Makers and the bridged Schema into [Irmin.Maker],
    and then delegates the bulk of the wrap to {!Wrap_store.Make}. *)

module Make
    (CA : Content_addressable.Maker)
    (AW : Atomic_write.Maker)
    (S : Schema.S) =
struct
  module CA_eio (H : Irmin.Hash.S) (V : Irmin.Type.S) =
    Lwt_to_eio.Content_addressable (CA) (H) (V)

  module AW_eio (K : Irmin.Type.S) (V : Irmin.Type.S) =
    Lwt_to_eio.Atomic_write (AW) (K) (V)

  (* Bridge the user's Lwt-typed schema into an Irmin 4-compatible one
     (Metadata.merge and Contents.merge are converted from Lwt to Eio). *)
  module Schema_eio = Lwt_to_eio.Schema (S)
  module Inner_maker = Irmin.Maker (CA_eio) (AW_eio)
  module Inner = Inner_maker.Make (Schema_eio)
  include Wrap_store.Make (S) (Schema_eio) (Inner)
end
