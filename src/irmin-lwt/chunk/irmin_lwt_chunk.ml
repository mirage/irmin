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

(* Lwt-flavoured shim over Irmin 4's irmin-chunk meta-backend. *)

let run = Lwt_eio.run_eio

module Conf = Irmin_chunk.Conf

let config = Irmin_chunk.config

(* Bridge a Lwt-typed [Irmin_lwt.Append_only.Maker] to its Eio counterpart,
   feed it through [Irmin_chunk.Content_addressable] (which produces an Eio
   [Irmin.Content_addressable.Maker]), and bridge the result back to a
   Lwt-typed [Irmin_lwt.Content_addressable.Maker]. *)
module Content_addressable (S : Irmin_lwt.Append_only.Maker) :
  Irmin_lwt.Content_addressable.Maker =
functor
  (H : Irmin_lwt.Hash.S)
  (V : Irmin_lwt.Type.S)
  ->
  struct
    (* Lift S to an Eio-typed Append_only.Maker. *)
    module S_eio (K : Irmin_lwt.Type.S) (V : Irmin_lwt.Type.S) =
      Irmin_lwt.Lwt_to_eio.Append_only (S) (K) (V)

    module Eio_CA = Irmin_chunk.Content_addressable (S_eio)
    module M = Eio_CA (H) (V)

    type 'a t = 'a M.t
    type key = M.key
    type value = M.value

    let v c = run (fun () -> M.v c)
    let mem t k = run (fun () -> M.mem t k)
    let find t k = run (fun () -> M.find t k)
    let add t v = run (fun () -> M.add t v)
    let unsafe_add t k v = run (fun () -> M.unsafe_add t k v)
    let close t = run (fun () -> M.close t)

    let batch t f =
      run (fun () -> M.batch t (fun rw -> Lwt_eio.Promise.await_lwt (f rw)))
  end
