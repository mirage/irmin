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

(* Lwt-flavoured shim over Irmin 4's in-memory backend. Each Lwt-typed
   operation forwards to its Irmin 4 counterpart through Lwt_eio.run_eio. *)

let run = Lwt_eio.run_eio

module Conf = Irmin_mem.Conf

let config = Irmin_mem.config

module Append_only (K : Irmin_lwt.Type.S) (V : Irmin_lwt.Type.S) = struct
  module M = Irmin_mem.Append_only (K) (V)

  type 'a t = 'a M.t
  type key = K.t
  type value = V.t

  let v c = run (fun () -> M.v c)
  let mem t k = run (fun () -> M.mem t k)
  let find t k = run (fun () -> M.find t k)
  let add t k v = run (fun () -> M.add t k v)
  let close t = run (fun () -> M.close t)

  let batch t f =
    run (fun () -> M.batch t (fun rw -> Lwt_eio.Promise.await_lwt (f rw)))
end

module Content_addressable (H : Irmin_lwt.Hash.S) (V : Irmin_lwt.Type.S) =
struct
  module M = Irmin_mem.Content_addressable (H) (V)

  type 'a t = 'a M.t
  type key = H.t
  type value = V.t

  (* Irmin 4's Content_addressable.S does not expose a [module Key] (its keys
     are concrete [Hash.t] values); our Lwt-flavoured Content_addressable.S
     inherits Indexable.S which does. Build it from Hash. *)
  module Key = Irmin_lwt.Key.Of_hash (H)

  let v c = run (fun () -> M.v c)
  let mem t k = run (fun () -> M.mem t k)
  let find t k = run (fun () -> M.find t k)
  let add t v = run (fun () -> M.add t v)
  let unsafe_add t h v = run (fun () -> M.unsafe_add t h v)
  let close t = run (fun () -> M.close t)

  let batch t f =
    run (fun () -> M.batch t (fun rw -> Lwt_eio.Promise.await_lwt (f rw)))
end

module Atomic_write (K : Irmin_lwt.Type.S) (V : Irmin_lwt.Type.S) = struct
  module M = Irmin_mem.Atomic_write (K) (V)

  type t = M.t
  type key = K.t
  type value = V.t
  type watch = M.watch

  let v c = run (fun () -> M.v c)
  let mem t k = run (fun () -> M.mem t k)
  let find t k = run (fun () -> M.find t k)
  let set t k v = run (fun () -> M.set t k v)

  let test_and_set t k ~test ~set =
    run (fun () -> M.test_and_set t k ~test ~set)

  let remove t k = run (fun () -> M.remove t k)
  let list t = run (fun () -> M.list t)

  let watch t ?init f =
    run (fun () ->
        M.watch t ?init (fun k d -> Lwt_eio.Promise.await_lwt (f k d)))

  let watch_key t k ?init f =
    run (fun () ->
        M.watch_key t k ?init (fun d -> Lwt_eio.Promise.await_lwt (f d)))

  let unwatch t w = run (fun () -> M.unwatch t w)
  let close t = run (fun () -> M.close t)
  let clear t = run (fun () -> M.clear t)
end

(* Plug the wrapped backends into Irmin_lwt.Maker / KV_maker. *)

include Irmin_lwt.Maker (Content_addressable) (Atomic_write)
module KV = Irmin_lwt.KV_maker (Content_addressable) (Atomic_write)
