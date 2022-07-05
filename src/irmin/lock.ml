(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import

module type S = sig
  type key
  type t

  val v : unit -> t
  val with_lock : t -> key -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  val stats : t -> int
end

module Make (K : Type.S) = struct
  module K = struct
    type t = K.t

    let hash = Hashtbl.hash
    let equal = Type.(unstage (equal K.t))
  end

  module KHashtbl = Hashtbl.Make (K)

  type key = K.t
  type t = { global : Lwt_mutex.t; locks : Lwt_mutex.t KHashtbl.t }

  let v () = { global = Lwt_mutex.create (); locks = KHashtbl.create 1024 }
  let stats t = KHashtbl.length t.locks

  let lock t key () =
    let lock =
      try KHashtbl.find t.locks key
      with Not_found ->
        let lock = Lwt_mutex.create () in
        KHashtbl.add t.locks key lock;
        lock
    in
    Lwt.return lock

  let unlock t key () =
    let () =
      if KHashtbl.mem t.locks key then
        let lock = KHashtbl.find t.locks key in
        if Lwt_mutex.is_empty lock then KHashtbl.remove t.locks key
    in
    Lwt.return_unit

  let with_lock t k fn =
    let* lock = Lwt_mutex.with_lock t.global (lock t k) in
    let* r = Lwt_mutex.with_lock lock fn in
    Lwt_mutex.with_lock t.global (unlock t k) >>= fun () -> Lwt.return r
end
