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
  val with_lock : t -> key -> (unit -> 'a) -> 'a
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
  type t = { global : Eio.Mutex.t; locks : Eio.Mutex.t KHashtbl.t }

  let v () = { global = Eio.Mutex.create (); locks = KHashtbl.create 1024 }
  let stats t = KHashtbl.length t.locks

  let lock t key () =
    let lock =
      try KHashtbl.find t.locks key
      with Not_found ->
        let lock = Eio.Mutex.create () in
        KHashtbl.add t.locks key lock;
        lock
    in
    lock

  let unlock t key () =
    if KHashtbl.mem t.locks key then
      let lock = KHashtbl.find t.locks key in
      (* TODO: is_empty not is_locked *)
      if Eio.Mutex.try_lock lock then KHashtbl.remove t.locks key

  let with_lock t k fn =
    let lock = Eio.Mutex.use_rw ~protect:true t.global (lock t k) in
    let r = Eio.Mutex.use_rw ~protect:true lock fn in
    Eio.Mutex.use_rw ~protect:true t.global (unlock t k);
    r
end
