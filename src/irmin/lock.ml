(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = Lock_intf.S

module Make (IO : IO.S) (K : Type.S) = struct
  open IO.Syntax
  module Mutex = IO.Mutex

  module K = struct
    type t = K.t

    let hash = Hashtbl.hash
    let equal = Type.(unstage (equal K.t))
  end

  module KHashtbl = Hashtbl.Make (K)

  type key = K.t
  type t = { global : Mutex.t; locks : Mutex.t KHashtbl.t }

  let v () = { global = Mutex.create (); locks = KHashtbl.create 1024 }
  let stats t = KHashtbl.length t.locks

  let lock t key () =
    let lock =
      try KHashtbl.find t.locks key
      with Not_found ->
        let lock = Mutex.create () in
        KHashtbl.add t.locks key lock;
        lock
    in
    IO.return lock

  let unlock t key () =
    let () =
      if KHashtbl.mem t.locks key then
        let lock = KHashtbl.find t.locks key in
        if Mutex.is_empty lock then KHashtbl.remove t.locks key
    in
    IO.return ()

  let with_lock t k fn =
    let* lock = Mutex.with_lock t.global (lock t k) in
    let* r = Mutex.with_lock lock fn in
    let+ () = Mutex.with_lock t.global (unlock t k) in
    r
end
