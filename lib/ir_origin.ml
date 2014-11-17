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

open Printf
open Sexplib.Std
open Bin_prot.Std

module type S = sig
  include Tc.I0
  val create: ?date:int64 -> ?id:string -> ('a, unit, string, t) format4 -> 'a
  val date: t -> int64
  val id: t -> string
  val message: t -> string
  val string_of_date: int64 -> string
end

module M = struct
  type t = {
    date: int64;
    id  : string;
    msg : string;
  } with bin_io, compare, sexp
end

include Tc.I0(M)

let date_hook =
  let c = ref 0L in
  ref (fun () -> c := Int64.add !c 1L; !c)

let set_date f =
  date_hook := f

let id_hook =
  let r = string_of_int (Random.int 1024) in
  ref (fun () -> r)

let set_id f =
  id_hook := f

open M

let create ?date ? id fmt =
  let date = match date with
    | None   -> !date_hook ()
    | Some d -> d in
  let id = match id with
    | None   -> !id_hook ()
    | Some i -> i in
  ksprintf (fun msg ->
      { M.date; id; msg }
    ) fmt

let date t = t.date
let id t = t.id
let message t = t.msg

let string_of_date_hook = ref Int64.to_string

let string_of_date d =
  !string_of_date_hook d

let set_string_of_date fn =
  string_of_date_hook := fn
