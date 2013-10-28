(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let debug_mode =
  let debug =
    try match Sys.getenv "IRMIN_DEBUG" with "" | "0" -> false | _ -> true
    with Not_found -> false in
  ref debug

let set_debug_mode b =
  debug_mode := b

let debug_enabled () =
  !debug_mode

let debug section fmt =
  if !debug_mode then
    Printf.fprintf stderr ("\027[36m%15s\027[m "^^fmt^^"\n%!") section
  else
    Printf.ifprintf stderr fmt

let info section fmt =
  if !debug_mode then
    Printf.fprintf stderr ("\027[33m%15s\027[m "^^fmt^^"\n%!") section
  else
    Printf.ifprintf stderr fmt

let error section fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31m%15s\027[m Error: %s\n%!" section str
    ) fmt

let msg fmt =
  Printf.kprintf (fun str ->
      Printf.printf "%s\n%!" str
    ) fmt
