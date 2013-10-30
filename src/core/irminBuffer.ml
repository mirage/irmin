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

type ba = Cstruct.buffer

type t = {
  mutable buffer: Cstruct.t;
}

let to_ba t =
  t.buffer.Cstruct.buffer

let of_ba ba =
  let buffer = Cstruct.of_bigarray ba in
 { buffer }

let length t =
  Cstruct.len t.buffer

let length_ba ba =
  Bigarray.Array1.dim ba

let debug fmt = IrminLog.debug "buf" fmt

external unsafe_blit_bigstring_to_string :
  Cstruct.buffer -> int -> string -> int -> int -> unit
  = "caml_blit_bigstring_to_string" "noalloc"

let dump_ba ?msg ba =
  if IrminLog.debug_enabled () then
    let len = length_ba ba in
    let str = String.create len in
    unsafe_blit_bigstring_to_string ba 0 str 0 len;
    let msg = match msg with None -> "" | Some msg -> msg ^ " " in
    debug "%s\027[33m[[ %S ]]\027[m" msg str

let dump ?msg t =
  if IrminLog.debug_enabled () then
    let dbg = Cstruct.debug t.buffer in
    let str = Cstruct.to_string (Cstruct.shift t.buffer (-t.buffer.Cstruct.off)) in
    let msg = match msg with None -> "" | Some msg -> msg ^ " " in
    debug "%s\027[33m[[ %s %S ]]\027[m" msg dbg str

exception Parse_error of string

let parse_error_buf buf fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31mParse error:\027[m %s\n" str;
      dump buf;
      raise (Parse_error str)
    ) fmt

let parse_error fmt =
  Printf.kprintf (fun str ->
      Printf.eprintf "\027[31mParse error:\027[m %s\n" str;
      raise (Parse_error str)
    ) fmt

(* XXX: do not 'x' the array *)
let create_aux len =
  let buffer = Cstruct.create len in
  let str = String.make len 'x' in
  Cstruct.blit_from_string str 0 buffer 0 len;
  buffer

let create_ba len =
  let buffer = create_aux len in
  buffer.Cstruct.buffer

let create len =
  let buffer = create_aux len in
  { buffer }

let set t len fn c =
  debug "set (%d)" len;
  dump ~msg:"-->" t;
  fn t.buffer 0 c;
  t.buffer <- Cstruct.shift t.buffer len;
  dump ~msg:"<--" t

let set_char t c =
  set t 1 Cstruct.set_char c

let set_uint8 t c =
  set t 1 Cstruct.set_uint8 c

let set_uint16 t c =
  set t 2 Cstruct.BE.set_uint16 c

let set_uint32 t c =
  set t 4 Cstruct.BE.set_uint32 c

let set_uint64 t c =
  set t 8 Cstruct.BE.set_uint64 c

let set_string t str =
  let len = String.length str in
  set t len (fun _ _ _ ->
      Cstruct.blit_from_string str 0 t.buffer 0 len;
    ) str

let get t n fn =
  let i = fn t.buffer 0 in
  t.buffer <- Cstruct.shift t.buffer n;
  i

let get_char t =
  get t 1 Cstruct.get_char

let get_uint8 t =
  get t 1 Cstruct.get_uint8

let get_uint16 t =
  get t 2 Cstruct.BE.get_uint16

let get_uint32 t =
  get t 4 Cstruct.BE.get_uint32

let get_uint64 t =
  get t 8 Cstruct.BE.get_uint64

let get_string t len =
  if len = 0 then ""
  else
    let str = String.create len in
    get t len (fun _ _ ->
        Cstruct.blit_to_string t.buffer 0 str 0 len;
      );
    str
