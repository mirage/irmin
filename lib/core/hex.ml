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

let hexa = "0123456789abcdef"

let encode s =
  let n = String.length s in
  let r = String.create (n*2) in
  for i = 0 to n-1 do
    let x = Char.code s.[i] in
    r.[i*2]   <- hexa.[x lsr 4];
    r.[i*2+1] <- hexa.[x land 0xf];
  done;
  r

let invalid_arg fmt =
  Printf.ksprintf (fun str -> raise (Invalid_argument str)) fmt

let decode_char c = match c with
  | '0'..'9' -> Char.code c - 48 (* Char.code '0' *)
  | 'A'..'F' -> Char.code c - 55 (* Char.code 'A' + 10 *)
  | 'a'..'f' -> Char.code c - 87 (* Char.code 'a' + 10 *)
  | _ -> invalid_arg "Hex.decode: %d is an invalid char" (Char.code c)

let decode s =
  let n = String.length s in
  if n mod 2 <> 0 then
    invalid_arg "Hex.decode: %d is not a valid hex string size" n;
  let char i = Char.chr (decode_char s.[i] lsl 4 + decode_char s.[i+1]) in
  let r = String.create (n/2) in
  for i = 0 to n/2 - 1 do
    r.[i] <- char (2 * i);
  done;
  r
