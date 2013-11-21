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

(* From OCaml's stdlib. See [Digest.to_hex] *)
let hex_encode s =
  let n = String.length s in
  let result = String.create (n*2) in
  for i = 0 to n-1 do
    String.blit (Printf.sprintf "%02x" (int_of_char s.[i])) 0 result (2*i) 2;
  done;
  result

(* From OCaml's stdlib. See [Digest.from_hex] *)
let hex_decode h =
  let n = String.length h in
  if n mod 2 <> 0 then (
    let msg =
      Printf.sprintf "hex_decode: wrong string size for %S (%d)" h (String.length h) in
    raise (Invalid_argument msg)
  );
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | c ->
      let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
      raise (Invalid_argument msg) in
  let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
  let result = String.create (n / 2) in
  for i = 0 to n/2 - 1 do
    result.[i] <- Char.chr (byte (2 * i));
  done;
  result

let sha1 str =
  let hash = Cryptokit.Hash.sha1 () in
  hash#add_string str;
  hash#result

let pretty_list f = function
  | [] -> "{}"
  | l  ->
    let buf = Buffer.create 1024 in
    let len = ref (List.length l - 1) in
    Buffer.add_string buf "{ ";
    List.iter (fun e ->
        Buffer.add_string buf (f e);
        if !len > 0 then Buffer.add_string buf ", ";
        decr len
      ) l;
    Buffer.add_string buf " }";
    Buffer.contents buf
