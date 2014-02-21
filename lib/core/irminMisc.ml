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

open Core_kernel.Std

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
    | '0'..'9' -> Char.to_int c - Char.to_int '0'
    | 'A'..'F' -> Char.to_int c - Char.to_int 'A' + 10
    | 'a'..'f' -> Char.to_int c - Char.to_int 'a' + 10
    | c ->
      let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
      raise (Invalid_argument msg) in
  let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
  let result = String.create (n / 2) in
  for i = 0 to n/2 - 1 do
    result.[i] <- Char.of_int_exn (byte (2 * i));
  done;
  result

let is_valid_utf8 str =
  try
    Uutf.String.fold_utf_8 (fun _ _ -> function
        | `Malformed _ -> raise (Failure "utf8")
        | _ -> ()
      ) () str;
    true
  with Failure "utf8" -> false

  let json_encode str =
    if is_valid_utf8 str
    then Ezjsonm.string str
    else `O [ "hex", Ezjsonm.string (hex_encode str) ]

let json_decode = function
  | `String str               -> Some str
  | `O [ "hex", `String str ] -> Some (hex_decode str)
  | j                         -> None

let json_decode_exn j =
  match json_decode j with
  | Some s -> s
  | None   ->
    failwith (
      Printf.sprintf "%s is not a valid UT8-encoded JSON string"
        (Ezjsonm.to_string j)
    )

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
    List.iter ~f:(fun e ->
        Buffer.add_string buf (f e);
        if !len > 0 then Buffer.add_string buf ", ";
        decr len
      ) l;
    Buffer.add_string buf " }";
    Buffer.contents buf

open Bin_prot.Type_class

let read bin buf =
  try Some (bin.reader.read ~pos_ref:(ref 0) buf)
  with Bin_prot.Common.Read_error _ -> None

let write bin t =
  let n = bin.writer.size t in
  let buf = Bigstring.create n in
  let k = bin.writer.write buf ~pos:0 t in
  assert (n=k);
  buf

open Lwt

let lift_stream s =
  let (stream: 'a Lwt_stream.t option ref) = ref None in
  let rec get () =
    match !stream with
    | Some s -> Lwt_stream.get s
    | None   ->
      s >>= fun s ->
      stream := Some s;
      get ()
  in
  Lwt_stream.from get
