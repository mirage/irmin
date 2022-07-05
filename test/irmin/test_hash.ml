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

open Irmin

let test_short_hash () =
  let h = Hash.BLAKE2B.hash (fun f -> f "") in
  let () =
    Hash.BLAKE2B.short_hash h
    |> Alcotest.(check int)
         "Specialised short hash"
         (Int64.to_int 241225442164632184L)
  in
  let () =
    Type.(unstage (short_hash Hash.BLAKE2B.t)) ~seed:0 h
    |> Alcotest.(check int) "Generic seeded short hash" 674923654
  in
  let () =
    Type.(unstage (short_hash Hash.BLAKE2B.t)) ?seed:None h
    |> Alcotest.(check int) "Generic unseeded short hash" 674923654
  in
  ()

let suite = [ Alcotest_lwt.test_case_sync "short_hash" `Quick test_short_hash ]
