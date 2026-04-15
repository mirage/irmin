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

open Irmin.Backend.Conf

let test_conf () =
  let spec_a = Spec.v "a" in
  let spec_b = Spec.v "b" in
  let x = key ~spec:spec_a "x" Irmin.Type.int 0 in
  let _y = key ~spec:spec_a "y" Irmin.Type.int 1 in
  let conf_a = add (empty spec_a) x 1 in
  let () = Alcotest.(check int) "x" 1 (get conf_a x) in
  let () =
    Alcotest.check_raises "Wrong spec"
      (Invalid_argument "invalid config key: x") (fun () ->
        ignore (add (empty spec_b) x 1))
  in
  let keys =
    Spec.keys spec_a
    |> Seq.map (fun (K k) -> name k)
    |> List.of_seq
    |> List.sort String.compare
  in
  let () = Alcotest.(check (list string)) "Key list" [ "x"; "y" ] keys in
  ()

let test_duplicate_key_names () =
  let spec = Spec.v "test" in
  let name = "name" in
  let _ = key ~spec name Irmin.Type.char 'Z' in
  Alcotest.check_raises "Duplicate key" (Invalid_argument "duplicate key: name")
    (fun () -> ignore (key ~spec name Irmin.Type.bool false))

let test_subsequent_root_paths () =
  let spec = Spec.v "roots1" in
  let module K = struct
    let root = root spec
  end in
  let r1 = "_build/test-roots-1" in
  let r2 = "_build/test-roots-2" in
  let c1 = add (empty spec) K.root r1 in
  let c2 = add (empty spec) K.root r2 in
  Alcotest.(check string) "same string" (find_root c1 |> Option.get) r1;
  Alcotest.(check string) "same string" (find_root c2 |> Option.get) r2

let test_subsequent_root_paths_duplicate () =
  let spec = Spec.v "roots2" in
  let r1 = "_build/test-roots-duplicate-1" in
  let root_key = root spec in
  let _ = add (empty spec) root_key r1 in
  Alcotest.check_raises "Duplicate key" (Invalid_argument "duplicate key: root")
    (fun () -> ignore (root spec))

let suite =
  [
    Alcotest.test_case "conf" `Quick test_conf;
    Alcotest.test_case "duplicate key names" `Quick test_duplicate_key_names;
    Alcotest.test_case "subsequent root paths" `Quick test_subsequent_root_paths;
    Alcotest.test_case "subsequent root paths duplicate" `Quick
      test_subsequent_root_paths_duplicate;
  ]
