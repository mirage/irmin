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

open Lwt.Infix
open Test_common

module Hash = Irmin.Hash.SHA1

module type S = sig
  include Irmin.LINK with type key = Hash.t and type value = Hash.t
  val v: unit -> t Lwt.t
end

let key x = Hash.digest Irmin.Type.string x
let key_t = testable Hash.t

let test (module M: S) () =
  let k1 = key "foo" in
  let k2 = key "bar" in
  let k3 = key "toto" in
  M.v () >>= fun t ->
  M.add t k1 k2 >>= fun () ->
  M.find t k1 >>= fun k2' ->
  Alcotest.(check @@ option key_t) "k1 -> k2" (Some k2) k2';
  begin Lwt.catch
    (fun () -> M.add t k1 k3 >>= fun () -> Alcotest.fail "already linked")
    (fun _e -> Lwt.return_unit)
  end >>= fun () ->
  M.add t k2 k3 >>= fun () ->
  M.find t k2 >>= fun k3' ->
  Alcotest.(check @@ option key_t) "k2 -> k3" (Some k3) k3';
  M.mem t k1 >>= fun m1 ->
  Alcotest.(check bool) "mem k1" true m1;
  M.mem t k2 >>= fun m2 ->
  Alcotest.(check bool) "mem k2" true m2;
  M.mem t k3 >>= fun m3 ->
  Alcotest.(check bool) "mem k3" false m3;
  Lwt.return_unit

let run f () = Lwt_main.run (f ())

let test msg m = "link store: " ^ msg, `Quick, run (test m)
