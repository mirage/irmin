 (*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

module Hash = Irmin.Hash.SHA1

module Key = struct
  include Irmin.Hash.SHA1
  let pp ppf x = Format.pp_print_string ppf (to_hum x)
end

module Value = struct
  include Irmin.Contents.Cstruct
  let pp ppf x = Format.pp_print_string ppf (Cstruct.to_string x)
end

module AO = struct
  include Irmin_chunk.AO(Irmin_mem.AO)(Key)(Value)
  let create () = create (Irmin_mem.config ()) Irmin.Task.none
end

let key_t: Key.t Alcotest.testable = (module Key)
let value_t: Value.t Alcotest.testable = (module Value)

let value s = Cstruct.of_string s

let run f () =
  Lwt_main.run (f ());
  flush stderr;
  flush stdout

let test_add_read () =
  AO.create () >>= fun t ->
  let v1 = value (Bytes.make 12 'x') in
  let v2 = value (Bytes.make 89990 'y') in
  AO.add (t ()) v1 >>= fun k1 ->
  AO.add (t ()) v2 >>= fun k2 ->
  Printf.printf "yyyy\n%!";
  AO.read (t ()) k1 >>= fun v1' ->
  Printf.printf "fff\n%!";
  AO.read (t ()) k2 >>= fun v2' ->
  Printf.printf "XXX\n%!";
  Alcotest.(check @@ option value_t) "v1" (Some v1) v1';
  Alcotest.(check @@ option value_t) "v2" (Some v2) v2';
  Printf.printf "YYYY\n%!";
  Lwt.return_unit

let simple =
  "simple", [
    "add_read", `Quick, run test_add_read;
  ]

let () =
  Alcotest.run "irmin-chunks" [simple]
