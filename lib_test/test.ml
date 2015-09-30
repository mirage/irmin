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

let () =
  Log.set_log_level Log.DEBUG;
  Log.color_on ();
  Printexc.record_backtrace true

module Hash = Irmin.Hash.SHA1

module Key = struct
  include Irmin.Hash.SHA1
  let pp ppf x = Format.pp_print_string ppf (to_hum x)
end

module Value = struct
  include Irmin.Contents.Cstruct
  let pp ppf x = Format.pp_print_string ppf (Cstruct.to_string x)
end

module type S = sig
  include Irmin.AO with type key = Key.t and type value = Value.t
  val create: unit -> (unit -> t) Lwt.t
end

module Mem = struct
  include Irmin_mem.AO(Key)(Value)
  let create () = create (Irmin_mem.config ()) Irmin.Task.none
end

module MemChunk = struct
  include Irmin_chunk.AO(Irmin_mem.AO)(Key)(Value)
  let small_config = Irmin_chunk.config ~min_size:44 ~size:44 ()
  let create () = create small_config Irmin.Task.none
end

let key_t: Key.t Alcotest.testable = (module Key)
let value_t: Value.t Alcotest.testable = (module Value)

let value s = Cstruct.of_string s

let run f () =
  Lwt_main.run (f ());
  flush stderr;
  flush stdout

let test_add_read (module AO: S) () =
  AO.create () >>= fun t ->
  let list () =
    let all = ref [] in
    AO.iter (t ()) (fun k _ -> all := k :: !all; Lwt.return_unit) >>= fun () ->
    Printf.eprintf "LIST: %s\n%!" (String.concat " " (List.map Key.to_hum !all));
    Lwt.return_unit
  in
  let test size =
    let name = Printf.sprintf "size %d" size in
    let v = value (String.make size 'x') in
    AO.add (t ()) v  >>= fun k ->
    list () >>= fun () ->
    AO.read (t ()) k >|= fun v' ->
    Alcotest.(check @@ option value_t) name (Some v) v'
  in
  let x = 40 in
  Lwt_list.iter_s test [
    x-1  ; x  ; x+1;
    x*2-1; x*2; x*2+1;
    x*x-1; x*x; x*x+1;
    x*x*x;
  ]

let simple =
  "simple", [
    "add/read: in-memory"       , `Quick, run @@ test_add_read (module Mem);
    "add/read: in-memory+chunks", `Quick, run @@ test_add_read (module MemChunk);
  ]

let () =
  Alcotest.run "irmin-chunk" [simple]
