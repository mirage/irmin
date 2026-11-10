(*
 * Copyright (c) 2026 Tarides <contact@tarides.com>
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

module W =
  Irmin.Backend.Watch.Make
    (struct
      type t = string

      let t = Irmin.Type.string
    end)
    (struct
      type t = string

      let t = Irmin.Type.string
    end)

(* [check_fails name f] checks that [f ()] raises [Invalid_argument]. *)
let check_fails name f =
  match f () with
  | _ -> Alcotest.failf "%s should fail without a watch switch" name
  | exception Invalid_argument _ -> ()

(* Adding a watch, or listening to a directory, fails at once while the watch
   switch is not set, rather than dropping the notifications. *)
let test_no_switch () =
  let w = W.v () in
  check_fails "watch_key" (fun () -> W.watch_key w "k" ignore);
  check_fails "watch" (fun () -> W.watch w (fun _ _ -> ()));
  check_fails "listen_dir" (fun () ->
      W.listen_dir w "dir" ~key:Option.some ~value:(fun _ -> None))

let () =
  Eio_main.run @@ fun _ ->
  Alcotest.run "irmin-watch-no-switch"
    [ ("watch", [ ("no switch", `Quick, test_no_switch) ]) ]
