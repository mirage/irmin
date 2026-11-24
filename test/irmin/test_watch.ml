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

exception Stop

type Eio.Exn.Backend.t += Unreadable

(* [with_watch_switch f] runs [f] with a switch for the notification workers,
   which run forever, and cancels them afterwards. The watch switch cannot be
   unset: a later test that needs workers must set its own. *)
let with_watch_switch f =
  try
    Eio.Switch.run @@ fun sw ->
    Irmin.Backend.Watch.set_watch_switch sw;
    f ();
    raise Stop
  with Stop -> ()

(* A value that cannot be read is neither notified as removed, nor stops the
   notifications of the other keys. *)
let test_unreadable_value () =
  with_watch_switch @@ fun () ->
  let on_change = ref (fun (_ : string) -> ()) in
  Irmin.Backend.Watch.set_listen_dir_hook (fun _ _ f ->
      on_change := f;
      fun () -> ());
  Fun.protect ~finally:(fun () ->
      Irmin.Backend.Watch.(set_listen_dir_hook none))
  @@ fun () ->
  let w = W.v () in
  let event, set_event = Eio.Promise.create () in
  let _ : W.watch =
    W.watch w
      ~init:[ ("unreadable", "v") ]
      (fun key diff -> Eio.Promise.resolve set_event (key, diff))
  in
  let value = function
    | "unreadable" -> raise (Eio.Fs.err (Permission_denied Unreadable))
    | _ -> Some "v"
  in
  let stop = W.listen_dir w "dir" ~key:Option.some ~value in
  !on_change "unreadable";
  !on_change "added";
  let key, diff = Eio.Promise.await event in
  Alcotest.(check string) "first notification" "added" key;
  Alcotest.(check bool) "added" true (diff = `Added "v");
  stop ()

let suite =
  [ Alcotest.test_case "unreadable value" `Quick test_unreadable_value ]
