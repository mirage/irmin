(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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

open! Irmin_pack_io.Import

let ref_domain_mgr = ref None
let set_domain_mgr t = ref_domain_mgr := Some t
let domain_mgr () = Option.get !ref_domain_mgr

module Unix = struct
  type outcome = [ `Success | `Cancelled | `Failure of string ]
  [@@deriving irmin]

  type status = [ `Running | `Success | `Cancelled | `Failure of string ]
  [@@deriving irmin]

  type t = Eio.Switch.t * outcome Eio.Promise.or_exn

  let async ~sw f =
    let run f () =
      Logs.set_level None;
      match f () with
      | () -> `Success
      | exception _ -> `Failure "Unhandled exception"
    in
    Stdlib.flush_all ();
    let gc_sw_promise, gc_sw_resolver = Eio.Promise.create ~label:"gc_sw" () in
    let promise =
      Eio.Fiber.fork_promise ~sw (fun () ->
          Eio.Switch.run @@ fun sw' ->
          Eio.Promise.resolve gc_sw_resolver sw';
          Eio.Domain_manager.run (domain_mgr ()) (run f))
    in
    let gc_sw = Eio.Promise.await gc_sw_promise in
    gc_sw, promise

  let await (_, p) : [> outcome ] =
    match Eio.Promise.await p with
    | Ok (#outcome as outcome) -> outcome
    | Error _ -> `Failure "Unhandled exception"

  let status (_, p) : [> status ] =
    match Eio.Promise.peek p with
    | Some (Ok (#outcome as outcome)) -> outcome
    | Some (Error e) -> `Failure (Printexc.to_string e)
    | None -> `Running

  exception Cancelled

  let cancel (sw, p) =
    match Eio.Promise.peek p with
    | None ->
        Eio.Switch.fail sw Cancelled;
        true
    | Some _ -> false
end
