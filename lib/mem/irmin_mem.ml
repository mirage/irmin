(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt

module Log = Log.Make(struct let section = "MEM" end)

let err_not_found n =
  Lwt.fail (Invalid_argument (Printf.sprintf "Irmin_mem.%s: not found" n))

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct

  module W = Irmin.Private.Watch.Make(K)(V)

  type key = K.t

  type value = V.t

  type t = {
    t: (K.t, value) Hashtbl.t;
    w: W.t;
    task: Irmin.task;
    config: Irmin.config;
  }

  let task t = t.task
  let table = Hashtbl.create 23
  let watches = W.create ()

  let create config task =
    return (fun a -> { t = table; w = watches; task = task a; config })

  let read { t; _ } key =
    try return (Some (Hashtbl.find t key))
    with Not_found -> Lwt.return_none

  let read_exn { t; _ } key =
    try return (Hashtbl.find t key)
    with Not_found -> err_not_found "read"

  let mem { t; _ } key =
    return (Hashtbl.mem t key)

  let iter { t; _ } fn =
    let todo = ref [] in
    Hashtbl.iter (fun k _ -> todo := (fn k) :: !todo) t;
    Lwt_list.iter_p (fun x -> x) !todo

end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let add { t; _ } value =
    (* XXX: hook to choose the serialization format / key generator
       ? *)
    let key = K.digest (Tc.write_cstruct (module V) value) in
    Hashtbl.replace t key value;
    return key

end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let update t key value =
    Log.debug "update %s %s"
      (Tc.show (module K) key) (Tc.show (module V) value);
    Hashtbl.replace t.t key value;
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    Hashtbl.remove t.t key;
    W.notify t.w key None;
    return_unit

  let watch t key =
    Irmin.Private.Watch.lwt_stream_lift (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

  let watch_all t = W.watch_all t.w

end

let config () = Irmin.Private.Conf.empty

module Make (C: Irmin.Contents.S) (T: Irmin.Tag.S) (H: Irmin.Hash.S) =
  Irmin.Make(AO)(RW)(C)(T)(H)
