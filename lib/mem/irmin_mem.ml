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

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct

  let err_not_found n k =
    let str = Printf.sprintf "Irmin_mem.%s: %s not found" n (K.to_hum k) in
    Lwt.fail (Invalid_argument str)

  module W = Irmin.Private.Watch.Make(K)(V)
  module L = struct

    type t = {
      global: Lwt_mutex.t;
      locks : (K.t, Lwt_mutex.t) Hashtbl.t;
    }

    let create () = {
      global = Lwt_mutex.create ();
      locks  = Hashtbl.create 1024;
    }

    let lock t key () =
      let lock =
        try Hashtbl.find t.locks key
        with Not_found ->
          let lock = Lwt_mutex.create () in
          Hashtbl.add t.locks key lock;
          lock
      in
      Lwt.return lock

    let unlock t key () =
      let () =
        if Hashtbl.mem t.locks key then
          let lock = Hashtbl.find t.locks key in
          if Lwt_mutex.is_empty lock then Hashtbl.remove t.locks key
      in
      Lwt.return_unit

    let with_lock t k fn =
      Lwt_mutex.with_lock t.global (lock t k) >>= fun lock ->
      Lwt_mutex.with_lock lock fn >>= fun r ->
      Lwt_mutex.with_lock t.global (unlock t k) >>= fun () ->
      Lwt.return r

  end

  type key = K.t

  type value = V.t

  type t = {
    t: (K.t, value) Hashtbl.t;
    w: W.t;
    task: Irmin.task;
    config: Irmin.config;
    lock: L.t;
  }

  let task t = t.task
  let table = Hashtbl.create 23
  let watches = W.create ()
  let lock = L.create ()

  let create config task =
    return (fun a -> { t = table; w = watches; task = task a; config; lock })

  let read { t; _ } key =
    Log.debug "read";
    try return (Some (Hashtbl.find t key))
    with Not_found -> Lwt.return_none

  let read_exn { t; _ } key =
    Log.debug "read";
    try return (Hashtbl.find t key)
    with Not_found -> err_not_found "read" key

  let mem { t; _ } key =
    Log.debug "mem";
    return (Hashtbl.mem t key)

  let iter { t; _ } fn =
    Log.debug "iter";
    let todo = ref [] in
    Hashtbl.iter (fun k _ -> todo := (fn k) :: !todo) t;
    Lwt_list.iter_p (fun x -> x) !todo

end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let add { t; _ } value =
    Log.debug "add";
    (* XXX: hook to choose the serialization format / key generator
       ? *)
    let key = K.digest (Tc.write_cstruct (module V) value) in
    Hashtbl.replace t key value;
    return key

end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let update t key value =
    Log.debug "update";
    L.with_lock t.lock key (fun () ->
        Hashtbl.replace t.t key value;
        Lwt.return_unit
      ) >>= fun () ->
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    Log.debug "remove";
    L.with_lock t.lock key (fun () ->
        Hashtbl.remove t.t key;
        Lwt.return_unit
      ) >>= fun () ->
    W.notify t.w key None;
    return_unit

  let compare_and_set t key ~test ~set =
    Log.debug "compare_and_set";
    L.with_lock t.lock key (fun () ->
        read t key >>= fun v ->
        if Tc.O1.equal V.equal test v then (
          let () = match set with
            | None   -> Hashtbl.remove t.t key
            | Some v -> Hashtbl.replace t.t key v
          in
          Lwt.return true
        ) else
          Lwt.return false
      ) >>= fun updated ->
    if updated then W.notify t.w key set;
    Lwt.return updated


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
