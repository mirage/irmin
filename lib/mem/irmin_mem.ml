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

let (>>=) = Lwt.bind

module Log = Log.Make(struct let section = "MEM" end)

module RO (K: Irmin.Hum.S) (V: Tc.S0) = struct

  module KHashtbl = Hashtbl.Make(K)

  let err_not_found n k =
    let str = Printf.sprintf "Irmin_mem.%s: %s not found" n (K.to_hum k) in
    Lwt.fail (Invalid_argument str)

  type key = K.t

  type value = V.t

  type t = { t: value KHashtbl.t }

  let table = KHashtbl.create 23

  let create _config =
    Lwt.return { t = table }

  let read { t; _ } key =
    Log.debug "read";
    try Lwt.return (Some (KHashtbl.find t key))
    with Not_found -> Lwt.return_none

  let read_exn { t; _ } key =
    Log.debug "read";
    try Lwt.return (KHashtbl.find t key)
    with Not_found -> err_not_found "read" key

  let mem { t; _ } key =
    Log.debug "mem";
    Lwt.return (KHashtbl.mem t key)

  let iter { t; _ } fn =
    Log.debug "iter";
    let todo = ref [] in
    KHashtbl.iter (fun k v -> todo := (fn k (Lwt.return v)) :: !todo) t;
    Lwt_list.iter_p (fun x -> x) !todo

end

module AO (K: Irmin.Hash.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let add { t; _ } value =
    Log.debug "add";
    let key = K.digest (Tc.write_cstruct (module V) value) in
    KHashtbl.replace t key value;
    Lwt.return key

end

module Link (K: Irmin.Hash.S) = struct

  include RO(K)(K)

  let add { t; _ } index key =
    Log.debug "add link";
    KHashtbl.replace t index key;
    Lwt.return_unit

end

module RW (K: Irmin.Hum.S) (V: Tc.S0) = struct

  module RO = RO(K)(V)
  module W = Irmin.Private.Watch.Make(K)(V)
  module L = Irmin.Private.Lock.Make(K)

  type t = { t: RO.t; w: W.t; lock: L.t }
  type key = RO.key
  type value = RO.value
  type watch = W.watch

  let watches = W.create ()
  let lock = L.create ()

  let create config =
    RO.create config >>= fun t ->
    Lwt.return { t; w = watches; lock }

  let read t = RO.read t.t
  let read_exn t = RO.read_exn t.t
  let mem t = RO.mem t.t
  let iter t = RO.iter t.t
  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let update t key value =
    Log.debug "update";
    L.with_lock t.lock key (fun () ->
        RO.KHashtbl.replace t.t.RO.t key value;
        Lwt.return_unit
      ) >>= fun () ->
    W.notify t.w key (Some value)

  let remove t key =
    Log.debug "remove";
    L.with_lock t.lock key (fun () ->
        RO.KHashtbl.remove t.t.RO.t key;
        Lwt.return_unit
      ) >>= fun () ->
    W.notify t.w key None

  let compare_and_set t key ~test ~set =
    Log.debug "compare_and_set";
    L.with_lock t.lock key (fun () ->
        read t key >>= fun v ->
        if Tc.O1.equal V.equal test v then (
          let () = match set with
            | None   -> RO.KHashtbl.remove t.t.RO.t key
            | Some v -> RO.KHashtbl.replace t.t.RO.t key v
          in
          Lwt.return true
        ) else
          Lwt.return false
      ) >>= fun updated ->
    (if updated then W.notify t.w key set else Lwt.return_unit) >>= fun () ->
    Lwt.return updated

end

let config () = Irmin.Private.Conf.empty

module Make (C: Irmin.Contents.S) (R: Irmin.Ref.S) (H: Irmin.Hash.S) =
  Irmin.Make(AO)(RW)(C)(R)(H)
