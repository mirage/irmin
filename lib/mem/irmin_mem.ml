(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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
module IB = Irmin.Backend

module Log = Log.Make(struct let section = "MEMORY" end)

let hashtbl_to_alist t =
  let l = ref [] in
  Hashtbl.iter (fun k v -> l := (k, v) :: !l) t;
  !l

let lwt_stream_lift s =
  let (stream: 'a Lwt_stream.t option ref) = ref None in
  let rec get () =
    match !stream with
    | Some s -> Lwt_stream.get s
    | None   ->
      s >>= fun s ->
      stream := Some s;
      get ()
  in
  Lwt_stream.from get

module RO (K: Tc.S0) (V: Tc.S0) = struct

  module W = IB.Watch.Make(K)(V)

  type key = K.t

  type value = V.t

  type t = {
    t: (K.t, value) Hashtbl.t;
    w: W.t;
    task: Irmin.task;
    config: Irmin.config;
  }

  let task t = t.task
  let config t = t.config
  let table = Hashtbl.create 23
  let watches = W.create ()

  let create config task =
    let t = {
      t = table;
      w = watches;
      task; config
    } in
    return t

  let read { t; _ } key =
    try return (Some (Hashtbl.find t key))
    with Not_found -> return_none

  let read_exn { t; _ } key =
    try return (Hashtbl.find t key)
    with Not_found -> fail Not_found

  let mem { t; _ } key =
    return (Hashtbl.mem t key)

  let list _ k =
    return k

  let dump { t; _ } =
    return (hashtbl_to_alist t)

end

module AO (K: IB.Hash.S) (V: Tc.S0) = struct

  include RO(K)(V)

  let add { t; _ } value =
    (* XXX: hook to choose the serialization format / key generator
       ? *)
    let key = K.digest (Tc.write_cstruct (module V) value) in
    Hashtbl.add t key value;
    return key

end

module RW (K: Tc.S0) (V: Tc.S0) = struct

  include RO(K)(V)

  let update t key value =
    Hashtbl.replace t.t key value;
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    Hashtbl.remove t.t key;
    W.notify t.w key None;
    return_unit

  let watch t key =
    lwt_stream_lift (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

end

module Make
    (K: IB.Hash.S)
    (S: Tc.S0)
    (C: Irmin.Contents.S)
    (T: Irmin.Tag.S)
 = struct
  include IB.Simple(K)(S)(C)(T)(AO)(RW)
  let mk = IB.Config.of_dict []
  let create = create mk
  let of_tag = of_tag mk
  let of_head = of_head mk
end
