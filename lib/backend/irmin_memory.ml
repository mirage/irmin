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
open Irmin.Misc.OP

module Log = Log.Make(struct let section = "MEMORY" end)

module Fresh (C: sig end) = struct

module RO (K: Irmin.Sig.Uid) (V: Irmin.Tc.I0) = struct

  module W = Irmin.Watch.Make(K)(V)

  type key = K.t

  type value = V.t

  type t = {
    t: (K.t, value) Hashtbl.t;
    w: W.t;
  }

  let unknown k =
    fail (Irmin.Uid.Unknown (K.pretty k))

  let table = Hashtbl.create 23
  let watches = W.create ()

  let create () =
    let t = {
      t = table;
      w = watches;
    } in
    return t

  let clear () =
    Hashtbl.clear table;
    W.clear watches

  let read { t } key =
    Log.debugf "read %a" force (show (module K) key);
    try return (Some (Hashtbl.find t key))
    with Not_found -> return_none

  let read_exn { t } key =
    Log.debugf "read_exn %a" force (show (module K) key);
    try return (Hashtbl.find t key)
    with Not_found -> unknown key

  let mem { t } key =
    Log.debugf "mem %a" force (show (module K) key);
    return (Hashtbl.mem t key)

  let list { t } k =
    return k

  let dump { t } =
    Log.debugf "dump";
    return (Irmin.Misc.hashtbl_to_alist t)

end

module AO (K: Irmin.Sig.Uid) (V: Irmin.Tc.I0) = struct

  include RO(K)(V)

  let add { t } value =
    (* XXX: hook to choose the serialization format / key generator
       ? *)
    let key = K.compute_from_cstruct (Irmin.Tc.write_cstruct (module V) value) in
    Hashtbl.add t key value;
    return key

end

module RW (K: Irmin.Sig.Uid) (V: Irmin.Sig.Uid) = struct

  include RO(K)(V)

  let update t key value =
    Log.debugf "update %a" force (show (module K) key);
    Hashtbl.replace t.t key value;
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    Log.debugf "remove %a" force (show (module K) key);
    Hashtbl.remove t.t key;
    W.notify t.w key None;
    return_unit

  let watch t key =
    Log.debugf "watch %a" force (show (module K) key);
    Irmin.Misc.Lwt_stream.lift (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

end

module BC (K: Irmin.Sig.Uid) (C: Irmin.Sig.Contents) (T: Irmin.Sig.Tag) =
struct
  module V = Irmin.Block.S(K)(C)
  module AO_K_V = AO(K)(V)
  module RW_T_K = RW(T)(K)
  module XBlock = Irmin.Block.Make(K)(C)(AO_K_V)
  module XTag = Irmin.Tag.Make(T)(K)(RW_T_K)
  include Irmin.Store.BC(XBlock)(XTag)
  let clear () =
    AO_K_V.clear ();
    RW_T_K.clear ()
end

end

include Fresh(struct end)

module type BC_MAKER =
  functor (K: Irmin.Sig.Uid)      ->
  functor (C: Irmin.Sig.Contents) ->
  functor (T: Irmin.Sig.Tag)      ->
  sig
    include Irmin.Sig.BC with type value     = C.t
                          and type branch    = T.t
    val clear: unit -> unit
    (** Clear the store. *)
  end

module type BACKEND = sig
  module RO: Irmin.Sig.RO_MAKER
  module AO: Irmin.Sig.AO_MAKER
  module RW: Irmin.Sig.RW_MAKER
  module BC: BC_MAKER
end
