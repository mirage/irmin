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
open IrminCore

module Log = Log.Make(struct let section = "MEMORY" end)

module Fresh (C: sig end) = struct

module RO (K: IrminKey.S) (V: I0) = struct

  module W = IrminWatch.Make(K)(V)

  type key = K.t

  type value = V.t

  module KTable = Hashtbl.Make(K)

  type t = {
    t: value KTable.t;
    w: W.t;
  }

  let unknown k =
    fail (IrminKey.Unknown (K.pretty k))

  let table = KTable.create ()
  let watches = W.create ()

  let create () =
    let t = {
      t = table;
      w = watches;
    } in
    return t

  let clear () =
    KTable.clear table;
    W.clear watches

  let read { t } key =
    Log.debugf "read %a" force (show (module K) key);
    return (KTable.find t key)

  let read_exn { t } key =
    Log.debugf "read_exn %a" force (show (module K) key);
    match KTable.find t key with
    | Some d -> return d
    | None   -> unknown key

  let mem { t } key =
    Log.debugf "mem %a" force (show (module K) key);
    return (KTable.mem t key)

  let list { t } k =
    return k

  let dump { t } =
    Log.debugf "dump";
    return (KTable.to_alist t)

end

module AO (K: IrminKey.S) (V: I0) = struct

  include RO(K)(V)

  let add { t } value =
    (* XXX: hook to choose the serialization format / key generator
       ? *)
    let key = K.compute_from_bigstring (write_all (module V) value) in
    match KTable.add t key value with
    | `Ok | `Duplicate -> return key

end

module RW (K: IrminKey.S) (V: IrminKey.S) = struct

  include RO(K)(V)

  let update t key value =
    Log.debugf "update %a" force (show (module K) key);
    KTable.replace t.t key value;
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    Log.debugf "remove %a" force (show (module K) key);
    KTable.remove t.t key;
    W.notify t.w key None;
    return_unit

  let watch t key =
    Log.debugf "watch %a" force (show (module K) key);
    Lwt_stream.lift (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

end

module Make (K: IrminKey.S) (C: IrminContents.S) (T: IrminTag.S) = struct
  module V = IrminBlock.S(K)(C)
  module AO_K_V = AO(K)(V)
  module RW_T_K = RW(T)(K)
  module XBlock = IrminBlock.Make(K)(C)(AO_K_V)
  module XTag = IrminTag.Make(T)(K)(RW_T_K)
  include Irmin.Make(XBlock)(XTag)
  let clear () =
    AO_K_V.clear ();
    RW_T_K.clear ()
end

end

include Fresh(struct end)


module type S_MAKER =
  functor (K: IrminKey.S)      ->
  functor (C: IrminContents.S) ->
  functor (T: IrminTag.S)      ->
    sig
      include Irmin.S with type Block.key = K.t
                       and type value     = C.t
                       and type branch    = T.t
      val clear: unit -> unit
      (** Clear the store. *)
    end

module type BACKEND = sig
  module RO  : Irmin.RO_MAKER
  module AO  : Irmin.AO_MAKER
  module RW  : Irmin.RW_MAKER
  module Make: S_MAKER
end
