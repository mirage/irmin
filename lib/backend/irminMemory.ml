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
open Core_kernel.Std

module Log = Log.Make(struct let section = "MEMORY" end)

module RO (K: IrminKey.S) (V: Identifiable.S) = struct

  module W = IrminWatch.Make(K)(V)

  type key = K.t

  type value = V.t

  type t = {
    t: value K.Table.t;
    w: W.t;
  }

  let unknown k =
    fail (IrminKey.Unknown (K.to_string k))

  let create () =
    return {
      t = K.Table.create ();
      w = W.create ();
    }

  let read { t } key =
    Log.debugf "read %s" (K.to_string key);
    return (Hashtbl.find t key)

  let read_exn { t } key =
    Log.debugf "read_exn %s" (K.to_string key);
    match Hashtbl.find t key with
    | Some d -> return d
    | None   -> unknown key

  let mem { t } key =
    Log.debugf "mem %s" (K.to_string key);
    return (Hashtbl.mem t key)

  let list { t } k =
    Log.debugf "list %s" (IrminMisc.pretty_list K.to_string k);
    return k

  let dump { t } =
    Log.debugf "dump";
    return (Hashtbl.to_alist t)

end

module AO (K: IrminKey.S) (V: Identifiable.S) = struct

  include RO(K)(V)

  let add { t } value =
    (* XXX: hook to choose the serialization format / key generator
       ? *)
    let key = K.of_bytes (IrminMisc.write V.bin_t value) in
    match Hashtbl.add t key value with
    | `Ok | `Duplicate -> return key

end

module RW (K: IrminKey.S) (V: Identifiable.S) = struct

  include RO(K)(V)

  let update t key value =
    Log.debugf "update %s" (K.to_string key);
    Hashtbl.replace t.t key value;
    W.notify t.w key (Some value);
    return_unit

  let remove t key =
    Log.debugf "remove %s" (K.to_string key);
    Hashtbl.remove t.t key;
    W.notify t.w key None;
    return_unit

  let watch t key =
    Log.debugf "watch %S" (K.to_string key);
    IrminMisc.lift_stream (
      read t key >>= fun value ->
      return (W.watch t.w key value)
    )

end

module Make (K: IrminKey.S) (C: IrminContents.S) (R: IrminReference.S) = struct

  let create () =
    let module V = IrminValue.S(K)(C) in
    let module Val = IrminValue.Make(K)(C)(AO(K)(V)) in
    let module Ref = IrminReference.Make(R)(K)(RW(R)(K)) in
    let module S = Irmin.Make(K)(C)(R)(Val)(Ref) in
    (module S: Irmin.S with type Internal.key = K.t
                        and type value = C.t
                        and type Reference.key = R.t)

  let cast (module M: Irmin.S with type Internal.key = K.t
                               and type value = C.t
                               and type Reference.key = R.t) =

    (module M: Irmin.S)

end
