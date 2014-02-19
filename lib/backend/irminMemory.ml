(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module L = Log.Make(struct let section = "MEMORY" end)

module RO (K: IrminKey.S) = struct

  (* That's not ideal to put that here, but we want to be W.t be
     part of the state. *)
  module W = IrminWatch.Make(K)(Bigstring)

  type key = string

  type value = Cstruct.buffer

  type t = {
    t: (key, value) Hashtbl.t;
    w: W.t;
  }

  let pretty_key k =
    K.to_string (K.of_raw k)

  let unknown k =
    fail (IrminKey.Unknown (pretty_key k))

  let create () =
    return {
      t = String.Table.create ();
      w = W.create ();
    }

  let read { t } key =
    L.debugf "read %s" (pretty_key key);
    return (Hashtbl.find t key)

  let read_exn { t } key =
    L.debugf "read_exn %s" (pretty_key key);
    match Hashtbl.find t key with
    | Some d -> return d
    | None   -> unknown key

  let mem { t } key =
    L.debugf "mem %s" (pretty_key key);
    return (Hashtbl.mem t key)

  let list { t } k =
    return [k]

  let contents { t } =
    return (Hashtbl.to_alist t)

end

module AO (K: IrminKey.S) = struct

  include RO(K)

  let add { t } value =
    let key = K.to_raw (K.of_bigarray value) in
    match Hashtbl.add t key value with
    | `Ok | `Duplicate -> return key

end

module RW (K: IrminKey.S) = struct

  include RO(K)

  let update t key value =
    L.debugf "update %s" (pretty_key key);
    Hashtbl.replace t.t key value;
    W.notify t.w (K.of_raw key) (Some value);
    return_unit

  let remove t key =
    L.debugf "remove %s" (pretty_key key);
    Hashtbl.remove t.t key;
    W.notify t.w (K.of_raw key) None;
    return_unit

  let watch t key =
    L.debugf "watch %S" (pretty_key key);
    IrminMisc.lift_stream (
      read t key >>= fun value ->
      return (W.watch t.w (K.of_raw key) value)
    )

end

module String = struct
  module K = IrminKey.SHA1
  module R = IrminReference.String
  include Irmin.String(AO(K))(RW(R))
end

module JSON = struct
  module K = IrminKey.SHA1
  module R = IrminReference.String
  include Irmin.JSON(AO(K))(RW(R))
end

let create = function
  | `String -> (module String: Irmin.S)
  | `JSON   -> (module JSON: Irmin.S)
