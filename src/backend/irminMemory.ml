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

let debug fmt =
  IrminLog.debug "MEMORY" fmt

let store = Hashtbl.create 8128

let reset () =
  Hashtbl.clear store

module X (K: IrminKey.S) = struct

  type t = (string, IrminBuffer.ba) Hashtbl.t

  let pretty_key k =
    K.pretty (K.of_string k)

  let pretty_value ba =
    IrminBuffer.pretty_ba ba

  let unknown k =
    fail (K.Unknown (K.pretty (K.of_string k)))

  let create () =
    return store

  let read t key =
    debug "read %s" (pretty_key key);
    return (
      try Some (Hashtbl.find t key)
      with Not_found -> None
    )

  let read_exn t key =
    debug "read_exn %s" (pretty_key key);
    try return (Hashtbl.find t key)
    with Not_found -> unknown key

  let mem t key =
    debug "mem %s" (pretty_key key);
    return (Hashtbl.mem t key)

  let list t k =
    return [k]

  let contents t =
    return (Hashtbl.fold (fun k v l -> (k, v) :: l) t [])

end

module A (K: IrminKey.BINARY) = struct

  include X(K)

  let add t value =
    let key = K.to_string (K.of_ba value) in
    Hashtbl.add t key value;
    return key

end

module M (K: IrminKey.S): IrminStore.M_BINARY = struct

  include X(K)

  let update t key value =
    debug "update %s %s" (pretty_key key) (pretty_value value);
    Hashtbl.replace t key value;
    return_unit

  let remove t key =
    debug "remove %s" (pretty_key key);
    Hashtbl.remove t key;
    return_unit

  let list t _ =
    let l = Hashtbl.fold (fun k _ acc -> k :: acc) t [] in
    return l

end

module Simple = struct

  module K = IrminKey.SHA1
  module A = A(K)
  module M = M(K)

  include Irmin.Binary
      (K)(IrminValue.Simple)(IrminTag.Simple)
      (A)(A)(A)(M)

end
