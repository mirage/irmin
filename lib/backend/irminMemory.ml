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

module L = Log.Make(struct let section = "MEMORY" end)

module RO (K: IrminKey.S) = struct

  type key = string

  type value = Cstruct.buffer

  type t = (key, value) Hashtbl.t

  let pretty_key k =
    K.to_string (K.of_raw k)

  let unknown k =
    fail (IrminKey.Unknown (pretty_key k))

  let create () =
    return (Hashtbl.create 8128) (*store*)

  let read t key =
    L.debugf "read %s" (pretty_key key);
    return (
      try Some (Hashtbl.find t key)
      with Not_found -> None
    )

  let read_exn t key =
    L.debugf "read_exn %s" (pretty_key key);
    try return (Hashtbl.find t key)
    with Not_found -> unknown key

  let mem t key =
    L.debugf "mem %s" (pretty_key key);
    return (Hashtbl.mem t key)

  let list t k =
    return [k]

  let contents t =
    return (Hashtbl.fold (fun k v l -> (k, v) :: l) t [])

end

module AO (K: IrminKey.S) = struct

  include RO(K)

  let add t value =
    let key = K.to_raw (K.of_bigarray value) in
    Hashtbl.add t key value;
    return key

end

module RW (K: IrminKey.S) = struct

  include RO(K)

  let update t key value =
    L.debugf "update %s" (pretty_key key);
    Hashtbl.replace t key value;
    return_unit

  let remove t key =
    L.debugf "remove %s" (pretty_key key);
    Hashtbl.remove t key;
    return_unit

end

module Simple = struct
  module K = IrminKey.SHA1
  module R = IrminReference.Simple
  include Irmin.Simple(AO(K))(RW(R))
end
