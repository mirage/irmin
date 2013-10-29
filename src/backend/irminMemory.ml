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

module A (K: IrminKey.S) = struct

  open Lwt

  type key = K.t

  type value = IrminBuffer.ba

  type t = (key, value) Hashtbl.t

  let create () =
    return (Hashtbl.create 4096)

  let init t =
    return_unit

  let add t value =
    let key = K.of_buffer (IrminBuffer.of_ba value) in
    Hashtbl.add t key value;
    return key

  let read t key =
    Printf.printf "Reading %s\n%!" (K.pretty key);
    return (
      try Some (Hashtbl.find t key)
      with Not_found -> None
    )

  let read_exn t key =
    Printf.printf "Reading %s\n%!" (K.pretty key);
    try return (Hashtbl.find t key)
    with Not_found -> fail (K.Unknown key)

  let mem t key =
    return (Hashtbl.mem t key)

  let list t k =
    return [k]

end

module M (K: IrminKey.S) = struct

  open Lwt

  type key = string

  type value = K.t

  type t = (key, value) Hashtbl.t

  exception Unknown of string

  let create () =
    return (Hashtbl.create 64)

  let init t =
    return_unit

  let update t tag key =
    Printf.printf "Update %s to %s\n%!" tag (K.pretty key);
    Hashtbl.replace t tag key;
    return_unit

  let remove t tag =
    Hashtbl.remove t tag;
    return_unit

  let read t tag =
    Printf.printf "Reading %s\n%!" tag;
    return (
      try Some (Hashtbl.find t tag)
      with Not_found -> None
    )

  let read_exn t tag =
    Printf.printf "Reading %s\n%!" tag;
    try return (Hashtbl.find t tag)
    with Not_found -> fail (Unknown tag)

  let mem t tag =
    return (Hashtbl.mem t tag)

  let list t _ =
    let elts = Hashtbl.fold (fun t _ acc -> t :: acc) t [] in
    return elts

end

module SimpleA = A(IrminKey.SHA1)
module SimpleM = M(IrminKey.SHA1)

module Simple = Irmin.Make
    (IrminKey.SHA1)(IrminValue.Simple)(IrminTag.Simple)
    (SimpleA)(SimpleA)(SimpleA)(SimpleM)
