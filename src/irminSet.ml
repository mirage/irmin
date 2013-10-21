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

module type S = sig
  include Set.S
  val of_list: elt list -> t
  val to_list: t -> elt list
  include IrminBase.S with type t := t
end

module Make (E: IrminBase.S) = struct

  module Set = Set.Make(E)

  include Set

  let debug fmt = IrminLog.debug "SET" fmt

  module L = IrminBase.List(E)

  let name = L.name

  let of_list l =
    List.fold_left (fun acc elt -> Set.add elt acc) Set.empty l

  let to_list s =
    Set.elements s

  let hash = Hashtbl.hash

  let compare t1 t2 =
    L.compare (to_list t1) (to_list t2)

  let equal t1 t2 =
    compare t1 t2 = 0

  let pretty s =
    if Set.is_empty s then "{}"
    else
      "{ "^ String.concat ", " (List.map E.pretty (to_list s)) ^ " }"

  let dump s =
    String.concat ":" (List.map E.dump (to_list s))

  let to_json t =
    L.to_json (to_list t)

  let of_json j =
    of_list (L.of_json j)

  let sizeof t =
    debug "sizeof";
    L.sizeof (to_list t)

  let get buf =
    let l = L.get buf in
    of_list l

  let set buf t =
    L.set buf (to_list t)

end
