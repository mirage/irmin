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

open Core_kernel.Std

module type S = sig
  type t
  include Identifiable.S with type t := t
  val name: string
  val pretty: t -> string
  val of_json: Ezjsonm.t -> t
  val to_json: t -> Ezjsonm.t
  val sizeof: t -> int
  val get: Mstruct.t -> t option
  val set: Mstruct.t -> t -> unit
end

module List (E: S) = struct

  module L = Log.Make(struct let section = "IO-LIST" end)

  module M = struct
    type t = E.t list
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "List"
    let name = E.name ^ "s"
  end
  include M
  include Identifiable.Make (M)

  let pretty ts =
    IrminMisc.pretty_list E.pretty ts

  let to_json t =
    `A (List.rev (List.rev_map ~f:E.to_json t))

  let of_json = function
    | `A l -> List.rev (List.rev_map ~f:E.of_json l)
    | _    -> Mstruct.parse_error "List.of_json"

  let sizeof l =
    L.debug (lazy "sizeof");
    4 +
    List.fold_left ~f:(fun acc e ->
        acc + E.sizeof e
      ) ~init:0 l

  let get buf =
    L.debug (lazy "get");
    Mstruct.dump ~msg:"-->" ~level:Log.DEBUG buf;
    let keys = Int32.to_int_exn (Mstruct.get_be_uint32 buf) in
    L.debugf "get %d" keys;
    let rec aux acc i =
      if Int.(i <= 0) then Some (List.rev acc)
      else
        match E.get buf with
        | None   -> None
        | Some t -> aux (t :: acc) (i-1) in
    if Int.(keys = 0) then Some []
    else aux [] keys

  let set buf t =
    L.debugf "set %s" (pretty t);
    let len = Int32.of_int_exn (List.length t) in
    Mstruct.set_be_uint32 buf len;
    List.iter ~f:(E.set buf) t

end

module Option (E: S) = struct

  module L = Log.Make(struct let section = "IO-OPTION" end)

  module M = struct
    type t = E.t option
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Option"
    let name = E.name
  end
  include M
  include Identifiable.Make (M)

  let pretty = function
    | None   -> "<none>"
    | Some e -> E.pretty e

  let to_json = function
    | None   -> `Null
    | Some e -> E.to_json e

  let of_json = function
    | `Null -> None
    | j     -> Some (E.of_json j)

  let sizeof t =
    L.debug (lazy "sizeof");
    1 + match t with
      | None   -> 0
      | Some e -> E.sizeof e

  let none = 0
  let some = 1

  let get buf =
    L.debug (lazy "get");
    let h = Mstruct.get_uint8 buf in
    if Int.(h = none) then Some None
    else match E.get buf with
      | None   -> None
      | Some v -> Some (Some v)

  let set buf t =
    L.debugf "set %s" (pretty t);
    match t with
    | None   ->
      Mstruct.set_uint8 buf none
    | Some e ->
      Mstruct.set_uint8 buf some;
      E.set buf e

end

module Pair (K: S) (V: S) = struct

  module List = Core_kernel.Core_list

  module L = Log.Make(struct let section = "IO-PAIR" end)

  module M = struct
    type t = K.t * V.t
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Pair"
    let name = K.name ^ "-" ^ V.name
  end
  include M
  include Identifiable.Make (M)

  let pretty (key, value) =
    Printf.sprintf "%s:%s" (K.pretty key) (V.pretty value)

  let to_json (key, value) =
    `O [ (K.name, K.to_json key);
         (V.name, V.to_json value)]

  let of_json = function
    | `O l ->
      let key =
        try List.Assoc.find_exn l K.name
        with Not_found -> Mstruct.parse_error "Pair.of_json: missing %s" K.name
      in
      let value =
        try List.Assoc.find_exn l V.name
        with Not_found -> Mstruct.parse_error "Pair.of_json: missing %s" V.name
      in
      (K.of_json key, V.of_json value)
    | _ -> Mstruct.parse_error "Pair.of_json: not an object"

  let sizeof (key, value) =
    let k = K.sizeof key in
    let v = V.sizeof value in
    L.debugf "sizeof k:%d v:%d" k v;
    k+v

  let get buf =
    L.debugf "get";
    match K.get buf, V.get buf with
    | Some k, Some v -> Some (k, v)
    | _              -> None

  let set buf (key, value as t) =
    L.debugf "set %s" (pretty t);
    Mstruct.dump buf;
    K.set buf key;
    V.set buf value

end

module String = struct

  module L = Log.Make(struct let section = "IO-string" end)

  include String

  let name = "string"

  let pretty t =
    Printf.sprintf "%S" t

  let to_json t =
    Ezjsonm.string t

  let of_json j =
    Ezjsonm.get_string j

  let sizeof s =
    4 + String.length s

  let get buf =
    L.debug (lazy "get");
    Mstruct.dump ~msg:"-->" ~level:Log.DEBUG buf;
    try
      let len = Mstruct.get_be_uint32 buf in
      L.debugf "|-- get (%ld)" len;
      let t = Mstruct.get_string buf (Int32.to_int_exn len) in
      L.debugf "<-- get %s" t;
      Some t
    with _ ->
      None

  let set buf t =
    L.debugf "set %s" (pretty t);
    let len = String.length t in
    Mstruct.set_be_uint32 buf (Int32.of_int_exn len);
    Mstruct.set_string buf t

end
