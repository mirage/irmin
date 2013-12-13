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
  type t
  val name: string
  val compare: t -> t -> int
  val equal: t -> t -> bool
  val hash: t -> int
  val pretty: t -> string
  val to_string: t -> string
  val of_json: Ezjsonm.t -> t
  val to_json: t -> Ezjsonm.t
  val sizeof: t -> int
  val get: IrminBuffer.t -> t option
  val set: IrminBuffer.t -> t -> unit
end

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module OCamlList = List
module OCamlString = String

module List (E: S) = struct

  let debug fmt = IrminLog.debug "IO-LIST" fmt

  type t = E.t list

  let name = E.name ^ "s"

  let rec compare l1 l2 = match l1, l2 with
    | []    , []     -> 0
    | h1::t1, h2::t2 ->
      begin match E.compare h1 h2 with
        | 0 -> compare t1 t2
        | i -> i
      end
    | _::_  , []     -> 1
    | []    , _::_   -> -1

  let pretty ts =
    IrminMisc.pretty_list E.pretty ts

  let to_string t =
    String.concat "" (OCamlList.rev_map E.to_string t)

  let equal l1 l2 =
    compare l1 l2 = 0

  let hash = Hashtbl.hash

  let to_json t =
    `A (OCamlList.rev (OCamlList.rev_map E.to_json t))

  let of_json = function
    | `A l -> OCamlList.rev (List.rev_map E.of_json l)
    | _    -> IrminBuffer.parse_error "List.of_json"

  let sizeof l =
    debug "sizeof";
    4 +
    List.fold_left (fun acc e ->
        acc + E.sizeof e
      ) 0 l

  let get buf =
    debug "get";
    IrminBuffer.dump ~msg:"-->" buf;
    let keys = Int32.to_int (IrminBuffer.get_uint32 buf) in
    debug "get %d" keys;
    let rec aux acc i =
      if i <= 0 then Some (OCamlList.rev acc)
      else
        match E.get buf with
        | None   -> None
        | Some t -> aux (t :: acc) (i-1) in
    if keys = 0 then Some []
    else aux [] keys

  let set buf t =
    debug "set %s" (pretty t);
    let len = Int32.of_int (List.length t) in
    let () = IrminBuffer.set_uint32 buf len in
    List.iter (E.set buf) t

end

module Option (E: S) = struct

  let debug fmt = IrminLog.debug "IO-OPTION" fmt

  let name = E.name

  type t = E.t option

  let compare o1 o2 = match o1, o2 with
    | None   , None    -> 0
    | Some _ , None    -> 1
    | None   , Some _  -> -1
    | Some e1, Some e2 -> E.compare e1 e2

  let pretty = function
    | None   -> "<none>"
    | Some e -> E.pretty e

  let to_string = function
    | None   -> ""
    | Some e -> E.to_string e

  let equal o1 o2 =
    compare o1 o2 = 0

  let hash = function
    | None   -> 0
    | Some e -> E.hash e

  let to_json = function
    | None   -> `Null
    | Some e -> E.to_json e

  let of_json = function
    | `Null -> None
    | j     -> Some (E.of_json j)

  let sizeof t =
    debug "sizeof";
    1 + match t with
      | None   -> 0
      | Some e -> E.sizeof e

  let none = 0
  let some = 1

  let get buf =
    debug "get";
    let h = IrminBuffer.get_uint8 buf in
    if h = none then Some None
    else match E.get buf with
      | None   -> None
      | Some v -> Some (Some v)

  let set buf t =
    debug "set %s" (pretty t);
    match t with
    | None   ->
      IrminBuffer.set_uint8 buf none
    | Some e ->
      IrminBuffer.set_uint8 buf some;
      E.set buf e

end

module Pair (K: S) (V: S) = struct

  let debug fmt = IrminLog.debug "IO-PAIR" fmt

  type t = K.t * V.t

  let name = K.name ^ "-" ^ V.name

  let compare (k1,v1) (k2,v2) =
    match K.compare k1 k2 with
    | 0 -> V.compare v1 v2
    | i -> i

  let pretty (key, value) =
    Printf.sprintf "%s:%s" (K.pretty key) (V.pretty value)

  let to_string (key, value) =
    K.to_string key ^ V.to_string value

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash (key, value) =
    Hashtbl.hash (K.hash key, V.hash value)

  let to_json (key, value) =
    `O [ (K.name, K.to_json key);
         (V.name, V.to_json value)]

  let of_json = function
    | `O l ->
      let key =
        try OCamlList.assoc K.name l
        with Not_found -> IrminBuffer.parse_error "Pair.of_json: missing %s" K.name
      in
      let value =
        try OCamlList.assoc V.name l
        with Not_found -> IrminBuffer.parse_error "Pair.of_json: missing %s" V.name
      in
      (K.of_json key, V.of_json value)
    | _ -> IrminBuffer.parse_error "Pair.of_json: not an object"

  let sizeof (key, value) =
    let k = K.sizeof key in
    let v = V.sizeof value in
    debug "sizeof k:%d v:%d" k v;
    k+v

  let get buf =
    debug "get";
    match K.get buf, V.get buf with
    | Some k, Some v -> Some (k, v)
    | _              -> None

  let set buf (key, value as t) =
    debug "set %s" (pretty t);
    IrminBuffer.dump buf;
    K.set buf key;
    V.set buf value

end

module String = struct

  let debug fmt =
    IrminLog.debug "IO-string" fmt

  type t = string

  let name = "string"

  let to_string t = t

  let of_string t = t

  let compare t1 t2 =
    String.compare t1 t2

  let pretty t =
    Printf.sprintf "%S" t

  let dump t =
    t

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash t =
    Hashtbl.hash t

  let to_json t =
    Ezjsonm.string t

  let of_json j =
    Ezjsonm.get_string j

  let sizeof s =
    4 + String.length s

  let get buf =
    debug "get";
    IrminBuffer.dump ~msg:"-->" buf;
    try
      let len = IrminBuffer.get_uint32 buf in
      debug "|-- get (%ld)" len;
      let t = IrminBuffer.get_string buf (Int32.to_int len) in
      debug "<-- get %s" t;
      Some t
    with _ ->
      None

  let set buf t =
    debug "set %s" (pretty t);
    let len = String.length t in
    IrminBuffer.set_uint32 buf (Int32.of_int len);
    IrminBuffer.set_string buf t

end

module PrivateString = String
