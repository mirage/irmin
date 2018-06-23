(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

type json = [
  | `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `O of (string * json) list
  | `A of json list
]

let json =
  let open Type in
  mu (fun ty ->
  variant "json" (fun null bool string float obj arr -> function
    | `Null -> null
    | `Bool b -> bool b
    | `String s -> string s
    | `Float f -> float f
    | `O o -> obj o
    | `A a -> arr a)
  |~ case0 "null" `Null
  |~ case1 "bool" bool (fun x -> `Bool x)
  |~ case1 "string" string (fun x -> `String x)
  |~ case1 "float" float (fun x -> `Float x)
  |~ case1 "object" (list (pair string ty)) (fun obj -> `O obj)
  |~ case1 "array" (list ty) (fun arr -> `A arr)
  |> sealv)

let assoc k l =
  try Some (List.assoc k l) with Not_found -> None

let merge_objects j a b =
  let equal = Type.equal json in
  try
    let v = List.fold_right (fun (k, v) acc ->
      match assoc k a, assoc k b with
      | Some x, Some y when not (equal x y) -> failwith "Unable to merge JSON objects"
      | Some x, _ -> (k, x) :: acc
      | None, Some x -> (k, x) :: acc
      | None, None -> (k, v) :: acc
    ) j []
    in Merge.ok (`O v)
  with
    Failure msg -> Merge.conflict "%s" msg

let merge2 a b =
  match a, b with
  | `Null, a | a, `Null -> Merge.ok a
  | `Float a, `Float b when a <> b -> Merge.conflict "Conflicting float values"
  | `String a, `String b when not (String.equal a b) -> Merge.conflict "Conflicting string values"
  | `Bool a, `Bool b when a <> b -> Merge.conflict "Conflicting bool values"
  | `O a, `O b -> merge_objects [] a b
  | `A a, `A b ->
      if List.for_all2 (Type.equal json) a b then
        Merge.ok (`A a)
      else
        Merge.conflict "Unable to merge JSON arrays"
  | `Float _, `Float _ | `String _, `String _ | `Bool _, `Bool _ -> Merge.ok a
  | _, _ -> Merge.conflict "Conflicting JSON values"

let merge3 a b c =
  let open Merge.Infix in
  match a, b, c with
  | `Null, a, b -> merge2 a b
  | `O a, `O b, `O c -> merge_objects a b c
  | a, b, c -> (merge2 a b >>=* fun b -> merge2 b c)

let merge_json' ~old a b =
  let equal = Type.equal json in
  let open Merge.Infix in
  old () >>=* function
    | Some old ->
        if equal old a then Merge.ok b
        else if equal old b then Merge.ok a
        else merge3 old a b
    | None ->
        merge2 a b

let merge_json = Merge.(v json merge_json')

module String = struct
  type t = string
  let t = Type.string
  let merge = Merge.idempotent Type.(option string)
  let pp = Fmt.string
  let of_string s = Ok s
end

module Cstruct = struct
  type t = Cstruct.t
  let t = Type.cstruct
  let merge = Merge.idempotent Type.(option t)
  let pp ppf b = Fmt.string ppf (Cstruct.to_string b)
  let of_string s = Ok (Cstruct.of_string s)
end

module Json = struct
  type t = (string * json) list

  let t = Type.(list (pair string json))

  let merge = Merge.(option (alist Type.string json (fun _key -> Merge.option merge_json)))
  let lexeme e x = ignore (Jsonm.encode e (`Lexeme x))

  let rec encode_json e = function
    | `Null -> lexeme e `Null
    | `Bool b -> lexeme e (`Bool b)
    | `String s -> lexeme e (`String s)
    | `Float f -> lexeme e (`Float f)
    | `A a ->
        lexeme e `As;
        List.iter (encode_json e) a;
        lexeme e `Ae;
    | `O o ->
        lexeme e `Os;
        List.iter (fun (k, v) ->
          lexeme e (`Name k);
          encode_json e v
        ) o;
        lexeme e `Oe

  let pp fmt x =
    let buffer = Buffer.create 32 in
    let encoder = Jsonm.encoder (`Buffer buffer) in
    encode_json encoder (`O x);
    ignore @@ Jsonm.encode encoder `End;
    let s = Buffer.contents buffer in
    Fmt.pf fmt "%s" s

  let decode_json d =
    let decode d = match Jsonm.decode d with
      | `Lexeme l -> l
      | `Error e -> failwith (Fmt.strf "%a" Jsonm.pp_error e)
      | _ -> failwith "invalid JSON encoding"
    in
    let rec unwrap v d = match v with
      | `Os -> obj [] d
      | `As -> arr [] d
      | `Null | `Bool _ | `String _ | `Float _ as v -> v
      | _ -> failwith "invalid JSON value"
    and arr vs d =
      match decode d with
      | `Ae -> `A (List.rev vs)
      | v ->
          let v = unwrap v d in
          arr (v::vs) d
    and obj ms d =
      match decode d with
      | `Oe -> `O (List.rev ms)
      | `Name k ->
          let v = unwrap (decode d) d in
          obj ((k, v) :: ms) d
      | _ -> failwith "invalid JSON object"
    in
    try
      Ok (unwrap (decode d) d)
    with
      | Failure msg -> Error (`Msg msg)

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    match decode_json decoder with
    | Ok (`O obj) -> Ok obj
    | Ok _ -> Error (`Msg "Irmin JSON values must be objects")
    | Error _ as err -> err
end

module Json_value = struct
  type t = json
  let t = json
  let merge = Merge.(option merge_json)

  let pp fmt x =
    let buffer = Buffer.create 32 in
    let encoder = Jsonm.encoder (`Buffer buffer) in
    Json.encode_json encoder x;
    ignore @@ Jsonm.encode encoder `End;
    let s = Buffer.contents buffer in
    Fmt.pf fmt "%s" s

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    match Json.decode_json decoder with
    | Ok obj -> Ok obj
    | Error _ as err -> err
end

module Json_tree(P: S.PATH)(M: S.METADATA) = struct
  include Json_value

  module type STORE = S.STORE with
    type step = P.step
    and type key = P.t
    and type contents = json
    and type metadata = M.t

  let set_tree (type a) (type n) (module S: STORE with type t = a and type node = n)
               (tree: S.tree) key (j : json) : S.tree Lwt.t =
    match j with
    | `O d ->
        Lwt_list.fold_right_s (fun (k, v) tree ->
          match S.Key.step_of_string k with
          | Ok k ->
            let k = S.Key.rcons key k in
            S.Tree.add tree k v
          | Error _ -> Lwt.return tree) d tree
    | v -> S.Tree.add tree key v

  let get_tree (type n) (module S: STORE with type node = n) (tree: S.tree) key =
    let mk_key = Fmt.to_to_string S.Key.pp_step in
    let rec aux key =
      S.Tree.list tree key >>= Lwt_list.map_s (fun (step, kind) ->
        match kind with
        | `Contents ->  S.Tree.get tree (S.Key.rcons key step) >|= fun v -> mk_key step, v
        | `Node -> aux (S.Key.rcons key step) >|= fun v -> mk_key step, v
      ) >|= fun x -> `O x
    in
    aux key

  let set (type a) (module S: STORE with type t = a) (t: a) key j =
    S.with_tree t key (fun _ ->
      let tree = S.Tree.empty in
      set_tree (module S) tree S.Key.empty j >>= Lwt.return_some)

  let get (type a) (module S: STORE with type t = a) t key =
    S.get_tree t key >>= fun tree ->
    get_tree (module S) tree S.Key.empty
end

module Store
    (S: sig
       include S.AO
       module Key: S.HASH with type t = key
       module Val: S.CONTENTS with type t = value
     end) =
struct
  include S

  let read_opt t = function
    | None   -> Lwt.return_none
    | Some k -> find t k

  let add_opt t = function
    | None -> Lwt.return_none
    | Some v -> add t v >>= fun k -> Lwt.return (Some k)

  let merge t =
    Merge.like_lwt Type.(option Key.t) Val.merge (read_opt t) (add_opt t)

end
