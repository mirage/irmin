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

module Json = struct
  type t = (string * json) list

  let t =
    Type.like json
    (function
      | `O obj -> obj
      | _ -> raise (Invalid_argument "Irmin value must be a JSON object"))
    (fun x -> `O x)

  let merge_object a b =
    List.fold_right (fun (k, v) acc ->
      match acc with
      | None -> None
      | Some dst ->
          (match List.assoc_opt k b with
          | Some x when v <> x -> None
          | Some _ -> acc
          | _ -> Some ((k, v) :: dst))
    ) a (Some b)

  let merge' a b =
    match merge_object a b with
      | Some obj -> Merge.ok obj
      | None -> Merge.conflict "Unable to merge JSON objects"

  let merge ~old t1 t2 =
    let open Merge.Infix in
    old () >>=* function
      | Some j ->
          if j = t1 then
            merge' t1 t2
          else if j = t2 then
            merge' t2 t1
          else
            merge' j t1 >>=* fun t1 ->
            merge' t1 t2
      | None -> merge' t1 t2

  let merge = Merge.(option (v t merge))

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
      | _ -> failwith "invalid json object"
    in
    try
      Ok (unwrap (decode d) d)
    with
      | Failure msg -> Error (`Msg msg)

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    match decode_json decoder with
    | Ok (`O obj) -> Ok obj
    | Ok _ -> Error (`Msg "Irmin value must be a JSON object")
    | Error _ as err -> err
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
