(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
include Contents_intf

let lexeme e x = ignore (Jsonm.encode e (`Lexeme x))

let rec encode_json e = function
  | `Null -> lexeme e `Null
  | `Bool b -> lexeme e (`Bool b)
  | `String s -> lexeme e (`String s)
  | `Float f -> lexeme e (`Float f)
  | `A a ->
      lexeme e `As;
      List.iter (encode_json e) a;
      lexeme e `Ae
  | `O o ->
      lexeme e `Os;
      List.iter
        (fun (k, v) ->
          lexeme e (`Name k);
          encode_json e v)
        o;
      lexeme e `Oe

let decode_json d =
  let decode d =
    match Jsonm.decode d with
    | `Lexeme l -> l
    | `Error e -> failwith (Fmt.str "%a" Jsonm.pp_error e)
    | _ -> failwith "invalid JSON encoding"
  in
  let rec unwrap v d =
    match v with
    | `Os -> obj [] d
    | `As -> arr [] d
    | (`Null | `Bool _ | `String _ | `Float _) as v -> v
    | _ -> failwith "invalid JSON value"
  and arr vs d =
    match decode d with
    | `Ae -> `A (List.rev vs)
    | v ->
        let v = unwrap v d in
        arr (v :: vs) d
  and obj ms d =
    match decode d with
    | `Oe -> `O (List.rev ms)
    | `Name k ->
        let v = unwrap (decode d) d in
        obj ((k, v) :: ms) d
    | _ -> failwith "invalid JSON object"
  in
  try Ok (unwrap (decode d) d) with Failure msg -> Error (`Msg msg)

type json =
  [ `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `O of (string * json) list
  | `A of json list ]
[@@deriving irmin]

module Json_value = struct
  type t = json [@@deriving irmin]

  let pp fmt x =
    let buffer = Buffer.create 32 in
    let encoder = Jsonm.encoder (`Buffer buffer) in
    encode_json encoder x;
    ignore @@ Jsonm.encode encoder `End;
    let s = Buffer.contents buffer in
    Fmt.pf fmt "%s" s

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    match decode_json decoder with Ok obj -> Ok obj | Error _ as err -> err

  let equal_bool = Type.(unstage (equal bool))
  let equal_float = Type.(unstage (equal float))

  let rec equal a b =
    match (a, b) with
    | `Null, `Null -> true
    | `Bool a, `Bool b -> equal_bool a b
    | `String a, `String b -> String.equal a b
    | `Float a, `Float b -> equal_float a b
    | `A a, `A b -> (
        try List.for_all2 (fun a' b' -> equal a' b') a b
        with Invalid_argument _ -> false)
    | `O a, `O b -> (
        let compare_fst (a, _) (b, _) = compare a b in
        try
          List.for_all2
            (fun (k, v) (k', v') -> k = k' && equal v v')
            (List.sort compare_fst a) (List.sort compare_fst b)
        with Invalid_argument _ -> false)
    | _, _ -> false

  let t = Type.like ~equal ~pp ~of_string t

  let rec merge_object ~old x y =
    let open Merge.Infix in
    let m =
      Merge.(alist Type.string t (fun _key -> option (v t merge_value)))
    in
    Merge.(f m ~old x y) >>=* fun x -> Merge.ok (`O x)

  and merge_float ~old x y =
    let open Merge.Infix in
    Merge.(f float ~old x y) >>=* fun f -> Merge.ok (`Float f)

  and merge_string ~old x y =
    let open Merge.Infix in
    Merge.(f string ~old x y) >>=* fun s -> Merge.ok (`String s)

  and merge_bool ~old x y =
    let open Merge.Infix in
    Merge.(f bool ~old x y) >>=* fun b -> Merge.ok (`Bool b)

  and merge_array ~old x y =
    let open Merge.Infix in
    Merge.(f (Merge.idempotent (Type.list t)) ~old x y) >>=* fun x ->
    Merge.ok (`A x)

  and merge_value ~old x y =
    let open Merge.Infix in
    old () >>=* fun old ->
    match (old, x, y) with
    | Some `Null, _, _ -> merge_value ~old:(fun () -> Merge.ok None) x y
    | None, `Null, `Null -> Merge.ok `Null
    | Some (`Float old), `Float a, `Float b ->
        merge_float ~old:(fun () -> Merge.ok (Some old)) a b
    | None, `Float a, `Float b -> merge_float ~old:(fun () -> Merge.ok None) a b
    | Some (`String old), `String a, `String b ->
        merge_string ~old:(fun () -> Merge.ok (Some old)) a b
    | None, `String a, `String b ->
        merge_string ~old:(fun () -> Merge.ok None) a b
    | Some (`Bool old), `Bool a, `Bool b ->
        merge_bool ~old:(fun () -> Merge.ok (Some old)) a b
    | None, `Bool a, `Bool b -> merge_bool ~old:(fun () -> Merge.ok None) a b
    | Some (`A old), `A a, `A b ->
        merge_array ~old:(fun () -> Merge.ok (Some old)) a b
    | None, `A a, `A b -> merge_array ~old:(fun () -> Merge.ok None) a b
    | Some (`O old), `O a, `O b ->
        merge_object ~old:(fun () -> Merge.ok (Some old)) a b
    | None, `O a, `O b -> merge_object ~old:(fun () -> Merge.ok None) a b
    | _, _, _ -> Merge.conflict "Conflicting JSON datatypes"

  let merge_json = Merge.(v t merge_value)
  let merge = Merge.(option merge_json)
end

module Json = struct
  type t = (string * json) list [@@deriving irmin]

  let pp fmt x =
    let buffer = Buffer.create 32 in
    let encoder = Jsonm.encoder (`Buffer buffer) in
    encode_json encoder (`O x);
    ignore @@ Jsonm.encode encoder `End;
    let s = Buffer.contents buffer in
    Fmt.pf fmt "%s" s

  let of_string s =
    let decoder = Jsonm.decoder (`String s) in
    match decode_json decoder with
    | Ok (`O obj) -> Ok obj
    | Ok _ -> Error (`Msg "Irmin JSON values must be objects")
    | Error _ as err -> err

  let equal a b = Json_value.equal (`O a) (`O b)
  let t = Type.like ~equal ~pp ~of_string t

  let merge =
    Merge.(option (alist Type.string Json_value.t (fun _ -> Json_value.merge)))
end

module String_v2 = struct
  type t = string [@@deriving irmin]

  let merge = Merge.idempotent Type.(option string)
end

module String = struct
  type t = string [@@deriving irmin]

  let pre_hash = Type.(unstage (pre_hash t))

  (* Manually add a prefix to default contents, in order to prevent hash
     collision between contents and nodes or commits (see
     https://github.com/mirage/irmin/issues/1304). *)
  let pre_hash_prefixed x f =
    f "B";
    pre_hash x f

  let t = Type.(like t ~pre_hash:pre_hash_prefixed)
  let merge = Merge.idempotent Type.(option string)
end

module Store_indexable
    (S : Indexable.S)
    (H : Hash.S with type t = S.hash)
    (C : S with type t = S.value) =
struct
  module Val = C
  module Hash = Hash.Typed (H) (C)
  include S

  let read_opt t = function None -> Lwt.return_none | Some k -> find t k

  let add_opt t = function
    | None -> Lwt.return_none
    | Some v -> add t v >>= Lwt.return_some

  let merge t =
    Merge.like_lwt Type.(option Key.t) Val.merge (read_opt t) (add_opt t)
end

module Store
    (S : Content_addressable.S)
    (H : Hash.S with type t = S.key)
    (C : S with type t = S.value) =
  Store_indexable (Indexable.Of_content_addressable (H) (S)) (H) (C)

module V1 = struct
  module String = struct
    include String

    let t = Type.(boxed (string_of `Int64))

    type nonrec t = t [@@deriving irmin ~encode_bin ~decode_bin ~pre_hash]

    let size_of = Type.Size.t t
    let t = Type.like t ~bin:(encode_bin, decode_bin, size_of) ~pre_hash
  end
end
