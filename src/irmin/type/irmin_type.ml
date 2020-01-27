(*
 * Copyright (c) 2016-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Type_core

let pre_hash t x =
  let rec aux : type a. a t -> a bin_seq =
   fun t v k ->
    match t with
    | Self s -> aux s.self v k
    | Map m -> aux m.x (m.g v) k
    | Custom c -> c.pre_hash v k
    | _ -> Type_binary.encode_bin ?headers:(Some false) t v k
  in
  aux t x

let short_hash t ?seed x =
  match t with
  | Custom c -> c.short_hash ?seed x
  | _ ->
      let seed = match seed with None -> 0 | Some t -> t in
      let h = ref seed in
      pre_hash t x (fun s -> h := Hashtbl.seeded_hash !h s);
      !h

(* Combinators for Irmin types *)

let unit = Prim Unit

let bool = Prim Bool

let char = Prim Char

let int = Prim Int

let int32 = Prim Int32

let int64 = Prim Int64

let float = Prim Float

let string = Prim (String `Int)

let bytes = Prim (Bytes `Int)

let string_of n = Prim (String n)

let bytes_of n = Prim (Bytes n)

let list ?(len = `Int) v = List { v; len }

let array ?(len = `Int) v = Array { v; len }

let pair a b = Tuple (Pair (a, b))

let triple a b c = Tuple (Triple (a, b, c))

let option a = Option a

let v ~cli ~json ~bin ~equal ~compare ~short_hash ~pre_hash =
  let pp, of_string = cli in
  let encode_json, decode_json = json in
  let encode_bin, decode_bin, size_of = bin in
  Custom
    {
      cwit = `Witness (Witness.make ());
      pp;
      of_string;
      pre_hash;
      encode_json;
      decode_json;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
    }

(* fix points *)

let mu : type a. (a t -> a t) -> a t =
 fun f ->
  let rec fake_x = { self = Self fake_x } in
  let real_x = f (Self fake_x) in
  fake_x.self <- real_x;
  real_x

let mu2 : type a b. (a t -> b t -> a t * b t) -> a t * b t =
 fun f ->
  let rec fake_x = { self = Self fake_x } in
  let rec fake_y = { self = Self fake_y } in
  let real_x, real_y = f (Self fake_x) (Self fake_y) in
  fake_x.self <- real_x;
  fake_y.self <- real_y;
  (real_x, real_y)

(* records *)

type ('record, 'constr, 'remaining, 'lenses, 'lens_nil) open_record = {
  open_record :
    'hole. ('record, 'remaining, 'lens_nil, 'hole) fields ->
    (* Append the two lens lists at the type level *)
    string * 'constr * ('record, 'constr, 'lenses, 'hole) fields;
}

let field fname ftype fget = { fname; ftype; fget }

let record : type r. string -> r -> ('a, r, r, 'lens_nil, 'lens_nil) open_record
    =
 fun n r ->
  let open_record fs = (n, r, fs) in
  { open_record }

let app :
    type r c ft rem lens lens_nil.
    (r, c, ft -> rem, lens, (r, ft) Lens.mono * lens_nil) open_record ->
    (r, ft) field ->
    (r, c, rem, lens, lens_nil) open_record =
 fun { open_record = previous } field ->
  let open_record' :
      type hole.
      (r, rem, lens_nil, hole) fields -> string * c * (r, c, lens, hole) fields
      =
   fun fs -> previous (Fields_cons (field, fs))
  in
  { open_record = open_record' }

(* Ground lens list with [unit] *)
let sealr_with_optics :
    type record cons lens.
    (record, cons, record, lens, unit) open_record ->
    record t * lens Lens.t_list =
 fun { open_record = r } ->
  let name, cons, fields = r Fields_nil in
  let rwit = Witness.make () in
  let lenses =
    let open Lens in
    let rec inner : type a l. (record, a, l, unit) fields -> l Lens.t_list =
      function
      | Fields_nil -> []
      | Fields_cons ({ fget; _ }, fs) ->
          (* TODO: build mutator from the accessors and constructor *)
          let ml = Lens.v fget (fun _ _ -> assert false) in
          ml :: inner fs
    in
    inner fields
  in
  (Record { rwit; rname = name; rfields = Fields (fields, cons) }, lenses)

let sealr : type a b. (a, b, a, _, _) open_record -> a t =
 fun r -> sealr_with_optics r |> fst

let ( |+ ) = app

(* variants *)

type ('v, 'pat, 'rem, 'rem_nil, 'prism, 'prism_nil) open_variant = {
  open_variant :
    'hole1 'hole2. ('v, 'rem_nil, 'hole1, 'prism_nil, 'hole2) cases ->
    string * 'hole1 * ('v, 'rem, 'hole1, 'prism, 'hole2) cases;
  next_tag : int;
}

let variant : string -> 'p -> ('v, 'p, 'r, 'r, 'opt, 'opt) open_variant =
 fun n p ->
  let open_variant cs =
    let rec inner :
        type v pat pat_nil opt opt_nil.
        pat -> (v, pat, pat_nil, opt, opt_nil) cases -> pat_nil =
     fun p -> function
      | Cases_nil -> p
      | Cases_cons (C0 case, cs) -> inner (p (CV0 case)) cs
      | Cases_cons (C1 case1, cs) -> inner (p (fun v -> CV1 (case1, v))) cs
    in
    (n, inner p cs, cs)
  in
  { open_variant; next_tag = 0 }

let app :
    type v c constr pat rem rem_nil opt opt_nil.
    ( v,
      pat,
      rem,
      constr -> rem_nil,
      opt,
      (v, c) Prism.mono * opt_nil )
    open_variant ->
    (v, c, constr) case ->
    (v, pat, rem, rem_nil, opt, opt_nil) open_variant =
 fun { open_variant = previous; next_tag } case ->
  let open_variant' cs = previous (Cases_cons (case next_tag, cs)) in
  { open_variant = open_variant'; next_tag = next_tag + 1 }

let array_of_case_list cases =
  let rec inner :
      type variant pat pat_nil pri pri_nil.
      (variant, pat, pat_nil, pri, pri_nil) cases -> variant a_case list =
    function
    | Cases_nil -> []
    | Cases_cons (C0 c, cs) -> CP0 c :: inner cs
    | Cases_cons (C1 c, cs) -> CP1 c :: inner cs
  in
  inner cases |> Array.of_list

let sealv_with_optics :
    type variant pat prisms.
    (variant, pat, pat, variant -> variant case_v, prisms, unit) open_variant ->
    variant t * prisms Prism.t_list =
 fun { open_variant = v; _ } ->
  let vname, (vget : variant -> variant case_v), cases = v Cases_nil in
  let vwit = Witness.make () in
  let vcases = array_of_case_list cases in
  let prisms =
    let open Prism in
    let rec inner : type p a b. (variant, a, b, p, unit) cases -> p Prism.t_list
        = function
      | Cases_nil -> []
      | Cases_cons (C0 { c0; ctag0 = tag_expected; _ }, cs) ->
          let review () = c0 in
          let preview v =
            match vget v with
            | CV0 { ctag0 = tag_actual; _ } ->
                if tag_actual = tag_expected then Some () else None
            | CV1 _ -> None
          in
          Prism.v review preview :: inner cs
      | Cases_cons (C1 { c1; ctag1 = tag_expected; _ }, cs) ->
          let review = c1 in
          let preview v =
            match vget v with
            | CV1 ({ ctag1 = tag_actual; _ }, elt) ->
                if tag_actual = tag_expected then Some (Obj.magic elt) else None
            | CV0 _ -> None
          in
          Prism.v review preview :: inner cs
    in
    inner cases
  in
  (Variant { vwit; vname; vcases; vget }, prisms)

type 'a case_p = 'a case_v

let sealv :
    type a b opt. (a, b, b, a -> a case_v, opt, unit) open_variant -> a t =
 fun v -> sealv_with_optics v |> fst

let ( |~ ) = app

type ('var, 'case, 'constr) case = int -> ('var, 'case, 'constr) case_with_tag

let case0 cname0 c0 ctag0 = C0 { ctag0; cname0; c0 }

let case1 cname1 ctype1 c1 ctag1 = C1 { ctag1; cname1; ctype1; c1 }

let enum vname l =
  let vwit = Witness.make () in
  let _, vcases, mk =
    List.fold_left
      (fun (ctag0, cases, mk) (n, v) ->
        let c = { ctag0; cname0 = n; c0 = v } in
        (ctag0 + 1, CP0 c :: cases, (v, CV0 c) :: mk))
      (0, [], []) l
  in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget = (fun x -> List.assq x mk) }

let result a b =
  variant "result" (fun ok error ->
    function Ok x -> ok x | Error x -> error x)
  |~ case1 "ok" a (fun a -> Ok a)
  |~ case1 "error" b (fun b -> Error b)
  |> sealv

let like ?cli ?json ?bin ?equal ?compare ?short_hash:h ?pre_hash:p t =
  let encode_json, decode_json =
    let ( >|= ) l f = match l with Ok l -> Ok (f l) | Error _ as e -> e in
    let join = function Error _ as e -> e | Ok x -> x in
    match json with
    | Some (x, y) -> (x, y)
    | None -> (
        let rec is_prim : type a. a t -> bool = function
          | Self s -> is_prim s.self
          | Map m -> is_prim m.x
          | Prim _ -> true
          | _ -> false
        in
        match (t, cli) with
        | ty, Some (pp, of_string) when is_prim ty ->
            let ty = string in
            ( (fun ppf u -> Type_json.encode ty ppf (Fmt.to_to_string pp u)),
              fun buf -> Type_json.decode ty buf >|= of_string |> join )
        | _ -> (Type_json.encode t, Type_json.decode t) )
  in
  let pp, of_string =
    match cli with
    | Some (x, y) -> (x, y)
    | None -> (Type_pp.t t, Type_pp.of_string t)
  in
  let encode_bin, decode_bin, size_of =
    match bin with
    | Some (x, y, z) -> (x, y, z)
    | None -> (Type_binary.encode_bin t, Type_binary.decode_bin t, Type_size.t t)
  in
  let equal =
    match equal with
    | Some x -> x
    | None -> (
        match compare with
        | Some f -> fun x y -> f x y = 0
        | None -> Type_ordered.equal t )
  in
  let compare =
    match compare with Some x -> x | None -> Type_ordered.compare t
  in
  let short_hash ?seed =
    match h with Some x -> x | None -> short_hash ?seed t
  in
  let pre_hash =
    match p with Some x -> x | None -> encode_bin ?headers:(Some false)
  in
  Custom
    {
      cwit = `Type t;
      pp;
      of_string;
      encode_json;
      decode_json;
      encode_bin;
      decode_bin;
      size_of;
      compare;
      equal;
      short_hash;
      pre_hash;
    }

let map ?cli ?json ?bin ?equal ?compare ?short_hash ?pre_hash x f g =
  match (cli, json, bin, equal, compare, short_hash, pre_hash) with
  | None, None, None, None, None, None, None ->
      Map { x; f; g; mwit = Witness.make () }
  | _ ->
      let x = Map { x; f; g; mwit = Witness.make () } in
      like ?cli ?json ?bin ?equal ?compare ?short_hash ?pre_hash x

module type S = sig
  type t

  val t : t ty
end

let equal, compare = Type_ordered.(equal, compare)

let pp, pp_ty, to_string, of_string = Type_pp.(t, ty, to_string, of_string)

let ( to_json_string,
      of_json_string,
      pp_json,
      encode_json,
      decode_json,
      decode_json_lexemes ) =
  Type_json.(to_string, of_string, pp, encode, decode_jsonm, decode_lexemes)

let encode_bin, decode_bin, to_bin_string, of_bin_string =
  Type_binary.(encode_bin, decode_bin, to_bin_string, of_bin_string)

let size_of = Type_size.t
