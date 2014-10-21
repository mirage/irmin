(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Printf
open Bin_prot.Std
open Sexplib.Std

module OP = struct

  let (!!) oc s = output_string oc (Lazy.force s)

end

module Hex = struct

  (* From OCaml's stdlib. See [Digest.to_hex] *)
  let encode s =
    let n = String.length s in
    let result = String.create (n*2) in
    for i = 0 to n-1 do
      String.blit (Printf.sprintf "%02x" (int_of_char s.[i])) 0 result (2*i) 2;
    done;
    result

  (* From OCaml's stdlib. See [Digest.from_hex] *)
  let decode h =
    let n = String.length h in
    if n mod 2 <> 0 then (
      let msg =
        Printf.sprintf "hex_decode: wrong string size for %S (%d)" h (String.length h) in
      raise (Invalid_argument msg)
    );
    let digit c =
      match c with
      | '0'..'9' -> Char.code c - Char.code '0'
      | 'A'..'F' -> Char.code c - Char.code 'A' + 10
      | 'a'..'f' -> Char.code c - Char.code 'a' + 10
      | c ->
        let msg = Printf.sprintf "hex_decode: %S is invalid" (String.make 1 c) in
        raise (Invalid_argument msg) in
    let byte i = digit h.[i] lsl 4 + digit h.[i+1] in
    let result = String.create (n / 2) in
    for i = 0 to n/2 - 1 do
      result.[i] <- Char.chr (byte (2 * i));
    done;
    result

end

module JSON = struct

  let is_valid_utf8 str =
    try
      Uutf.String.fold_utf_8 (fun _ _ -> function
          | `Malformed _ -> raise (Failure "utf8")
          | _ -> ()
        ) () str;
      true
    with Failure "utf8" -> false

  let encode_string str =
    if is_valid_utf8 str
    then Ezjsonm.string str
    else `O [ "hex", Ezjsonm.string (Hex.encode str) ]

  let decode_string = function
    | `String str               -> Some str
    | `O [ "hex", `String str ] -> Some (Hex.decode str)
    | j                         -> None

  let decode_string_exn j =
    match decode_string j with
    | Some s -> s
    | None   ->
      failwith (
        Printf.sprintf "%s is not a valid UT8-encoded JSON string"
          (Ezjsonm.to_string j)
      )

  let rec of_sexp = function
    | Sexplib.Type.Atom x -> encode_string x
    | Sexplib.Type.List l -> Ezjsonm.list of_sexp l

  let rec to_sexp json =
    match decode_string json with
    | Some s -> Sexplib.Type.Atom s
    | None   ->
      match json with
      | `A l -> Sexplib.Type.List (List.map to_sexp l)
      | _    -> failwith (sprintf "sexp_of_json: %s" (Ezjsonm.to_string json))

end

let invalid_argf fmt =
  ksprintf (fun str ->
      Invalid_argument str
    ) fmt

let list_partition_map f t =
  let rec aux fst snd = function
    | []   -> List.rev fst, List.rev snd
    | h::t ->
      match f h with
      | `Fst x -> aux (x :: fst) snd t
      | `Snd x -> aux fst (x :: snd) t
  in
  aux [] [] t

let list_pretty f = function
  | [] -> "{}"
  | l  ->
    let buf = Buffer.create 1024 in
    let len = ref (List.length l - 1) in
    Buffer.add_string buf "{ ";
    List.iter (fun e ->
        Buffer.add_string buf (f e);
        if !len > 0 then Buffer.add_string buf ", ";
        decr len
      ) l;
    Buffer.add_string buf " }";
    Buffer.contents buf

let list_dedup ?(compare=Pervasives.compare) t =
  let t = List.sort compare t in
  let rec aux acc = function
    | []      -> List.rev acc
    | [x]     -> aux (x :: acc) []
    | x::(y::t as tl) ->
      match compare x y with
      | 0 -> aux acc tl
      | _ -> aux (x :: acc) tl
  in
  aux [] t

let list_filter_map f l =
  List.fold_left (fun acc x -> match f x with
      | None -> acc
      | Some y -> y :: acc
    ) [] l

module type MAP = sig
  include Map.S
  include Tc.I1 with type 'a t := 'a t
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val keys: 'a t -> key list
  val add_multi: key -> 'a -> 'a list t -> 'a list t
  val iter2:
    (key -> [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> unit)
    -> 'a t -> 'b t -> unit
  module Lwt: sig
    val merge:
      (key -> [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> 'c option Lwt.t)
      -> 'a t -> 'b t -> 'c t Lwt.t
    val iter2:
      (key -> [ `Both of 'a * 'b | `Left of 'a | `Right of 'b ] -> unit Lwt.t)
      -> 'a t -> 'b t -> unit Lwt.t
  end
end
module Map (K: Tc.I0) = struct

  include Map.Make(K)

  let keys m =
    List.map fst (bindings m)

  let of_alist l =
    List.fold_left (fun map (k, v)  -> add k v map) empty l

  let to_alist = bindings

  let add_multi key data t =
    try
      let l = find key t in
      add key (data :: l) t
    with Not_found ->
      add key [data] t

  let iter2 f t1 t2 =
    let rec aux l1 l2 = match l1, l2 with
      | [], t -> List.iter (fun (key, v) -> f key (`Right v)) t
      | t, [] -> List.iter (fun (key, v) -> f key (`Left v)) t
      | (k1,v1)::t1, (k2,v2)::t2 ->
        match K.compare k1 k2 with
        | 0 ->
          f k1 (`Both (v1, v2));
          aux t1 t2
        | x -> if x < 0 then (
            f k1 (`Left v1);
            aux t1 l2
          ) else (
            f k2 (`Right v2);
            aux l1 t2
          )
    in
    aux (bindings t1) (bindings t2)

  module Lwt = struct
    open Lwt

    let iter2 f m1 m2 =
      let m3 = ref [] in
      iter2 (fun key data ->
          m3 := f key data :: !m3
        ) m1 m2;
      Lwt_list.iter_p
        (fun b -> b >>= fun () -> return_unit) (List.rev !m3)

    let merge f m1 m2 =
      let l3 = ref [] in
      let f key data =
        f key data >>= function
        | None   -> return_unit
        | Some v -> l3 := (key, v) :: !l3; return_unit
      in
      iter2 f m1 m2 >>= fun () ->
      let m3 = of_alist !l3 in
      return m3

  end

  include Tc.AL(struct
      type 'a r = 'a t
      type 'a t = 'a r
      module K = K
      let of_alist = of_alist
      let to_alist = to_alist
    end)
end

let hashtbl_add_multi t k v =
  let vs =
    try Hashtbl.find t k
    with Not_found -> []
  in
  Hashtbl.replace t k (v::vs)

(** Persistent Sets. *)
module type SET = sig
  include Set.S
  include I0 with type t := t
  val of_list: elt list -> t
  val to_list: t -> elt list
end
module Set (K: I0) = struct

  include Set.Make(K)

  let of_list l =
    List.fold_left (fun set elt -> add elt set) empty l

  let to_list = elements

  include L0(struct
      type r = t
      type t = r
      module K = K
      let to_list = to_list
      let of_list = of_list
    end)
end

module S = I0(struct type t = string with compare, sexp, bin_io end)
module U = I0(struct type t = unit with compare, sexp, bin_io end)
module O = I1(struct type 'a t = 'a option with compare, sexp, bin_io end)
module P = I2(struct type ('a, 'b) t = 'a * 'b with compare, sexp, bin_io end)
module I = I0(struct type t = int with compare, sexp, bin_io end)
module L = I1(struct type 'a t = 'a list with sexp, compare, bin_io end)

module StringMap = Map(S)

module Lwt_stream = struct

  include Lwt_stream

  open Lwt

  let lift s =
    let (stream: 'a Lwt_stream.t option ref) = ref None in
    let rec get () =
      match !stream with
      | Some s -> Lwt_stream.get s
      | None   ->
        s >>= fun s ->
        stream := Some s;
        get ()
    in
    Lwt_stream.from get

end
