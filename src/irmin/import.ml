(*
 * Copyright (c) 2019-2021 Craig Ferguson <craig@tarides.com>
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

(* Extensions to the default namespace, opened throughout the Irmin codebase. *)

type read = Perms.read
type write = Perms.write
type read_write = Perms.read_write

(** {2 Lwt syntax} *)

include Lwt.Syntax

let ( >>= ) = Lwt.Infix.( >>= )
let ( >|= ) = Lwt.Infix.( >|= )

(** {2 Dependency extensions} *)

module Option = struct
  include Option
  (** @closed *)

  let of_result = function Ok x -> Some x | Error _ -> None
  let might f = function Some x -> f x | None -> Ok ()
end

module List = struct
  include List
  (** @closed *)

  let rec is_longer_than : type a. int -> a list -> bool =
   fun len l ->
    if len < 0 then true
    else match l with [] -> false | _ :: tl -> is_longer_than (len - 1) tl

  let map f l =
    let rec aux acc = function
      | [] -> acc []
      | h :: t -> (aux [@tailcall]) (fun t' -> acc (f h :: t')) t
    in
    aux (fun x -> x) l

  let concat l =
    let rec aux acc curr l =
      match (curr, l) with
      | [], [] -> List.rev acc
      | [], [ l ] -> List.rev_append acc l
      | [], h :: t -> (aux [@tailcall]) acc h t
      | h :: t, l -> (aux [@tailcall]) (h :: acc) t l
    in
    aux [] [] l

  (* For compatibility with versions older than ocaml.4.11.0 *)
  let concat_map f l =
    let rec aux f acc = function
      | [] -> rev acc
      | x :: l ->
          let xs = f x in
          aux f (rev_append xs acc) l
    in
    aux f [] l

  let rec mem : type a. equal:(a -> a -> bool) -> a -> a t -> bool =
   fun ~equal y -> function
    | [] -> false
    | x :: xs -> equal x y || mem ~equal y xs

  let rec rev_append_map : type a b. (a -> b) -> a list -> b list -> b list =
   fun f xs ys ->
    match xs with [] -> ys | x :: xs -> rev_append_map f xs (f x :: ys)

  let insert_exn : type a. a list -> int -> a -> a list =
   fun l idx v ->
    (* [list_insert l 0 v] is [v :: l] *)
    assert (idx >= 0);
    let rec aux l i acc =
      if i = 0 then List.rev_append acc (v :: l)
      else
        match l with
        | [] -> failwith "list_insert: input list too short"
        | hd :: tl -> aux tl (i - 1) (hd :: acc)
    in
    aux l idx []
end

module Seq = struct
  include Seq
  (** @closed *)

  let rec drop : type a. int -> a t -> a t =
   fun n l () ->
    match l () with
    | l' when n = 0 -> l'
    | Nil -> Nil
    | Cons (_, l') -> drop (n - 1) l' ()

  let exists : type a. (a -> bool) -> a Seq.t -> bool =
   fun f s ->
    let rec aux s =
      match s () with Seq.Nil -> false | Seq.Cons (v, s) -> f v || aux s
    in
    aux s

  let rec take : type a. int -> a t -> a t =
   fun n l () ->
    if n = 0 then Nil
    else match l () with Nil -> Nil | Cons (x, l') -> Cons (x, take (n - 1) l')

  let for_all : type a. (a -> bool) -> a Seq.t -> bool =
   fun f s ->
    let rec aux s =
      match s () with Seq.Nil -> true | Seq.Cons (v, s) -> f v && aux s
    in
    aux s

  (* For compatibility with versions older than ocaml.4.11.0 *)
  let rec append seq1 seq2 () =
    match seq1 () with
    | Nil -> seq2 ()
    | Cons (x, next) -> Cons (x, append next seq2)
end

let shuffle state arr =
  let rec aux n =
    if n > 1 then (
      let k = Random.State.int state (n + 1) in
      let temp = arr.(n) in
      arr.(n) <- arr.(k);
      arr.(k) <- temp;
      aux (n - 1))
  in
  let len = Array.length arr in
  aux (len - 1);
  ()

module Small_map = struct
  module Make (K : sig
    type t [@@deriving irmin]
  end) : sig
    type 'a t [@@deriving irmin]
    type key = K.t

    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val of_list : (key * 'a) list -> 'a t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val cardinal : 'a t -> int
    val to_seq : 'a t -> (key * 'a) Seq.t
    val of_seq : (key * 'a) Seq.t -> 'a t
    val bindings : 'a t -> (key * 'a) list
    val is_empty : 'a t -> bool
    val find : key -> 'a t -> 'a
    val find_opt : key -> 'a t -> 'a option
    val singleton : key -> 'a -> 'a t
    val remove : key -> 'a t -> 'a t
    val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end = struct
    type key = K.t [@@deriving irmin ~compare ~pp]
    type 'a t = (key * 'a) list [@@deriving irmin]

    (* Use a (short) sorted list of pairs *)
    let empty = []
    let is_empty = function [] -> true | _ -> false
    let cardinal = List.length

    exception No_change

    let compare_pair (x, _) (y, _) = compare_key x y

    let remove k t =
      (* non tail-rec as it's a short list *)
      let rec aux = function
        | [] -> raise No_change
        | ((x, _) as e) :: rest -> (
            match compare_key x k with
            | 0 -> rest
            | i when i < 0 -> e :: aux rest
            | _ -> raise No_change)
      in
      try aux t with No_change -> t

    let add k v t =
      (* non tail-rec as it's a short list *)
      let rec aux t =
        match t with
        | [] -> [ (k, v) ]
        | ((x, y) as e) :: rest -> (
            match compare_key x k with
            | 0 -> if y == v then raise No_change else (x, v) :: rest
            | i when i < 0 -> e :: aux rest
            | _ -> (k, v) :: t)
      in
      try aux t with No_change -> t

    let bindings t = t
    let to_seq t = List.to_seq t
    let singleton k v = [ (k, v) ]
    let iter f t = List.iter (fun (k, v) -> f k v) t

    let find k t =
      let rec aux = function
        | [] -> raise Not_found
        | (x, v) :: rest -> (
            match compare_key x k with
            | 0 -> v
            | i when i < 0 -> aux rest
            | _ -> raise Not_found)
      in
      aux t

    let find_opt k t = try Some (find k t) with Not_found -> None
    let fold f t acc = List.fold_left (fun acc (k, v) -> f k v acc) acc t
    let of_list l = List.sort_uniq compare_pair l
    let of_seq s = of_list (List.of_seq s)

    let update k f t =
      (* non tail-rec as it's a short list *)
      let rec aux t =
        match t with
        | [] -> (
            match f None with None -> raise No_change | Some v -> [ (k, v) ])
        | ((x, y) as e) :: rest -> (
            match compare_key x k with
            | 0 -> (
                match f (Some y) with
                | None -> rest
                | Some v -> if v == y then raise No_change else (x, v) :: rest)
            | i when i < 0 -> e :: aux rest
            | _ -> (
                match f None with
                | None -> raise No_change
                | Some v -> (k, v) :: t))
      in
      try aux t with No_change -> t
  end
end
