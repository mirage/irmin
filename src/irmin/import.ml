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

module Mtime = struct
  include Mtime

  let span_to_s span = Mtime.Span.to_float_ns span *. 1e-9
  let span_to_us span = Mtime.Span.to_float_ns span *. 1e-3
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

  (* Since 4.14 *)
  let rec for_all2 f xs ys =
    match xs () with
    | Nil -> true
    | Cons (x, xs) -> (
        match ys () with
        | Nil -> true
        | Cons (y, ys) -> f x y && for_all2 f xs ys)
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
