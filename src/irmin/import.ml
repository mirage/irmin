(*
 * Copyright (c) 2019-2021 Craig Ferguson <craig@tarides.com>
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

(* Some functions from that file are xtracted from ocaml-containers:
   Copyright (c) 2013, Simon Cruanes *)

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
end

let direct_depth_default_ = 100

module List = struct
  include List
  (** @closed *)

  let rec is_longer_than : type a. int -> a list -> bool =
   fun len l ->
    if len < 0 then true
    else match l with [] -> false | _ :: tl -> is_longer_than (len - 1) tl

  let tail_map f l =
    (* Unwind the list of tuples, reconstructing the full list front-to-back.
       @param tail_acc a suffix of the final list; we append tuples' content
       at the front of it *)
    let rec rebuild tail_acc = function
      | [] -> tail_acc
      | (y0, y1, y2, y3, y4, y5, y6, y7, y8) :: bs ->
          rebuild
            (y0 :: y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: y7 :: y8 :: tail_acc)
            bs
    in
    (* Create a compressed reverse-list representation using tuples
       @param tuple_acc a reverse list of chunks mapped with [f] *)
    let rec dive tuple_acc = function
      | x0 :: x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: xs ->
          let y0 = f x0 in
          let y1 = f x1 in
          let y2 = f x2 in
          let y3 = f x3 in
          let y4 = f x4 in
          let y5 = f x5 in
          let y6 = f x6 in
          let y7 = f x7 in
          let y8 = f x8 in
          dive ((y0, y1, y2, y3, y4, y5, y6, y7, y8) :: tuple_acc) xs
      | xs ->
          (* Reverse direction, finishing off with a direct map *)
          let tail = List.map f xs in
          rebuild tail tuple_acc
    in
    dive [] l

  let map f l =
    let rec direct f i l =
      match l with
      | [] -> []
      | [ x ] -> [ f x ]
      | [ x1; x2 ] ->
          let y1 = f x1 in
          [ y1; f x2 ]
      | [ x1; x2; x3 ] ->
          let y1 = f x1 in
          let y2 = f x2 in
          [ y1; y2; f x3 ]
      | _ when i = 0 -> tail_map f l
      | x1 :: x2 :: x3 :: x4 :: l' ->
          let y1 = f x1 in
          let y2 = f x2 in
          let y3 = f x3 in
          let y4 = f x4 in
          y1 :: y2 :: y3 :: y4 :: direct f (i - 1) l'
    in
    direct f direct_depth_default_ l

  let concat l =
    let rec aux acc curr l =
      match (curr, l) with
      | [], [] -> List.rev acc
      | [], [ l ] -> List.rev_append acc l
      | [], h :: t -> (aux [@tailcall]) acc h t
      | h :: t, l -> (aux [@tailcall]) (h :: acc) t l
    in
    aux [] [] l
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

  let take : type a. int -> a t -> a list =
    let rec aux acc n l =
      if n = 0 then acc
      else
        match l () with Nil -> acc | Cons (x, l') -> aux (x :: acc) (n - 1) l'
    in
    fun n s -> List.rev (aux [] n s)

  let exists : type a. (a -> bool) -> a Seq.t -> bool =
   fun f s ->
    let rec aux s =
      match s () with Seq.Nil -> false | Seq.Cons (v, s) -> f v || aux s
    in
    aux s

  let of_seq_rev l =
    let rec loop acc s =
      match s () with Seq.Nil -> acc | Seq.Cons (x, tl) -> loop (x :: acc) tl
    in
    loop [] l

  let of_seq l =
    let rec direct i seq =
      if i <= 0 then List.rev (of_seq_rev seq)
      else
        match seq () with
        | Seq.Nil -> []
        | Seq.Cons (x, tl) -> x :: direct (i - 1) tl
    in
    direct direct_depth_default_ l
end
