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

module List = struct
  include List
  (** @closed *)

  let mem (k : string) l = mem k l

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
end
