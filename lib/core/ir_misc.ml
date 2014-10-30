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

open Sexplib.Std

module OP = struct
  let force oc s = output_string oc (Lazy.force s)
  let (!!) = force
  let show m t = Lazy.from_fun (fun () -> Tc.show m t)
  let shows m ts = Lazy.from_fun (fun () -> Tc.shows m ts)
end

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
    | x::(y::_ as tl) ->
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

let hashtbl_to_alist t =
  let l = ref [] in
  Hashtbl.iter (fun k v -> l := (k, v) :: !l) t;
  !l

let hashtbl_add_multi t k v =
  let vs =
    try Hashtbl.find t k
    with Not_found -> []
  in
  Hashtbl.replace t k (v::vs)

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

module type SET = sig
  include Set.S
  include Tc.I0 with type t := t
  val of_list: elt list -> t
  val to_list: t -> elt list
end

module Set (K: Tc.I0) = struct

  include Set.Make(K)

  let of_list l =
    List.fold_left (fun set elt -> add elt set) empty l

  let to_list = elements

  include Tc.L0(struct
      type nonrec t = t
      module K = K
      let to_list = to_list
      let of_list = of_list
    end)
end

module StringMap = Map(Tc.S)

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
