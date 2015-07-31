(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

open Irmin

              
(** Signature of a general persistant index, allow to find the internal key from an index *)
module type LINK = sig
    type key
    type value	   
    type t
	   
    val create: config -> 'a Task.f -> ('a -> t) Lwt.t
    val read: t -> key -> value option Lwt.t
    val add: t -> key -> value -> unit Lwt.t
    val iter: t -> (key -> value Lwt.t -> unit Lwt.t) -> unit Lwt.t
    val mem: t -> key -> bool Lwt.t
    val length_index: int
    val digest_index: Cstruct.t -> key
    val length_key: int
    val digest_key: Cstruct.t -> value
end


(** Functor for creating a persistant index *)				 
module type LINK_MAKER =
  functor (K:Irmin.Hash.S) ->
  LINK with type key = K.t and type value = K.t

					      
(** Functor for creating a STORE with an persistant index *)
module Make (L: LINK_MAKER) (AO: AO_MAKER_RAW) (RW:RW_MAKER) : S_MAKER

									     
(** Index module (in-memory) implemented with HashTable *)
module MEM: LINK_MAKER

	      
(** Index module (file system) *)
	      (*module FS: LINK_MAKER*)


