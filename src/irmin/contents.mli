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

(** Values. *)

type json = [
  | `Null
  | `Bool of bool
  | `String of string
  | `Float of float
  | `O of (string * json) list
  | `A of json list
]

module String: S.CONTENTS with type t = string
module Cstruct: S.CONTENTS with type t = Cstruct.t
module Json: S.CONTENTS with type t = (string * json) list
module Json_value: S.CONTENTS with type t = json
module Json_tree(P: S.PATH)(M: S.METADATA): sig
  include S.CONTENTS with type t = json
  module type STORE = S.STORE with
    type contents = t
    and type key = P.t
    and type step = P.step
    and type metadata = M.t

  val get_tree:
    (module STORE with type node = 'a) ->
    [ `Contents of t * M.t | `Node of 'a ] ->
    P.t -> json Lwt.t

  val set_tree :
    (module STORE with type node = 'a and type t = 'b) ->
    [ `Contents of t * M.t | `Node of 'a ] ->
    P.t ->
    json -> [ `Contents of t * M.t | `Node of 'a ] Lwt.t

  val get :
    (module STORE with type t = 'a) ->
    'a -> P.t -> json Lwt.t

  val set :
    (module STORE with type t = 'a) ->
    'a -> P.t -> json -> info:Info.f -> unit Lwt.t
end

module Store
    (C: sig
       include S.AO
       module Key: S.HASH with type t = key
       module Val: S.CONTENTS with type t = value
     end):
  S.CONTENTS_STORE
  with type t = C.t
   and type key = C.key
   and type value = C.value
