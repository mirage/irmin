(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type S = sig
  include
    Irmin.S
      with type step = string
       and type key = string list
       and type contents = string
       and type branch = string

  val gc_hook : (repo -> commit list -> unit Lwt.t) option
end

module type Layered_store = sig
  include
    Irmin_layers.S
      with type step = string
       and type key = string list
       and type contents = string
       and type branch = string

  val gc_hook : (repo -> commit list -> unit Lwt.t) option
end

val reporter : ?prefix:string -> unit -> Logs.reporter

type t = {
  name : string;
  init : unit -> unit Lwt.t;
  clean : unit -> unit Lwt.t;
  config : Irmin.config;
  store : (module S);
  layered_store : (module Layered_store) option;
  stats : (unit -> int * int) option;
}

val line : string -> unit
val store : (module Irmin.Maker) -> (module Irmin.Metadata.S) -> (module S)

val layered_store :
  (module Irmin_layers.Maker) ->
  (module Irmin.Metadata.S) ->
  (module Layered_store)

val testable : 'a Irmin.Type.t -> 'a Alcotest.testable
val check : 'a Irmin.Type.t -> string -> 'a -> 'a -> unit
val checks : 'a Irmin.Type.t -> string -> 'a list -> 'a list -> unit

module Store : sig
  val run :
    string ->
    ?slow:bool ->
    misc:unit Alcotest.test list ->
    (Alcotest.speed_level * t) list ->
    unit
end

module Common = Common
