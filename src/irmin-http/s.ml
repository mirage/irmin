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

module T = Irmin.Type

module type HELPER = sig
  type ctx

  val err_bad_version : string option -> 'a Lwt.t

  val check_version : Cohttp.Response.t -> unit Lwt.t

  val is_success : Cohttp.Response.t -> bool

  val map_string_response :
    (string -> ('a, [< `Msg of string ]) result) ->
    Cohttp.Response.t * Cohttp_lwt.Body.t ->
    'a Lwt.t

  val map_stream_response :
    'a T.t -> Cohttp.Response.t * Cohttp_lwt.Body.t -> 'a Lwt_stream.t Lwt.t

  val headers : keep_alive:bool -> unit -> Cohttp.Header.t

  val map_call :
    Cohttp.Code.meth ->
    Uri.t ->
    ctx option ->
    keep_alive:bool ->
    ?body:string ->
    string list ->
    (Cohttp.Response.t * Cohttp_lwt.Body.t -> 'a Lwt.t) ->
    'a Lwt.t

  val call :
    Cohttp.Code.meth ->
    Uri.t ->
    ctx option ->
    ?body:string ->
    string list ->
    (string -> ('a, [< `Msg of string ]) result) ->
    'a Lwt.t

  val call_stream :
    Cohttp.Code.meth ->
    Uri.t ->
    ctx option ->
    ?body:string ->
    string list ->
    'a T.t ->
    'a Lwt_stream.t Lwt.t
end

module type READ_ONLY_STORE = sig
  type ctx

  type 'a t = { uri : Uri.t; item : string; items : string; ctx : ctx option }

  type key

  type value

  module HTTP : HELPER with type ctx = ctx

  val uri : 'a t -> Uri.t

  val item : 'a t -> string

  val items : 'a t -> string

  val key_str : key -> string

  val val_of_str : value T.of_string

  val find : 'a t -> key -> value option Lwt.t

  val mem : 'a t -> key -> bool Lwt.t

  val cast : 'a t -> [ `Read | `Write ] t

  val batch : 'a t -> ([ `Read | `Write ] t -> 'b) -> 'b

  val v : ?ctx:ctx -> Uri.t -> string -> string -> 'a t Lwt.t
end

module type APPEND_ONLY_STORE = sig
  type 'a t

  type key

  type value

  type ctx

  val mem : [> `Read ] t -> key -> bool Lwt.t

  val find : [> `Read ] t -> key -> value option Lwt.t

  val add : 'a t -> value -> key Lwt.t

  val unsafe_add : 'a t -> key -> value -> unit Lwt.t

  val v : ?ctx:ctx -> Uri.t -> string -> string -> 'a t Lwt.t

  val close : 'a t -> unit Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t
end

module type APPEND_ONLY_STORE_MAKER = functor
  (Client : Cohttp_lwt.S.Client)
  (K : Irmin.Hash.S)
  (V : Irmin.Type.S)
  ->
  APPEND_ONLY_STORE
    with type key = K.t
     and type value = V.t
     and type ctx = Client.ctx

module type ATOMIC_WRITE_STORE = sig
  include Irmin.ATOMIC_WRITE_STORE

  type ctx

  val v : ?ctx:ctx -> Uri.t -> string -> string -> t Lwt.t
end

module type ATOMIC_WRITE_STORE_MAKER = functor
  (Client : Cohttp_lwt.S.Client)
  (K : Irmin.Branch.S)
  (V : Irmin.Hash.S)
  -> sig
  module W : Irmin.Private.Watch.S with type key = K.t and type value = V.t

  module RO : READ_ONLY_STORE

  module HTTP = RO.HTTP

  include
    ATOMIC_WRITE_STORE
      with type key = K.t
       and type value = V.t
       and type ctx = Client.ctx
end
