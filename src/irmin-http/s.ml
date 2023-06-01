(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
module T = Irmin.Type

module type Helper = sig
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

module Read_only = struct
  module type S = sig
    type ctx

    type -'a t = {
      uri : Uri.t;
      item : string;
      items : string;
      ctx : ctx option;
    }

    include Irmin.Read_only.S with type 'a t := 'a t
    module HTTP : Helper with type ctx = ctx

    val uri : 'a t -> Uri.t
    val item : 'a t -> string
    val items : 'a t -> string
    val key_str : key -> string
    val val_of_str : value T.of_string
    val v : ?ctx:ctx -> Uri.t -> string -> string -> 'a t
  end
end

module Content_addressable = struct
  module type S = sig
    include Irmin.Content_addressable.S

    type ctx

    val v : ?ctx:ctx -> Uri.t -> string -> string -> 'a t
  end

  module type Maker = functor
    (Client : Cohttp_lwt.S.Client)
    (H : Irmin.Hash.S)
    (V : Irmin.Type.S)
    -> S with type key = H.t and type value = V.t and type ctx = Client.ctx
end

module Atomic_write = struct
  module type S = sig
    include Irmin.Atomic_write.S

    type ctx

    val v : ?ctx:ctx -> Uri.t -> string -> string -> t
  end

  module type Maker = functor
    (Client : Cohttp_lwt.S.Client)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    -> sig
    module W : Irmin.Backend.Watch.S with type key = B.t and type value = H.t
    module RO : Read_only.S
    module HTTP = RO.HTTP
    include S with type key = B.t and type value = H.t and type ctx = Client.ctx
  end
end
