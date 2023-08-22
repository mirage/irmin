(*
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

open Irmin_server

type addr =
  [ `TLS of [ `Hostname of string ] * [ `IP of Ipaddr.t ] * [ `Port of int ]
  | `TCP of [ `IP of Ipaddr.t ] * [ `Port of int ]
  | `Ws of ([ `IP of Ipaddr.t ] * [ `Port of int ]) option * string
  | `Unix_domain_socket of [ `File of string ] ]

module type IO = sig
  include Conn.IO

  type ctx

  val default_ctx : ctx lazy_t
  val connect : ctx:ctx -> addr -> (ic * oc) Lwt.t
  val close : ic * oc -> unit Lwt.t
end

module type S = sig
  include Irmin.Generic_key.S

  val connect : ?tls:bool -> ?hostname:string -> Uri.t -> repo Lwt.t
  val reconnect : repo -> unit Lwt.t

  val uri : repo -> Uri.t
  (** Get the URI the client is connected to *)

  val close : repo -> unit Lwt.t
  (** Close connection to the server *)

  val dup : repo -> repo Lwt.t
  (** Duplicate a client. This will create a new connection with the same
      configuration *)

  val ping : repo -> unit Error.result Lwt.t
  (** Ping the server *)

  val export : ?depth:int -> repo -> slice Lwt.t
  val import : repo -> slice -> unit Lwt.t
  val current_branch : t -> branch Lwt.t

  (** The batch API is used to have better control of when data is sent between
      the client and server when manipulating trees. *)
  module Batch : sig
    module Request_tree : Irmin_server.Tree.S

    type store = t

    type batch_contents =
      [ `Hash of hash | `Value of contents ] * metadata option

    type t =
      (path
      * [ `Contents of batch_contents | `Tree of Request_tree.t | `Remove ])
      list
    (** A batch is list of updates and their associated paths *)

    val v : unit -> t
    (** [val ()] creates a new batch *)

    val add_value : path -> ?metadata:metadata -> contents -> t -> t
    (** [add_value path ~metadata value batch] will add [value] at [path] with
        associated [metadata] when [batch] is {!apply}'d *)

    val add_hash : path -> ?metadata:metadata -> hash -> t -> t
    (** [add_hash path ~metadata hash batch] will add [hash] at [path] with
        associated [metadata] when [batch] is {!apply}'d *)

    val add_tree : path -> tree -> t -> t Lwt.t
    (** [add_tree path batch] will add [tree] at [path] when [batch] is
        {!apply}'d

        Note: if [tree] has been modified locally, calls to the server may be
        made. *)

    val remove : path -> t -> t
    (** [remove path batch] will remove [path] when [batch] is {!apply}'d *)

    val apply : info:Info.f -> ?path:path -> store -> t -> commit_key Lwt.t
    (** [apply ~info ~path store batch] applies [batch] to the subtree at [path]
        (defaults to the root) in [store]. The key of the commit is returned. *)
  end
end

module type Client = sig
  module type S = S

  type nonrec addr = addr

  module type IO = IO

  val config : ?tls:bool -> ?hostname:string -> Uri.t -> Irmin.config

  module Make (I : IO) (Codec : Conn.Codec.S) (Store : Irmin.Generic_key.S) :
    S
      with module Schema = Store.Schema
       and type Backend.Remote.endpoint = unit
       and type commit_key = Store.commit_key
       and type contents_key = Store.contents_key
       and type node_key = Store.node_key
end
