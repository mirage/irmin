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
  val connect : sw:Eio.Switch.t -> ctx:ctx -> addr -> (ic * oc) Lwt.t
  val close : ic * oc -> unit Lwt.t
end

(** Server-side trees. *)
module type Batch = sig
  type repo
  type hash
  type path
  type contents_key
  type node_key
  type store
  type tree
  type contents
  type step

  include Irmin_server.Tree.S

  (** Every tree generated on the server side is associated with a unique ID.
      Mmanually call {!cleanup} to collect this ID when not in used anymore.

      TODO: devise a better scheme. *)

  val empty : repo -> t
  val of_hash : repo -> hash -> t option
  val of_path : store -> path -> t option
  val of_commit : repo -> hash -> t option
  val save : repo -> t -> [ `Contents of contents_key | `Node of node_key ]
  val to_local : repo -> t -> tree
  val of_local : tree -> t

  val of_key : kinded_key -> t
  (** Create a tree from a key that specifies a tree that already exists in the
      store *)

  val key : repo -> t -> kinded_key
  (** Get key of tree *)

  val add : repo -> t -> path -> contents -> t
  (** Add contents to a tree *)

  val add_tree : repo -> t -> path -> t -> t

  val find : repo -> t -> path -> contents option
  (** Find the value associated with the given path *)

  val find_tree : repo -> t -> path -> t option
  (** Find the tree associated with the given path *)

  val remove : repo -> t -> path -> t
  (** Remove value from a tree, returning a new tree *)

  val cleanup : repo -> t -> unit
  (** Invalidate a tree, this frees the tree on the server side *)

  val cleanup_all : repo -> unit
  (** Cleanup all trees *)

  val mem : repo -> t -> path -> bool
  (** Check if a path is associated with a value *)

  val mem_tree : repo -> t -> path -> bool
  (** Check if a path is associated with a tree *)

  val list : repo -> t -> path -> (step * [ `Contents | `Tree ]) list
  (** List entries at the specified root *)

  val merge : repo -> old:t -> t -> t -> t
  (** Three way merge *)

  val hash : repo -> t -> hash
  val clear : repo -> t -> unit
end

module type S = sig
  include Irmin.Generic_key.S

  val connect :
    sw:Eio.Switch.t -> ?tls:bool -> ?hostname:string -> Uri.t -> repo

  val reconnect : repo -> unit Lwt.t

  val uri : repo -> Uri.t
  (** Get the URI the client is connected to *)

  val close : repo -> unit
  (** Close connection to the server *)

  val dup : repo -> repo Lwt.t
  (** Duplicate a client. This will create a new connection with the same
      configuration *)

  val ping : repo -> unit Error.result
  (** Ping the server *)

  val export : ?depth:int -> repo -> slice
  val import : repo -> slice -> unit
  val current_branch : t -> branch Lwt.t

  (** The batch API is used to have better control of when data is sent between
      the client and server. It is designed to be similar to the [Tree] API but
      functions in this module do not make requests to the server. *)
  module Batch : sig
    type store = t

    module Tree :
      Batch
        with type concrete = Tree.concrete
         and type kinded_key = Tree.kinded_key
         and type repo := repo
         and type hash := hash
         and type path := path
         and type contents_key := contents_key
         and type node_key := node_key
         and type store := store
         and type tree := tree
         and type contents := contents
         and type step := step

    type batch_contents =
      [ `Hash of hash | `Value of contents ] * metadata option

    type t =
      (path * [ `Contents of batch_contents | `Tree of Tree.t ] option) list
    (** A batch is list of updated and their associated paths *)

    val v : unit -> t
    val of_tree : ?path:path -> Tree.t -> t
    val of_contents : ?path:path -> ?metadata:metadata -> contents -> t

    val commit :
      parents:Commit.t list -> info:Info.f -> repo -> Tree.t -> Commit.t
    (** Commit a batch tree, returning the resulting commit. This will make a
        request to the server with the entire bulk tree. *)

    val apply :
      ?parents:Commit.t list -> info:Info.f -> store -> path -> t -> unit
    (** Submit a batch update to the server. This will make a request to the
        server with the entire bulk update. *)

    val tree : repo -> t -> Tree.t -> Tree.t
    val find : t -> path -> batch_contents option
    val find_tree : t -> path -> Tree.t option
    val mem : t -> path -> bool
    val mem_tree : t -> path -> bool
    val remove : t -> path -> t
    val add : t -> path -> ?metadata:metadata -> contents -> t
    val add_hash : t -> path -> ?metadata:metadata -> hash -> t
    val add_tree : t -> path -> Tree.t -> t
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
