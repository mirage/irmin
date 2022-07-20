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

  module Batch : sig
    type store = t

    module Tree : sig
      include
        Irmin_server.Tree.S
          with type concrete = Tree.concrete
           and type kinded_key = Tree.kinded_key

      val empty : repo -> t Lwt.t
      val of_hash : repo -> hash -> t option Lwt.t
      val of_path : store -> path -> t option Lwt.t
      val of_commit : repo -> hash -> t option Lwt.t

      val save :
        repo -> t -> [ `Contents of contents_key | `Node of node_key ] Lwt.t

      val to_local : repo -> t -> tree Lwt.t
      val of_local : tree -> t Lwt.t

      val of_key : kinded_key -> t
      (** Create a tree from a key that specifies a tree that already exists in
          the store *)

      val key : repo -> t -> kinded_key Lwt.t
      (** Get key of tree *)

      val add : repo -> t -> path -> contents -> t Lwt.t
      (** Add contents to a tree *)

      val add_tree : repo -> t -> path -> t -> t Lwt.t

      val find : repo -> t -> path -> contents option Lwt.t
      (** Find the value associated with the given path *)

      val find_tree : repo -> t -> path -> t option Lwt.t
      (** Find the tree associated with the given path *)

      val remove : repo -> t -> path -> t Lwt.t
      (** Remove value from a tree, returning a new tree *)

      val cleanup : repo -> t -> unit Lwt.t
      (** Invalidate a tree, this frees the tree on the server side *)

      val cleanup_all : repo -> unit Lwt.t
      (** Cleanup all trees *)

      val mem : repo -> t -> path -> bool Lwt.t
      (** Check if a path is associated with a value *)

      val mem_tree : repo -> t -> path -> bool Lwt.t
      (** Check if a path is associated with a tree *)

      val list :
        repo -> t -> path -> (Path.step * [ `Contents | `Tree ]) list Lwt.t
      (** List entries at the specified root *)

      val merge : repo -> old:t -> t -> t -> t Lwt.t
      (** Three way merge *)

      val hash : repo -> t -> hash Lwt.t
      val clear : repo -> t -> unit Lwt.t
    end

    type batch_contents =
      [ `Hash of hash | `Value of contents ] * metadata option

    type t =
      (path * [ `Contents of batch_contents | `Tree of Tree.t ] option) list

    val v : unit -> t
    val of_tree : ?path:path -> Tree.t -> t
    val of_contents : ?path:path -> ?metadata:metadata -> contents -> t

    val commit :
      parents:Commit.t list -> info:Info.f -> repo -> Tree.t -> Commit.t Lwt.t

    val apply :
      ?parents:Commit.t list -> info:Info.f -> store -> path -> t -> unit Lwt.t

    val tree : repo -> t -> Tree.t -> Tree.t Lwt.t
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
