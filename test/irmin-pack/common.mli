open Irmin.Perms
module Dict : Irmin_pack.Dict.S
module H = Irmin.Hash.SHA1
module I = Index

module Filename : sig
  include module type of Filename

  val quote_command :
    string ->
    ?stdin:string ->
    ?stdout:string ->
    ?stderr:string ->
    string list ->
    string
end

module Alcotest : sig
  include module type of Alcotest

  val check_raises_lwt : string -> exn -> (unit -> _ Lwt.t) -> unit Lwt.t
  val check_repr : 'a Irmin.Type.t -> string -> 'a -> 'a -> unit
end

module Index : Irmin_pack.Index.S with type key = H.t

module Pack :
  Irmin_pack.Pack.INDEXED_S
    with type key = H.t
     and type value = string
     and type index = Index.t

module P :
  Irmin_pack.Pack.INDEXED_MAKER
    with type key = H.t
     and type index = Irmin_pack.Index.Make(H).t

(** Helper constructors for fresh pre-initialised dictionaries and packs *)
module Make_context (Config : sig
  val root : string
end) : sig
  val fresh_name : string -> string
  (** [fresh_name typ] is a clean directory for a resource of type [typ]. *)

  type d = { dict : Dict.t; clone : readonly:bool -> Dict.t }

  val get_dict : unit -> d
  (** Fresh, empty dict. *)

  type t = {
    index : Index.t;
    pack : read Pack.t;
    clone_pack : readonly:bool -> read Pack.t Lwt.t;
    clone_index_pack : readonly:bool -> (Index.t * read Pack.t) Lwt.t;
  }

  val get_pack : ?lru_size:int -> unit -> t Lwt.t
  (** Fresh, empty index and pack. [clone_pack] opens a clone of the pack at the
      same location, [clone_index_pack] opens a clone of the index and the pack. *)

  val close : Index.t -> read Pack.t -> unit Lwt.t
end

val ( let* ) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val get : 'a option -> 'a
val sha1 : string -> H.t
val rm_dir : string -> unit
val index_log_size : int option

module Conf : sig
  val entries : int
  val stable_hash : int
end

val random_string : int -> string
val random_letters : int -> string
