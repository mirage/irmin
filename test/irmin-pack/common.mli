module Dict = Irmin_pack.Dict
module H = Irmin.Hash.SHA1
module I = Index

module Index : Irmin_pack.Index.S with type key = H.t

module Pack :
  Irmin_pack.Pack.S
    with type key = H.t
     and type value = string
     and type index = Index.t

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
    pack : [ `Read ] Pack.t;
    clone_pack : readonly:bool -> [ `Read ] Pack.t Lwt.t;
    clone_index_pack : readonly:bool -> (Index.t * [ `Read ] Pack.t) Lwt.t;
  }

  val get_pack : unit -> t Lwt.t
  (** Fresh, empty index and pack. [clone_pack] opens a clone of the pack at the
      same location, [clone_index_pack] opens a clone of the index and the pack. *)

  val close : Index.t -> [ `Read ] Pack.t -> unit Lwt.t
end

val get : 'a option -> 'a

val sha1 : string -> H.t

val rm_dir : string -> unit
