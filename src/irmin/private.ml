open! Import
open S

module type S = sig
  module Hash : Hash.S
  (** Internal hashes. *)

  module Contents : Contents.STORE with type key = Hash.t
  (** Private content store. *)

  module Node : Node.STORE with type key = Hash.t
  (** Private node store. *)

  module Commit : Commit.STORE with type key = Hash.t
  (** Private commit store. *)

  module Branch : Branch.STORE with type value = Hash.t
  (** Private branch store. *)

  (** Private slices. *)
  module Slice :
    Slice.S
      with type contents = Contents.key * Contents.value
       and type node = Node.key * Node.value
       and type commit = Commit.key * Commit.value

  (** Private repositories. *)
  module Repo : sig
    type t

    include OF_CONFIG with type _ t := t
    (** @inline *)

    include CLOSEABLE with type _ t := t
    (** @inline *)

    val contents_t : t -> read Contents.t
    val node_t : t -> read Node.t
    val commit_t : t -> read Commit.t
    val branch_t : t -> Branch.t

    val batch :
      t ->
      (read_write Contents.t ->
      read_write Node.t ->
      read_write Commit.t ->
      'a Lwt.t) ->
      'a Lwt.t
  end

  (** URI-based low-level sync. *)
  module Sync : sig
    include Sync.S with type commit = Commit.key and type branch = Branch.key

    val v : Repo.t -> t Lwt.t
  end
end
