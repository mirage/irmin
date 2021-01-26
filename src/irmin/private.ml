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

    val v : Conf.t -> t Lwt.t
    val close : t -> unit Lwt.t
    val contents_t : t -> [ `Read ] Contents.t
    val node_t : t -> [ `Read ] Node.t
    val commit_t : t -> [ `Read ] Commit.t
    val branch_t : t -> Branch.t

    val batch :
      t ->
      ([ `Read | `Write ] Contents.t ->
      [ `Read | `Write ] Node.t ->
      [ `Read | `Write ] Commit.t ->
      'a Lwt.t) ->
      'a Lwt.t
  end

  (** URI-based low-level sync. *)
  module Sync : sig
    include S.SYNC with type commit = Commit.key and type branch = Branch.key

    val v : Repo.t -> t Lwt.t
  end
end
