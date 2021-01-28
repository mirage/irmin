open S

module type S = sig
  (** {1 Signature for Branches} *)

  type t [@@deriving irmin]
  (** The type for branches. *)

  val master : t
  (** The name of the master branch. *)

  val is_valid : t -> bool
  (** Check if the branch is valid. *)
end

module type STORE = sig
  (** {1 Branch Store} *)

  include ATOMIC_WRITE_STORE

  module Key : S with type t = key
  (** Base functions on keys. *)

  module Val : Hash.S with type t = value
  (** Base functions on values. *)
end

module type Branch = sig
  (** {1 Branches} *)

  module type S = S
  (** The signature for branches. Irmin branches are similar to Git branches:
      they are used to associated user-defined names to head commits. Branches
      have a default value: the {{!Branch.S.master} master} branch. *)

  module String : S with type t = string
  (** [String] is an implementation of {{!Branch.S} S} where branches are
      strings. The [master] branch is ["master"]. Valid branch names contain
      only alpha-numeric characters, [-], [_], [.], and [/]. *)

  module type STORE = STORE
  (** [STORE] specifies the signature for branch stores.

      A {i branch store} is a mutable and reactive key / value store, where keys
      are branch names created by users and values are keys are head commmits. *)
end
