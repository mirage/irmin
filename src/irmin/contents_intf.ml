open S

module type S = sig
  (** {1 Signature for store contents} *)

  type t [@@deriving irmin]
  (** The type for user-defined contents. *)

  val merge : t option Merge.t
  (** Merge function. Evaluates to [`Conflict msg] if the values cannot be
      merged properly. The arguments of the merge function can take [None] to
      mean that the key does not exists for either the least-common ancestor or
      one of the two merging points. The merge function returns [None] when the
      key's value should be deleted. *)
end

module type STORE = sig
  include CONTENT_ADDRESSABLE_STORE

  val merge : [ `Read | `Write ] t -> key option Merge.t
  (** [merge t] lifts the merge functions defined on contents values to contents
      key. The merge function will: {e (i)} read the values associated with the
      given keys, {e (ii)} use the merge function defined on values and
      {e (iii)} write the resulting values into the store to get the resulting
      key. See {!Contents.S.merge}.

      If any of these operations fail, return [`Conflict]. *)

  (** [Key] provides base functions for user-defined contents keys. *)
  module Key : TYPED_HASH with type t = key and type value = value

  module Val : S with type t = value
  (** [Val] provides base functions for user-defined contents values. *)
end

module type Contents = sig
  module type S = S
  module type STORE = STORE

  type json =
    [ `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `O of (string * json) list
    | `A of json list ]

  module String : S with type t = string
  module Json : S with type t = (string * json) list
  module Json_value : S with type t = json

  module V1 : sig
    module String : S with type t = string
  end

  module Store (C : sig
    include S.CONTENT_ADDRESSABLE_STORE
    module Key : S.HASH with type t = key
    module Val : S with type t = value
  end) :
    STORE with type 'a t = 'a C.t and type key = C.key and type value = C.value
end