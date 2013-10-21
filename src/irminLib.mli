
module Tree = struct

  type t
  (** Type of trees. *)

  val empty: t
  (** An empty tree. *)

  val create: ?value:value -> (label * t) list -> t
  (** Create a new trie with the given components *)

  val value: t -> value option
  (** Get the value carried by the node. *)

  val mem: t -> label list -> bool
  (** Returns true if there is a value associated with the given path *)

  val find: t -> label list -> value
  (** Returns the value associated with the given path.  @raise
      [Not_found] *)

  val set: t -> label list -> value -> t
  (** Associates a value with the given path, or replaces if there
      was already one *)

  val unset: t -> label list -> t
  (** Removes all associations to a given key from the
      trie. Warning: doesn't cleanup branches that don't point to
      anything anymore *)

  val iter: (label list -> value -> unit) -> t -> unit
  (** iters over all the bindings in the trie, top-down *)

  val fold: ('acc -> label list -> value -> 'acc) -> t -> 'acc -> 'acc
  (** folds over all bindings of the trie, bottom-up *)

  val sub: t -> label list -> t
  (** [sub t p] returns the sub-trie associated with the path [p] in
      the trie [t].  If [p] is not a valid path of [t], it returns
      an empty trie. *)

  val graft: t -> label list -> t -> t
  (** [graft tree path subtree] grafts the children of [subtree] in
      [tree] at [path], replacing the whole subtree *)

  val merge : ?value:(value -> value -> value) -> t -> t -> t
  (** Merges two tries, accepting an optional function to resolve
      value conflicts. The default function pushes right-hand values
      on top of left-hand ones. By default, use the default merge
      function defined over values. *)

  val append : t -> (label list * t) -> t
  (** [append tree (path, subtree)] appends [subtree] in [tree] at
      [path], merging with the previous subtree of [tree]. The
      interface allows for multiple appends with a simple
      [List.fold_left] *)

  val of_node: (key -> value option) -> (key -> Node.t option) -> Node.t -> t
  (** Build a full tree from a node. *)

  val to_node: t -> Node.t list
  (** Convert a tree into a list of raw nodes. *)

end
