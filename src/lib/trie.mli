(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

(** Lazy tries based on lists *)

type ('a, 'b) t
(** Type of tries mapping from ['a list] to ['b] *)

val empty : ('a,'b) t

val create :
  ?children: ('a * ('a,'b) t) list Lazy.t ->
  ?value: 'b ->
  unit ->
  ('a,'b) t
(** Create a new trie with the given components *)

val children: ('a, 'b) t -> ('a * ('a, 'b) t) list
(** Return the list of children *)

val mem : ('a,'b) t -> 'a list -> bool
(** Returns true if there is a value associated with the given path *)

val find : ('a, 'b) t -> 'a list -> 'b
(** Returns the value associated with the given path.
    @raise [Not_found] *)

val find_all : ('a, 'b) t -> 'a list -> 'b list
(** Returns all values associated with the given path, most recent first. *)

val set : ('a, 'b) t -> 'a list -> 'b -> ('a, 'b) t
(** Associates a value with the given path, or replaces if there was already
    one *)

val add : ('a, 'b) t -> 'a list -> 'b -> ('a, 'b) t
(** Associates a value with the given path, keeping previous bindings *)

val set_lazy : ('a, 'b) t -> 'a list -> 'b Lazy.t -> ('a, 'b) t
(** The same but taking a lazy value *)

val unset : ('a, 'b) t -> 'a list -> ('a, 'b) t
(** Removes an association from the trie. Warning: doesn't cleanup branches that
    don't point to anything anymore *)

val map_subtree :
  ('a, 'b) t -> 'a list ->
  (('a, 'b) t -> ('a, 'b) t) ->
  ('a, 'b) t
(** [map_subtree tree path f] applies [f] on value and children of the node
    found at [path] in [tree], and bind the returned node back at that position
    in the tree *)

val iter : ('a list -> 'b -> unit) -> ('a, 'b) t -> unit
(** iters over all the bindings in the trie, top-down *)

val fold : ('acc -> 'a list -> 'b -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
(** folds over all bindings of the trie, bottom-up *)

val fold0 : ('acc -> 'a list -> 'b list -> 'acc) -> ('a, 'b) t -> 'acc -> 'acc
(** same as [fold], but the list of bindings at a given path is given at once *)

val map_filter_values : ('b -> 'c option) -> ('a,'b) t -> ('a,'c) t
(** Maps and filters over all values in the trie, removing the value if [None]
    is returned *)

val sub : ('a, 'b) t -> 'a list -> ('a,'b) t
(** [sub t p] returns the sub-trie associated with the path [p] in the trie
    [t].  If [p] is not a valid path of [t], it returns an empty trie. *)

val filter_keys : ('a -> bool) -> ('a, 'b) t -> ('a, 'b) t
(** [filter f t] returns t with all subtrees for which [f key = false] pruned *)

val graft : ('a, 'b) t -> 'a list -> ('a, 'b) t -> ('a, 'b) t
(** [graft tree path subtree] grafts the children of [subtree] in [tree] at
    [path], replacing the whole subtree *)

val graft_lazy : ('a, 'b) t -> 'a list -> ('a, 'b) t Lazy.t -> ('a, 'b) t
(** Lazy version of [graft] *)

val merge :
  ?values:('b list -> 'b list -> 'b list) -> ('a, 'b) t -> ('a, 'b) t
  -> ('a, 'b) t
(** Merges two tries, accepting an optional function to resolve value
    conflicts. The default function pushes right-hand values. on top of
    left-hand ones *)

val append : ('a, 'b) t -> ('a list * ('a, 'b) t) -> ('a, 'b) t
(** [append tree (path, subtree)] appends [subtree] in [tree] at [path], merging
    with the previous subtree of [tree]. The interface allows for multiple
    appends with a simple [List.fold_left] *)
