(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** A library database following the same design principle as Git.

    Irmin is a distributed and history-preserving library database
    with built-in snapshot, branch and revert mechanisms. It is
    designed to use a large variety of backends. Irmin is written in
    pure OCaml and does not depend on external C stubs; it aims is to
    run everywhere, from Linux to Xen unikernels -- and can be be
    compiled to JavaScipt to run in a browser.

    FIXME
*)

(** {1 Merge operators} *)

module Merge: sig

  (** 3-way merge combinators.*)

  (** {1 Merge Results} *)

  type 'a result = [ `Ok of 'a | `Conflict of string ]
  (** Type for merge results. *)

  module Result: Tc.I1 with type 'a t = 'a result
  (** Base functions over results. *)

  val bind: 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
  (** Monadic bind over Result. *)

  exception Conflict of string
  (** Exception which might be raised when merging.  *)

  val exn: 'a result -> 'a Lwt.t
  (** Convert [`Conflict] results to [Conflict] exceptions. *)

  (** {1 Merge Combinators} *)

  type ('a, 'o) t = 'o -> old:'a -> 'a -> 'a -> 'a result Lwt.t
  (** Signature of a merge function. ['o] denotes the type for
      tracking change origins.

              /----> t1 ----\
      ----> old              |--> result
              \----> t2 ----/
  *)

  module type S = Tc.I0

  type 'a elt = (module S with type t = 'a)
  (** The type for mergeable contents of type ['a]. *)

  val default: 'a elt -> ('a, 'o) t
  (** Create a default merge function. This is a simple merge
      functions which support changes in one branch at the time:

      - if t1=t2  then return t1
      - if t1=old then return t2
      - if t2=old then return t1
      - otherwise raise [Conflict].
  *)

  val string: (string, 'o) t
  (** The default string merge function. Do not anything clever, just
      compare the strings using the [default] merge function. *)

  val counter: (int, 'o) t
  (** Mergeable counters. *)

  val seq: ('a, 'o) t list -> ('a, 'o) t
  (** Try the merge operations in sequence. *)

  val some: 'a elt -> ('a, 'o) t -> ('a option, 'o) t
  (** Lift a merge function to optional values of the same type. If all
      the provided values are inhabited, then call the provided merge
      function, otherwise use the same behavior as [create]. *)

  val alist: 'a elt -> 'b elt -> ('b, 'o) t -> ( ('a * 'b) list, 'o) t
  (** List to association lists. *)

  module Map (X: S): sig
    val merge: ('a elt) -> ('a, 'o) t -> ('a Map.Make(X).t, 'o) t
  end
  (** Lift to string maps. *)

  val pair: 'a elt -> 'b elt -> ('a, 'o) t -> ('b, 'o) t -> ('a * 'b, 'o) t
  (** Lift to pairs. *)

  val biject: 'a elt -> 'b elt ->
    ('a, 'o) t -> ('a -> 'b) -> ('b -> 'a) -> ('b, 'o) t
  (** Use the merge function defined in another domain. If the
      functions given in argument are partial (ie. returning
      [Not_found] on some entries), the exception is catched and
      [Conflict] is returned instead. *)

  val biject': 'a elt -> 'b elt ->
    ('a, 'o) t -> ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> ('b, 'o) t
  (** Same as [map] but with potentially blocking converting
      functions. *)

  val apply: ('a -> ('b, 'o) t) -> 'a -> ('b, 'o) t
  (** [apply] combinator. Usefull to untie recursive loops. *)

  module OP: sig

    (** Default operators. *)

    (** Use [open Irmin.Merge.OP] at the top of your file to use
        them. *)

    val ok: 'a -> 'a result Lwt.t
    (** Return [`Ok x]. *)

    val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
    (** Return [`Conflict str]. *)

    val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
    (** Same as [bind]. *)

  end

end

(** {1 Track read and write origins} *)

module Origin: sig

  module type S = sig

    (** Value keeping track of database accesses and
        updates. *)

    include Tc.I0
    (** The type for origin values. Every operation origin has a date,
        a text message and an name identifying the entity performing
        that operation. Origins are threaded from high-level calls to
        all low-level database operations, so they can use for
        encryption and access-control. FIXME *)

    val create: ?date:int64 -> ?id:string -> ('a, unit, string, t) format4 -> 'a
    (** Create a new provenance message. *)

    val date: t -> int64
    (** Get the origin date. *)

    val pretty_date: t -> string
    (** Get the date as a pretty string. *)

    val id: t -> string
    (** Get the origin ID. *)

    val message: t -> string
    (** Get the origin message. *)

  end

  module type P = sig

    (** Signature for building origin values. *)

    val date: unit -> int64
    (** [date ()] is the current date. *)

    val id: unit -> string
    (** [id ()] is a string identifying the current process. *)

    val string_of_date: int64 -> string
    (** [string_of_date d] is a nicely formatted string representation
        of the date [d]. *)

  end

  module Make (P: P): S
  (** Build an implementation of origin values from a an
      implementation of origin parameters. *)

  module Default: S
  (** The default origin, where [date] is an incremented counter and
      [id] is a random number. *)

end

(** {1 User-defined contents} *)

module Contents: sig

  module type S = sig

    (** Signature for store contents. *)

    include Tc.I0

    type origin
    (** Type for origin of merges. *)

    module Origin: Origin.S with type t = origin
    (** Base functions for origins. *)

    val merge: (t, origin) Merge.t
    (** Merge function. Evaluates to [`Conflict] if the values cannot be
        merged properly. *)

  end

  module String (O: Origin.S): S with type t = string and type origin = O.t
  (** String values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)

  module Json (O: Origin.S): S with type t = Ezjsonm.t and type origin = O.t
  (** JSON values where only the last modified value is kept on
      merge. If the value has been modified concurrently, the [merge]
      function raises [Conflict]. *)


  module Cstruct (O: Origin.S): S with type t = Cstruct.t
  (** Cstruct values where only the last modified value is kept on
      merge. If the value has been modified concurrently, then this is a
      conflict. *)

end

(** {2 User-defined unique identifiers (digests)} *)

module Hash: sig

  module type S = sig

    (** Signature for unique identifiers. *)

    include Tc.I0

    val digest: Cstruct.t -> t
    (** Compute a (deterministic) key from a cstruct and an [origin]
        information. *)

    (** FIXME: add hmac: key:origin -> Cstuct.t -> Cstruct.t ? *)

  end

  module SHA1: S
  (** SHA1 digests *)

end

(** {2 Stores} *)

module Store: sig

  (** {2 Read-only Stores} *)

  module type RO = sig

    (** Read-only stores. *)

    type t
    (** Type for stores. *)

    type key
    (** Type for keys. *)

    type value
    (** Type for values. *)

    type origin
    (** Type for origin tracking. *)

    val create: unit -> t
    (** Create a store handle. The operation can be used multiple times
        as it is supposed to be very cheap (and usually
        non-blocking). *)

    val read: t -> origin -> key -> value option Lwt.t
    (** Read a value from the store. *)

    val read_exn: t -> origin -> key -> value Lwt.t
    (** Same as [read] but raise [Not_found] if the key does not
        exist. *)

    val mem: t -> origin -> key -> bool Lwt.t
    (** Check if a key exists. *)

    val list: t -> origin -> key list -> key list Lwt.t
    (** Return all the keys that [origin] are allowed to access,
        knowing a given collection of {i root} keys. *)

    val dump: t -> origin -> (key * value) list Lwt.t
    (** Return the store contents. *)

  end

  (** {2 Append-only store} *)

  module type AO = sig

    (** Signature for append-only stores. *)

    include RO

    val add: t -> origin -> value -> key Lwt.t
    (** Write the contents of a value to the store. That's the
        responsibility of the append-only store to generate a
        consistent key. *)

  end

  (** {2 Mutable store} *)

  module type RW = sig

    (** Mutable store. *)

    include RO

    val update: t -> origin -> key -> value -> unit Lwt.t
    (** Replace the contents of [key] by [value] if [key] is already
        defined and create it otherwise. *)

    val remove: t -> origin -> key -> unit Lwt.t
    (** Remove the given key. *)

    val watch: t -> origin -> key -> value Lwt_stream.t
    (** Watch a given key. *)

  end

  (** {2 Branch-consistent store} *)

  module type BC = sig

    (** A branch-consistent store is a mutable store which supports
        fork/join operations. *)

    include RW

    (** {2 Tags} *)

    type tag
    (** Type of branch tags. *)

    val of_tag: tag -> t
    (** Create a store handle. Similar to [create], but use any tag name
        instead of the [master] tag. *)

    val tag: t -> tag option
    (** Return the branch of the given store handle. *)

    val tag_exn: t -> tag
    (** Same as [tag] but raise [Not_found] in case of a detached
        head. *)

    val update_tag: t -> origin -> tag -> [`Ok | `Duplicated_tag] Lwt.t
    (** Change the current tag name. Fail if a tag with the same name
        already exists. The head is unchanged. *)

    val update_tag_force: t -> origin -> tag -> unit Lwt.t
    (** Same as [update_tag] but delete and update the tag if it already
        exists. *)

    val detach: t -> origin -> unit Lwt.t
    (** Detach the current branch (ie. it is not assiaciated to a tag
        anymore). *)

    (** {2 Heads} *)

    type head
    (** Type for head values. *)

    val of_head: head -> t
    (** Create a temporary detached branch, which will not persist in
        the database as it has no associated persistent tag name. *)

    val head: t -> origin -> head option Lwt.t
    (** Return the head commit. Might block if the branch is persistent
        as it needs to lookup some tag contents. *)

    val head_exn: t -> origin -> head Lwt.t
    (** Same as [read_head] but raise [Not_found] if the commit does not
        exist. *)

    val heads: t -> origin -> head list Lwt.t
    (** The list of all the databse heads. *)

    val update_head: t -> origin -> head -> unit Lwt.t
    (** Set the commit head. *)

    val merge_head: t -> origin -> head -> unit Merge.result Lwt.t
    (** Merge a commit with the current branch. *)

    val watch_head: t -> origin -> key -> (key * head) Lwt_stream.t
    (** Watch changes for given key and the one it has recursive access.
        Return the stream of heads of the modified keys. *)

    (** {2 Functions over stores} *)

    val clone: t -> origin -> tag -> [`Ok of t | `Duplicated_tag] Lwt.t
    (** Fork the store, using the given branch name. Return [None] if
        the branch already exists. *)

    val clone_force: t -> origin -> tag -> t Lwt.t
    (** Same as [clone] but delete and update the existing branch if a
        branch with the same name already exists. *)

    val switch: t -> origin -> tag -> unit Lwt.t
    (** Switch the database contents the be same as the contents of the
        given branch name. The two branches are still independant. *)

    val merge: t -> origin -> tag -> unit Merge.result Lwt.t
    (** [merge db t] merges the branch [t] into the current database
        branch. The two branches are still independant. *)

    module T: Tc.I0 with type t = t
    (** Base functions over values of type [t]. *)

    (** {2 Slices} *)

    type slice
    (** Type for database slices. *)

    module Slice: Tc.I0 with type t = slice
    (** Base functions over slices. *)

    val export: ?full:bool -> ?depth:int -> ?min:head list -> ?max:head list ->
      t -> origin -> slice Lwt.t
    (** [export t origin ~depth ~min ~max] exports the database slice
        between [min] and [max], using at most [depth] history depth
        (starting from the max).

        If [max] is not specified, use the current [heads]. If [min] is
        not specified, use an unbound past (but can be still limited by
        [depth]).

        [depth] is used to limit the depth of the commit history. [None]
        here means no limitation.

        If [full] is set (default is true) the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the
        commit history graph only. *)

    val import: t -> origin -> slice -> [`Ok | `Duplicated_tags of tag list] Lwt.t
    (** Import a database slide. Do not modify existing tags. *)

    val import_force: t -> origin -> slice -> unit Lwt.t
    (** Same as [import] but delete and update the tags they already
        exist in the database. *)

  end

end

(** {2 Views} *)

(** Views are in-memory partial views of the database, with lazy reads
    and delayed write.

    Views are like staging area in Git: they are temporary
    non-persistent areas (they disapear if the host crash), hold in
    memory for efficiency, where reads are done lazily and writes are
    done only when needed on commit: if if you modify a key twice,
    only the last change will be written to the database when you
    commit. Views also hold a list of operations, which are checked
    for conflicts on commits and are used to replay/rebase the view if
    needed. The most important feature of views is that they keep
    track of reads: ie. you can have a conflict if a view reads a key
    which has been modified concurrently by someone else.

*)

module View: sig

  module type ACTION = sig
    type path
    type contents
    type t =
      [ `Read of (path * contents option)
      | `Write of (path * contents option)
      | `List of (path list * path list) ]
    (** Operations on view. We record the result of reads to be able to
        replay them on merge. *)

    include Tc.I0 with type t := t

    val pretty: t -> string
    (** Pretty-print an action. *)
  end

  module type S = sig

    (** Signature for views independant of any database substrate. *)

    type step

    include Store.RW with type key = step list

    type action
    (** The type for actions. *)

    val actions: t -> action list
    (** Return the list of actions performed on this view since its
        creation. *)

    val merge: t -> origin -> into:t -> unit Merge.result Lwt.t
    (** Merge the actions done on one view into an other one. If a read
        operation doesn't return the same result, return
        [Conflict]. Only the [into] view is updated. *)

    module Action: ACTION
      with type path = key
       and type contents = value
       (** Base functions over actions. *)

  end

  module Make (S: Tc.I0) (V: Tc.I0) (O: Tc.I0): S
    with type step = S.t
     and type value = V.t
     and type origin = O.t
  (** Create a view implementation independant of any underlying
      store. *)

end

(** {1 Irmin Stores} *)

module type S = sig

  (** TODO: doc *)

  type step

  include Store.BC with type key = step list

  (** {1 Views} *)

  module View: sig

    (** Database temporary and in-memory views. Similar to [View.S]
        but can lazily be created from a path in the database, and
        later commited back. *)

    type db = t
    (** Database handler. *)

    include View.S with type origin = origin
                    and type step = step
                    and type value = value

    val origin_of_actions: t -> origin
    (** Create an origin using the list of actions as message. *)

    val of_path: db -> origin -> key -> t Lwt.t
    (** Read a view from a path in the store. This is a cheap operation,
        all the real reads operation will be done on-demand when the
        view is used. *)

    val update_path: db -> origin -> key -> t -> unit Lwt.t
    (** Commit a view to the store. The view *replaces* the current
        subtree, so if you want to do a merge, you have to do it
        manually (by creating a new branch, or rebasing before
        commiting). [origin] helps keeping track of provenance. *)

    val rebase_path: db -> origin -> key -> t -> unit Merge.result Lwt.t
    (** Rebase the view to the tip of the store. *)

    val merge_path: db -> origin -> key -> t -> unit Merge.result Lwt.t
  (** Same as [update_path] but *merges* with the current subtree. *)

  end

  (** {1 Snapshots} *)

  module Snapshot: sig

    (** Snapshots are read-only checkpoints of the dabase. *)

    type db = t
    (** Type for database handler. *)

    include Store.RO with type origin = origin
                      and type key = key
                      and type value = value

    val create: db -> origin -> t Lwt.t
    (** Snapshot the current state of the store. *)

    val revert: db -> origin -> t -> unit Lwt.t
    (** Revert the store to a previous state. *)

    val merge: db -> origin -> t -> unit Merge.result Lwt.t
    (** Merge the given snasphot into the current branch of the
        database. *)

    val watch: db -> origin -> key -> (key * t) Lwt_stream.t
    (** Subscribe to the stream of modification events attached to a
        given path. Takes and returns a new snapshot every time a
        sub-path is modified. *)

  end

  (** {1 Dumps} *)

  module Dump: sig

    (** Import/export capabilities. *)

    type db = t
    (** The type for databse handler. *)

    type origin = View.origin
    (** The type to track origins. *)

    val output_buffer:
      db -> origin -> ?html:bool -> ?depth:int -> ?full:bool ->
      Buffer.t -> unit Lwt.t
    (** [output_buffer t ?html ?depth ?full buf] outputs the Graphviz
        representation of [t] in the buffer [buf].

        [html] (default is false) enables HTML labels.

        [depth] is used to limit the depth of the commit history. [None]
        here means no limitation.

        If [full] is set (default is not) the full graph, including the
        commits, nodes and contents, is exported, otherwise it is the
        commit history graph only. *)

  end

end

(*
(** {2 Backends} *)

module Backend: sig

  (** {2 Basic Store Makers} *)

  module Maker: sig

    module type RO =
      functor (K: Tc.I0) ->
      functor (V: Tc.I0) ->
        Store.RO with type key = K.t and type value = V.t
    (** Signature for functor creating read-only stores. *)

  end

  (** {2 Binary stores} *)

  module Binary: sig

    module type RO = Store.RO with type key = Cstruct.t and type value = Cstruct.t
    (** Binary read-only stores. Keys, values and origin are cstruct
        buffers. *)

    module RO (S: RO) (K: Tc.I0) (V: Tc.I0): Maker.RO
    (** Create a typed read-only store from a binary one. *)

  end

  (** {2 JSON stores} *)

  module Json: sig

    module type RO = Store.RO with type key = Ezjsonm.t and type value = Ezjsonm.t
    (** Binary read-only stores. Keys, values and origin are cstruct
        buffers. *)

    module RO (S: RO) (K: Tc.I0) (V: Tc.I0): Maker.RO
    (** Create a typed read-only store from a JSON one. *)

  end

end
*)
