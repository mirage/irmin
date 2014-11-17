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

    Irmin uses a set of {{!Store}store signatures} describing what
    {{!RO}read-only}, {{!AO}append-only}, {{!RW}read-write} stores are
    and {{!Backend}backend functor} generating these signature,
    providing the proper store contents.

*)

module Merge: sig

  (** Merge API. *)

  type 'a result = [ `Ok of 'a | `Conflict of string ]
  (** Type for merge results. *)

  type 'a t = old:'a -> 'a -> 'a -> 'a result Lwt.t
  (** User-defined nerge functions. *)

  val bind: 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
  (** Monadic bind over Result. *)

  exception Conflict of string
  (** Exception which might be raised when merging.  *)

  val exn: 'a result -> 'a Lwt.t
  (** Convert [`Conflict] results to [Conflict] exceptions. *)

  module Infix: sig

    val ok: 'a -> 'a result Lwt.t
    (** Return [`Ok x]. *)

    val conflict: ('a, unit, string, 'b result Lwt.t) format4 -> 'a
    (** Return [`Conflict str]. *)

    val (>>|): 'a result Lwt.t -> ('a -> 'b result Lwt.t) -> 'b result Lwt.t
    (** Same as [bind]. *)

  end

end

(** {2 Track read and write origins} *)

module Origin: sig

  (** Simple origin tracking, where every operations has a date, a
      text message and an name identifying the entity performing the
      operation. *)

  type t
  (** Provenance values. *)

  val create: ?date:int64 -> ?id:string -> ('a, unit, string, t) format4 -> 'a
  (** Create a new provenance message. *)

  val date: t -> int64
  (** Get the origin date. *)

  val id: t -> string
  (** Get the origin ID. *)

  val message: t -> string
  (** Get the origin message. *)

end

type origin = Origin.t
(** Type for tracking change provenance.
    XXX: we might want to abstract that. *)

type path = string list
(** Type for database paths.
    XXX: We might want to abstract that: [type step and path = step list]*)

(** {2 User-defined contents} *)

module type CONTENTS = sig

  (** Signature for store contents. *)

  include Tc.I0

  val merge: origin -> t Merge.t
  (** Merge function for user-defined contents. *)

end

module String: CONTENTS with type t = string
(** String values where only the last modified value is kept on
    merge. If the value has been modified concurrently, then this is a
    conflict. *)

module Json: CONTENTS with type t = Ezjsonm.t
(** JSON values where only the last modified value is kept on
    merge. If the value has been modified concurrently, then this is a
    conflict. *)

module Cstruct: CONTENTS with type t = Cstruct.t
(** Cstruct values where only the last modified value is kept on
    merge. If the value has been modified concurrently, then this is a
    conflict. *)

(** {2 User-defined unique identifiers (digests)} *)

module type UID = sig

  (** Signature for unique identifiers. *)

  include Tc.I0

  val digest: Cstruct.t -> t
  (** Compute a (deterministic) key from a cstruct. *)

end

module SHA1: UID
(** SHA1 digests *)

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

    val create: unit -> t Lwt.t
    (** Create a store handle. The operation can be used multiple times
        as it is supposed to be very cheap (and usually
        non-blocking). *)

    val read: t -> origin -> key -> [`Ok of value | `Not_found] Lwt.t
    (** Read a value from the store. *)

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

    val add: t -> value -> key Lwt.t
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
        fork/join operations of branches. *)

    include RW

    type branch
    (** Type for branch names. *)

    val of_branch: branch -> t Lwt.t
    (** Create a store handle from a branch name. [create] is similar
        but it uses the [master] branch. If the branch name does not
        already exists, create an empty branch. *)

    val branch: t -> origin -> [`Ok of branch | `Detached] Lwt.t
    (** Return the branch name of the given store handle. Return
        [Detached] if the branch does not have a name. *)

    val attach: t -> origin -> branch -> [`Ok | `Branch_in_use] Lwt.t
    (** [attach t origin b] attaches the branch name [b] to the
        current branch of [t]. *)

    val attach_force: t -> origin -> branch -> unit Lwt.t
    (** Same as [attach] but delete and update the the existing branch
        if a branch with the same name already exists. *)

    val detach: t -> origin -> unit Lwt.t
    (** Detach the current branch. It is not assiaciated with a branch
        name anymore but it can continue to be used as a normal (but
        anonymous) branch. Subsequent calls to [branch] will return
        [`Detached]. *)

    val clone: t -> origin -> branch -> [`Ok of t | `Branch_in_use] Lwt.t
    (** [clone t origin b] is a fork the store [t], with [b] as the
        new store's current branch name. Return [Branch_in_use] if the
        branch already exists. *)

    val clone_force: t -> origin -> branch -> t Lwt.t
    (** Same as [clone] but delete and update the existing branch if a
        branch with the same name already exists. *)

    val update_branch: t -> origin -> branch -> unit Lwt.t
    (** [update_branch t o b] updates the current branch of [t] to
        have the same contents as [b]. The two branches are still
        independant. *)

    val merge_branch: t -> origin -> branch -> unit Merge.result Lwt.t
    (** [merge db t] merges the branch [t] into the current database
        branch. The two branches are still independant. *)

    (** {2 Anonymous branches} *)

    type head
    (** Type for head values. *)

    val of_head: head -> t Lwt.t
    (** Create a temporary anonymous branch, which will not have an
        associated branch name. *)

    val head: t -> origin -> [`Ok of head | `Not_found] Lwt.t
    (** Return the name of the head commit. *)

    val update_head: t -> origin -> head -> unit Lwt.t
    (** Set the commit head. *)

    val merge_head: t -> origin -> head -> unit Merge.result Lwt.t
    (** Merge a commit with the current branch. *)

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

  type 'a action =
    [ `Read of path * 'a option
    | `Write of path * 'a option
    | `List of path list * path list ]
  (** Operations on view. We record the result of reads to be able to
      replay them on merge. *)

  module type S = sig

    (** Signature for views independant of any database
        implementation. View are tree-like datastructure: keys are
        paths in the database and tree nodes can contains some
        contents. *)

    include Store.RW with type key = path

    val actions: t -> value action list
    (** Return the list of actions performed on this view since its
        creation. *)

    val merge: t -> into:t -> unit Merge.result Lwt.t
    (** Merge the actions done on one view into an other one. If a
        read operation doesn't return the same result, return
        [Conflict]. Only the [into] view is updated. *)

  end

  module Make (C: CONTENTS): S with type value = C.t
  (** Create a view implementation independant of any underlying
      store. *)

end

module type VIEW = sig

  (** Signature for views which are tied to a given database
      implementation. *)

    include View.S

    type db
    (** Database handler. *)

    val of_path: db -> path -> t Lwt.t
    (** Read a view from a path in the store. This is a cheap
        operation, all the real reads operation will be done on-demand
        when the view is used. *)

    val update_path: db -> origin -> path -> t -> unit Lwt.t
    (** Commit a view to the store. The view *replaces* the current
        subtree. *)

    val rebase_path: db -> origin -> path -> t -> unit Merge.result Lwt.t
    (** [rebase t o p v] rebases the view [v] on top of the tip of the
        store [t], at the path [p]. Rebasing means applying all
        operations applied in [v]: all writes are replayed directly
        and reads are checked for consistency. If a read returns a
        different result than in the view, then the rebase is a
        conflict. *)

    val merge_path: db -> origin -> path -> t -> unit Merge.result Lwt.t
    (** Same as [update_path] and [rebase_path] but *merges* the view
        with the current subtree. All concurrently modified contents
        will try to be merged using the user-provided merge
        function. *)

  end

(** {2 Slices} *)

  (** Slices are used to import/export part of a store. They are
      abstract objects, which can be translated to and from a binary
      format or JSON. *)

module type SLICE = sig

  include Tc.I0

  type db
  (** Databse handler. *)

  type head
  (** Type for head values. *)

  val export: db -> min:head list -> max:head list -> t Lwt.t
  (** Export a slice. *)

  val import: db -> t -> unit Lwt.t
  (** Import a slice. *)

end

(** {2 Irmin Stores} *)

module type S = sig

  (** TODO: doc *)

  include Store.BC
  module View: VIEW with type db = t
  module Slice: SLICE with type db = t and type head = head

end

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
