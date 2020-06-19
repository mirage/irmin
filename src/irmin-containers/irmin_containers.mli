(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
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

(** Irmin_containers public API

    [Irmin_containers] is a collection of simple, ready-to-use mergeable data
    structures. Each data structure is customisable according to the user's
    needs with regards to the type of backend being used and its accompanying
    parameters. Along with that, [Irmin_containers] also provides data
    structures with suitable instantiations performed using two backends: the
    {{!Irmin_mem} in-memory backend} provided by [Irmin_mem] and the
    {{!Irmin_unix.FS} FS backend} provided by [Irmin_unix]. *)

(** {1 Auxiliary signatures and modules} *)

module type Store_maker = Stores.Store_maker
(** {2 Store_maker}

    [Store_maker] is the signature for the backend input to the data structures.
    The Irmin stores of the data structures are constructed using modules of
    this type *)

module type Cas_maker = Stores.Cas_maker
(** {2 Cas_maker}

    [Cas_maker] is the signature for the store which will be used to maintain
    linked data structures. The elements are hashed into this store and the hash
    value is used to construct the linkages. *)

module Time = Time
(** {2 Time}

    [Time] specifies the method to obtain timestamps for the values to be
    stored. It is necessary for the timestamps to be monotonic for the data
    structures to function properly. *)

(** {1 Data structures}*)

(** {2 Counter}

    [Counter] is the implementation of the [int64] counter. This module supports
    operations to increment, decrement and read the value of the counter. *)
module Counter : sig
  (** Counter signature *)
  module type S = sig
    include Counter.S
    (** @inline *)
  end

  (** Constructor for counter *)
  module Make (Backend : Store_maker) :
    S
      with type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** Counter instantiated using the {{!Irmin_unix.FS} FS backend} provided by
      [Irmin_unix] *)
  module FS :
    S
      with type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** Counter instantiated using the {{!Irmin_mem} in-memory backend} provided
      by [Irmin_mem] *)
  module Mem :
    S
      with type Store.branch = string
       and type Store.key = string list
       and type Store.step = string
end

(** {2 Last-write-wins register}

    [Lww_register] is the implementation of the last-write-wins register. The
    type of value to be stored in the register as well as the method to obtain
    timestamps are provided by the user. This module supports reading and
    writing to the register. *)
module Lww_register : sig
  (** Lww_register signature *)
  module type S = sig
    include Lww_register.S
    (** @inline *)
  end

  (** Constructor for last-write-wins register *)
  module Make (Backend : Store_maker) (T : Time.S) (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** LWW register instantiated using the {{!Irmin_unix.FS} FS backend} provided
      by [Irmin_unix] and the timestamp method {!Time.Unix} *)
  module FS (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** LWW register instantiated using the {{!Irmin_mem} in-memory backend}
      provided by [Irmin_mem] and the timestamp method {!Time.Unix} *)
  module Mem (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string
end

(** {2 Blob log}

    [Blob_log] is the implementation of log in which it is maintained as a
    single unit, or blob. Hence, two versions of the log cannot share their
    common predecessor. The type of values to be stored as well as a method to
    obtain timestamps are provided by the user. The blob log supports appending
    an entry into the log and reading the entire log. *)
module Blob_log : sig
  (** Signature of the blob log *)
  module type S = sig
    include Blob_log.S
    (** @inline *)
  end

  (** Constructor for blob log *)
  module Make (Backend : Store_maker) (T : Time.S) (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** Blob log instantiated using the {{!Irmin_unix.FS} FS backend} provided by
      [Irmin_unix] and the timestamp method {!Time.Unix} *)
  module FS (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** Blob log instantiated using the {{!Irmin_mem} in-memory backend} provided
      by [Irmin_mem] and the timestamp method {!Time.Unix} *)
  module Mem (V : Irmin.Type.S) :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string
end

(** {2 Linked log}

    [Linked_log] is the linked list implementation of log. Due to the linked
    property, two versions of the log share their common predecessor. As it is a
    linked data structure, a content addressable store of type {!Cas_maker} is
    required. Along with that, a method to obtain timestamps, a hash for the
    content addressable store and the type of values stored must also be
    provided. The linked log supports appending an entry into the log, getting a
    cursor, reading a certain number of elements from the cursor and reading the
    entire log. *)
module Linked_log : sig
  (** [Linked_log] signature *)
  module type S = sig
    include Linked_log.S
    (** @inline *)
  end

  (** Constructor for linked log *)
  module Make
      (Backend : Store_maker)
      (C : Cas_maker)
      (T : Time.S)
      (K : Irmin.Hash.S)
      (V : Irmin.Type.S)
      () :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** Linked log instantiated using the {{!Irmin_unix.FS} FS backend} provided
      by [Irmin_unix], timestamp method {!Time.Unix} and hash {!Irmin.Hash.SHA1} *)
  module FS (C : Cas_maker) (V : Irmin.Type.S) () :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string

  (** Linked log instantiated using the {{!Irmin_mem} in-memory backend}
      provided by [Irmin_mem], timestamp method {!Time.Unix} and hash
      {!Irmin.Hash.SHA1} *)
  module Mem (C : Cas_maker) (V : Irmin.Type.S) () :
    S
      with type value = V.t
       and type Store.branch = string
       and type Store.key = string list
       and type Store.step = string
end
