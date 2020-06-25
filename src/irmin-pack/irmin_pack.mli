(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

val config :
  ?fresh:bool ->
  ?readonly:bool ->
  ?lru_size:int ->
  ?index_log_size:int ->
  string ->
  Irmin.config

module Pack = Pack
module Dict = Pack_dict
module Index = Pack_index

exception RO_Not_Allowed

module type CONFIG = sig
  val entries : int

  val stable_hash : int
end

module type Stores_extra = sig
  type repo

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result
  (** Checks the integrity of the repository. if [auto_repair] is [true], will
      also try to fix the issues. [ppf] is a formatter for progressive
      reporting. [`Fixed] and [`Corrupted] report the number of fixed/corrupted
      entries. *)

  val sync : repo -> unit
  (** [sync t] syncs a readonly pack with the files on disk. Raises
      [invalid_argument] if called by a read-write pack.*)
end

module Make_ext
    (Config : CONFIG)
    (Metadata : Irmin.Metadata.S)
    (Contents : Irmin.Contents.S)
    (Path : Irmin.Path.S)
    (Branch : Irmin.Branch.S)
    (Hash : Irmin.Hash.S)
    (N : Irmin.Private.Node.S
           with type metadata = Metadata.t
            and type hash = Hash.t
            and type step = Path.step)
    (CT : Irmin.Private.Commit.S with type hash = Hash.t) : sig
  include
    Irmin.S
      with type key = Path.t
       and type contents = Contents.t
       and type branch = Branch.t
       and type hash = Hash.t
       and type step = Path.step
       and type metadata = Metadata.t
       and type Key.step = Path.step
       and type Private.Sync.endpoint = unit

  include Stores_extra with type repo := repo
end

module Make
    (Config : CONFIG)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) : sig
  include
    Irmin.S
      with type key = P.t
       and type step = P.step
       and type metadata = M.t
       and type contents = C.t
       and type branch = B.t
       and type hash = H.t
       and type Private.Sync.endpoint = unit

  include Stores_extra with type repo := repo
end

module KV (Config : CONFIG) : Irmin.KV_MAKER

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) : sig
  include Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t

  val v : ?fresh:bool -> ?readonly:bool -> string -> t Lwt.t
end

module Stats = Stats
