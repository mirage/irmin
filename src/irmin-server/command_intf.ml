(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

module type S = sig
  module Conn : Conn.S

  module Store : Irmin.Generic_key.S
  (** Irmin [Store] type *)

  (** [Tree] wraps [Store.Tree] to avoid encoding/decoding trees more than
      needed *)
  module Tree :
    Tree.S
      with type kinded_key = Store.Tree.kinded_key
       and type concrete = Store.Tree.concrete

  (** Commits *)
  module Commit :
    Commit.S
      with type hash = Store.Hash.t
       and type key = Store.commit_key
       and type tree = Tree.t
       and module Info = Store.Info

  (** Used to track information about the server *)
  module Server_info : sig
    type t = { start_time : float }
  end

  type context = {
    conn : Conn.t;
    config : Irmin.Backend.Conf.t;
    repo : Store.Repo.t;
    mutable watch : Store.watch option;
    mutable branch_watch : Store.Backend.Branch.watch option;
  }
  (** [context] is passed to every command as the first argument *)

  module type CMD = sig
    type req
    type res

    val req_t : req Irmin.Type.t
    val res_t : res Irmin.Type.t
    val name : string

    val run :
      Conn.t -> context -> Server_info.t -> req -> res Conn.Return.t Lwt.t
  end

  type t = (module CMD)
  (** Command type *)

  val name : t -> string
  (** Get the name of a command *)

  val of_name : string -> t
  (** Find a command by name, the name should be lowercase *)

  val commands : (string * t) list
  (** A list of all registered commands *)

  module Commands : sig
    module Contents : sig
      type key = Store.Backend.Contents.key
      type value = Store.Backend.Contents.value
      type hash = Store.Backend.Contents.hash

      module Mem : CMD with type req = key and type res = bool
      module Find : CMD with type req = key and type res = value option
      module Add : CMD with type req = value and type res = key
      module Unsafe_add : CMD with type req = hash * value and type res = key
      module Index : CMD with type req = hash and type res = key option

      module Merge :
        CMD
          with type req = key option option * key option * key option
           and type res = (key option, Irmin.Merge.conflict) Result.t
    end

    module Node : sig
      type key = Store.Backend.Node.key
      type value = Store.Backend.Node.value
      type hash = Store.Backend.Node.hash

      module Mem : CMD with type req = key and type res = bool
      module Find : CMD with type req = key and type res = value option
      module Add : CMD with type req = value and type res = key
      module Unsafe_add : CMD with type req = hash * value and type res = key
      module Index : CMD with type req = hash and type res = key option

      module Merge :
        CMD
          with type req = key option option * key option * key option
           and type res = (key option, Irmin.Merge.conflict) Result.t
    end

    module Commit : sig
      type key = Store.Backend.Commit.key
      type value = Store.Backend.Commit.value
      type hash = Store.Backend.Commit.hash

      module Mem : CMD with type req = key and type res = bool
      module Find : CMD with type req = key and type res = value option
      module Add : CMD with type req = value and type res = key
      module Unsafe_add : CMD with type req = hash * value and type res = key
      module Index : CMD with type req = hash and type res = key option

      module Merge :
        CMD
          with type req =
            Store.Info.t * (key option option * key option * key option)
           and type res = (key option, Irmin.Merge.conflict) Result.t
    end

    module Branch : sig
      type key = Store.Backend.Branch.key
      type value = Store.Backend.Branch.value

      module Mem : CMD with type req = key and type res = bool
      module Find : CMD with type req = key and type res = value option
      module Set : CMD with type req = key * value and type res = unit

      module Test_and_set :
        CMD
          with type req = key * value option * value option
           and type res = bool

      module Remove : CMD with type req = key and type res = unit
      module List : CMD with type req = unit and type res = key list
      module Clear : CMD with type req = unit and type res = unit

      module Watch :
        CMD with type req = (key * value) list option and type res = unit

      module Watch_key :
        CMD with type req = value option * key and type res = unit

      module Unwatch : CMD with type req = unit and type res = unit
    end

    (** Check connectivity *)
    module Ping : CMD with type req = unit and type res = unit

    (** Export repo *)
    module Export : CMD with type req = int option and type res = Store.slice

    (** Import repo *)
    module Import : CMD with type req = Store.slice and type res = unit

    type store =
      [ `Empty | `Branch of Store.branch | `Commit of Store.commit_key ]
    [@@deriving irmin]

    (* Batch operations for tree manipulation *)
    module Batch : sig
      type batch =
        (Store.path
        * [ `Contents of
            [ `Hash of Store.hash | `Value of Store.contents ]
            * Store.metadata option
          | `Tree of Tree.t
          | `Remove ])
        list

      module Apply :
        CMD
          with type req = (store * Store.path) * Store.info * batch
           and type res = Store.commit_key
    end

    (* Store *)
    module Store : sig
      type t = store [@@deriving irmin]

      type write_options =
        (bool option * int option) * (bool option * Store.hash list option)
      [@@deriving irmin]

      (** Find a value in the store *)
      module Find :
        CMD with type req = t * Store.path and type res = Store.contents option

      (** Remove a value from the store *)
      module Remove :
        CMD
          with type req = write_options * (t * Store.path) * Store.Info.t
           and type res = unit

      (** Get a tree from the store *)
      module Find_tree :
        CMD
          with type req = t * Store.path
           and type res = Store.Tree.concrete option

      (** Check for the existence of a value in the store *)
      module Mem : CMD with type req = t * Store.path and type res = bool

      (** Check for the existence of a tree in the store *)
      module Mem_tree : CMD with type req = t * Store.path and type res = bool
    end
  end
end

module type Command = sig
  module type S = S

  module Make
      (IO : Conn.IO)
      (Codec : Conn.Codec.S)
      (Store : Irmin.Generic_key.S) :
    S
      with module Store = Store
       and type Tree.kinded_key = Store.Tree.kinded_key
       and type Tree.concrete = Store.Tree.concrete
       and module Conn.IO = IO
end
