(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include Irmin.CONTENT_ADDRESSABLE_STORE

  type index

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    [ `Read ] t Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  module Key : Irmin.Hash.S with type t = key

  module Val : sig
    include Irmin.Private.Node.S with type t = value and type hash = key

    val pred : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
  end

  include S.CHECKABLE with type 'a t := 'a t and type key := key
  include S.CLOSEABLE with type 'a t := 'a t

  val sync : ?on_generation_change:(unit -> unit) -> 'a t -> unit
  val clear_caches : 'a t -> unit
end

module type INTER = sig
  type hash

  val pp_hash : hash Fmt.t

  module Elt : Pack.ELT with type hash := hash

  val decode_bin :
    dict:(int -> string option) ->
    hash:(int64 -> hash) ->
    string ->
    int ->
    int * Elt.t

  module Val_impl : sig
    type t

    val pred : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
    val of_bin : Elt.t -> t
    val save : add:(hash -> Elt.t -> unit) -> mem:(hash -> bool) -> t -> unit
    val hash : t -> hash
  end

  module Val : sig
    type nonrec hash = hash
    type t = { find : hash -> Val_impl.t option; v : Val_impl.t }

    include Irmin.Private.Node.S with type hash := hash and type t := t

    val pred : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list
  end
end

module type S_EXT = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  module Key : Irmin.Hash.S with type t = key
  include S.CHECKABLE with type 'a t := 'a t and type key := key
  include S.CLOSEABLE with type 'a t := 'a t

  val clear_caches : 'a t -> unit
  val hash : value -> key
  val check_hash : key -> key -> unit
end

module type Inode = sig
  module type S = S
  module type INTER = INTER
  module type S_EXT = S_EXT

  module Make_intermediate
      (Conf : Config.S)
      (H : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S with type hash = H.t) :
    INTER
      with type hash = H.t
       and type Val.metadata = Node.metadata
       and type Val.step = Node.step

  module Make_ext
      (H : Irmin.Hash.S)
      (Node : Irmin.Private.Node.S with type hash = H.t)
      (Inter : INTER
                 with type hash = H.t
                  and type Val.metadata = Node.metadata
                  and type Val.step = Node.step)
      (Pack : Pack.S with type value = Inter.Elt.t and type key = H.t) :
    S_EXT with type key = H.t and type value = Inter.Val.t

  module Make
      (Conf : Config.S)
      (H : Irmin.Hash.S)
      (P : Pack.MAKER with type key = H.t and type index = Pack_index.Make(H).t)
      (Node : Irmin.Private.Node.S with type hash = H.t) : sig
    include
      S
        with type key = H.t
         and type Val.metadata = Node.metadata
         and type Val.step = Node.step
         and type index = Pack_index.Make(H).t

    val decode_bin :
      dict:(int -> string option) -> hash:(int64 -> key) -> string -> int -> int
  end
end
