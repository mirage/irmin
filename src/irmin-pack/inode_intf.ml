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

module type Val_intf = sig
  include Irmin.Private.Node.S

  val pred : t -> [ `Node of hash | `Inode of hash | `Contents of hash ] list

  module type DATA_FORMAT = sig
    type vt
    type t

    exception Misconstructed_Data of string

    val of_int : ?label:string -> int -> t
    val to_int : ?label:string -> t -> int
    val of_string : ?label:string -> string -> t
    val to_string : ?label:string -> t -> string
    val of_hash : ?label:string -> hash -> t
    val to_hash : ?label:string -> t -> hash
    val of_lazy_hash : ?label:string -> hash lazy_t -> t
    val to_lazy_hash : ?label:string -> t -> hash lazy_t
    val of_metadata : ?label:string -> metadata -> t
    val to_metadata : ?label:string -> t -> metadata

    val of_value :
      ?label:string ->
      ?label_hash:string ->
      ?label_metadata:string ->
      [< `Contents of hash * metadata | `Node of hash ] ->
      t

    val to_value :
      ?label:string ->
      ?label_hash:string ->
      ?label_metadata:string ->
      t ->
      [> `Contents of hash * metadata | `Node of hash ]

    val of_step : ?label:string -> step -> t
    val to_step : ?label:string -> t -> step
    val join : ?label:string -> t list -> t
    val disjoin : ?label:string -> t -> t list
    val parse_from_file : string -> t
    val parse_from_string : string -> t
  end

  module Sexp : DATA_FORMAT with type t = Sexplib.Sexp.t

  module type SERDE = sig
    exception Wrong_Config of (int * int) * (int * int)

    type d

    val of_t : t -> d
    val to_t : d -> t * hash
  end

  module MinimalSerde (D : DATA_FORMAT) : SERDE with type d = D.t
  module MinimalSerdeSexp : SERDE with type d = Sexplib.Sexp.t

  module Private : sig
    val hash : t -> hash
    val stable : t -> bool
    val length : t -> int
  end
end

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
  module Val : Val_intf with type t = value and type hash = key
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

    include Val_intf with type hash := hash and type t := t
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
