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

open! Import

module type Sigs = sig
  type 'hash t
  (** The type of {i keys} referencing values stored in the [irmin-pack]
      backend. *)

  type safe = SAFE
  and unsafe = UNSAFE

  (** The internal state of a key (read with {!inspect}).

      Invariant: keys of the form {!Indexed} always reference values that have
      entries in the index (as otherwise these keys could not be dereferenced). *)
  type ('hash, _) unsafe_state = private
    | Direct : {
        hash : 'hash;
        offset : int63;
        length : int;
        volume_identifier : Lower.volume_identifier option;
      }
        -> ('hash, safe) unsafe_state
        (** A "direct" pointer to a value stored at [offset] in the pack-file
            (with hash [hash] and length [length]). Such keys can be
            dereferenced from the store with a single IO read, without needing
            to consult the index.

            They are built in-memory (e.g. after adding a fresh value to the
            pack file), but have no corresponding encoding format, as the pack
            format keeps length information with the values themselves.

            When decoding a inode, which references its children as single
            offsets, we fetch the length information of the child at the same
            time as fetching its hash (which we must do anyway in order to do an
            integrity check), creating keys of this form. *)
    | Indexed : 'hash -> ('hash, safe) unsafe_state
        (** A pointer to an object in the pack file that is indexed. Reading the
            object necessitates consulting the index, after which the key can be
            promoted to {!Direct}.

            Such keys result from decoding pointers to other store objects
            (nodes or commits) from commits or from the branch store. *)
    | Offset : int63 -> ('hash, unsafe) unsafe_state
        (** Same as [Direct], but the hash and length of the object have not
            been fetched. Only used to speed up the GC traversal. *)

  type 'hash state = ('hash, safe) unsafe_state

  (** {2 Undereferencable keys}

      A key [k] is "undereferencable" with respect to some store handle [t] if
      [find t k <> Some _]. Such keys should not arise during regular operation
      of a single Irmin repository, but are still technically constructible in
      the following ways:

      - {b storage corruption}. When decoding a key from disk, we may not
        immediately check that it is dereferenceable for performance reasons. In
        this case, any corruption to the key (or the referenced section of the
        store) will be discovered on attempted [find] (or [mem]).

      - {b passing keys between store handles}. Read-only handles on a pack
        store must explicitly {i reload} to observe recent writes to the store.
        This means that any keys built by a read-write instance and passed to a
        read-only instance will be undereferencable until that reader has
        reloaded.

      - {b passing keys between repositories}. Keys created for one Irmin
        repository may not be dereferenced with respect to another by design. *)

  val inspect : 'hash t -> 'hash state

  val v_direct :
    offset:int63 ->
    length:int ->
    ?volume_identifier:Lower.volume_identifier ->
    'h ->
    'h t

  val v_indexed : 'h -> 'h t
  val v_offset : int63 -> 'h t

  val promote_exn :
    offset:int63 ->
    length:int ->
    ?volume_identifier:Lower.volume_identifier ->
    'h t ->
    unit

  val set_volume_identifier_exn :
    volume_identifier:Lower.volume_identifier option -> 'h t -> unit

  val to_offset : 'h t -> int63 option
  val to_hash : 'h t -> 'h
  val to_length : 'h t -> int option

  module type S = sig
    type hash

    (** @inline *)
    include Irmin_pack.Pack_key.S with type t = hash t and type hash := hash
  end

  module Make (Hash : Irmin.Hash.S) : S with type hash = Hash.t

  module type Store_spec = sig
    type ('h, _) contents_key = 'h t
    type 'h node_key = 'h t
    type 'h commit_key = 'h t
  end

  module Store_spec : Store_spec
end
