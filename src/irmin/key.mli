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

(** Module types for {i keys} into an arbitrary store.

    {2 Choices of key representation}

    {3 Hash-like keys}

    Most Irmin stores are keyed directly by a {i hash} of the values that they
    store. This results in a so-called "content-addressable" store, in which all
    possible contents values have exactly one key. The key is derived from the
    value via [hash] and, given a key, the corresponding value can be retrieved
    from the store via [find]:

    {[
                      ┌───────────────────────────────┐
                      │                               │
      ┌───────┐       v    find   ┌───────┐  hash   ┌─────┐
      │ Store │ ──>  (+)  ──────> │ Value │ ──────> │ Key │
      └───────┘                   └───────┘         └─────┘
    ]}

    Keys built this way – with a 1:1 correspondence between the key and the
    hash, are known as {!Hash_like}. This class of key representation has some
    important properties:

    - {b irredundant storage}: given that an object's key is derived directly
      from its representation, there can be at most one copy of any content
      value in the store at one time – the store is naturally free of
      duplicates.

    - {b reproducibility}: again, because keys are derived directly from values,
      multiple parties are guaranteed to compute the same store keys for a given
      set of stored values.

    However, it also has some disadvantages. Implementing a hash-keyed store
    requires some mechanism for mapping hashes to their physical location (e.g.
    their offset on disk), called an "index". This auxiliary index occupies
    space, and must be queried for each pointer in the store (even for internal
    pointers between nodes along a path).

    {3 Non-hash-like keys}

    For this reason, Irmin allows backends to supply custom key representations
    that do not satisfy the above properties (i.e. not irredundant, and not
    reproducible). This leads to the following more complex set of relationships
    between the values:

    {[
                     ┌────────────────────────────────────────────────────────┐
                     │                                                        │
        ┌············┼··········································┐             │
        :            │                                          :             │
      ┌───────┐      v    find  ┌───────┐  hash   ┌──────┐      v   index   ┌─────┐
      │ Store │ ──> (+) ──────> │ Value │ ──────> │ Hash │ ··> (+) ·······> │ Key │
      └───────┘                 └───────┘         └──────┘                  └─────┘
        │                          v                 ∧        to_hash         │ ^
        └───────────────────────> (+)                └────────────────────────┘ │
                                   │       add                                  │
                                   └────────────────────────────────────────────┘
    ]}

    In general, the key of a value isn't known until it has been added to a
    particular store, and keys need not be portable between different stores.
    It's still required that keys be convertible to hashes, as this ensures the
    consistency of Irmin's Merkle tree structure.

    Stores that use non-hash-like keys are not content-addressable, since the
    user can't compute the key of a value given the value itself (and perhaps a
    value has more than one possible key).

    Implementer's note: all key implementations must have a pre-hash
    implementation derived in terms of [Key.to_hash]. That is, for all keys [k],
    given [h = Key.to_hash k] then [Irmin.Type.pre_hash key_t k] must equal
    [Irmin.Type.pre_hash hash_t h]. *)

include Key_intf.Sigs
(** @inline *)
