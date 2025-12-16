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

(** [Irmin_containers] is a collection of simple, ready-to-use mergeable data
    structures. Each data structure works with an arbitrary Irmin backend and is
    customisable in a variety of ways.

    Additionally, [Irmin_containers] supplies instantiations of each of these
    data structures with two backends:

    - the {{!Irmin_mem} in-memory backend} provided by {!Irmin_mem}
    - the {{!Irmin_fs_unix} FS backend} provided by {!Irmin_fs_unix}. *)

(** {1 Data structures} *)

module Counter = Counter
module Lww_register = Lww_register
module Blob_log = Blob_log
module Linked_log = Linked_log

(** {1 Auxiliary signatures and modules} *)

(** [Store_maker] is the signature for the backend input to the data structures.
    The Irmin stores of the data structures are constructed using modules of
    this type *)

module type Content_addressable = Stores.Content_addressable

(** [Cas_maker] is the signature for the store which will be used to maintain
    linked data structures. The elements are hashed into this store and the hash
    value is used to construct the linkages. *)

module Time = Time
