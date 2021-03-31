(* Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)

open! Import

module Content_addressable (S : Pack.S) = struct
  include Irmin.Content_addressable.Check_closed (S)

  type index = S.index

  let v ?fresh ?readonly ?lru_size ~index root =
    let+ t = S.v ?fresh ?readonly ?lru_size ~index root in
    v t

  let add t v =
    check_not_closed t;
    S.add (raw t) v

  let unsafe_add t v =
    check_not_closed t;
    S.unsafe_add (raw t) v

  let unsafe_append ~ensure_unique ~overcommit t k v =
    check_not_closed t;
    S.unsafe_append ~ensure_unique ~overcommit (raw t) k v

  let unsafe_mem t k =
    check_not_closed t;
    S.unsafe_mem (raw t) k

  let unsafe_find ~check_integrity t k =
    check_not_closed t;
    S.unsafe_find ~check_integrity (raw t) k

  let flush ?index ?index_merge t =
    check_not_closed t;
    S.flush ?index ?index_merge (raw t)

  let sync ?on_generation_change t =
    check_not_closed t;
    S.sync ?on_generation_change (raw t)

  let integrity_check ~offset ~length k t =
    check_not_closed t;
    S.integrity_check ~offset ~length k (raw t)

  let clear_caches t =
    check_not_closed t;
    S.clear_caches (raw t)

  let version t =
    check_not_closed t;
    S.version (raw t)

  let generation t =
    check_not_closed t;
    S.generation (raw t)

  let offset t =
    check_not_closed t;
    S.offset (raw t)

  let clear_keep_generation t =
    check_not_closed t;
    S.clear_keep_generation (raw t)
end

module Atomic_write (AW : S.Atomic_write.Store) = struct
  include Irmin.Atomic_write.Check_closed (AW)

  let v ?fresh ?readonly root =
    let+ t = AW.v ?fresh ?readonly root in
    v t

  let flush t =
    check_not_closed t;
    AW.flush (raw t)

  let clear_keep_generation t =
    check_not_closed t;
    AW.clear_keep_generation (raw t)
end
