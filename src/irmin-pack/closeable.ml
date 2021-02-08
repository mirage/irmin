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
  type 'a t = { closed : bool ref; t : 'a S.t }
  type key = S.key
  type value = S.value

  let check_not_closed t = if !(t.closed) then raise Irmin.Closed

  let mem t k =
    check_not_closed t;
    S.mem t.t k

  let find t k =
    check_not_closed t;
    S.find t.t k

  let add t v =
    check_not_closed t;
    S.add t.t v

  let unsafe_add t k v =
    check_not_closed t;
    S.unsafe_add t.t k v

  let batch t f =
    check_not_closed t;
    S.batch t.t (fun w -> f { t = w; closed = t.closed })

  let v ?fresh ?readonly root =
    let+ t = S.v ?fresh ?readonly root in
    { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t)

  let unsafe_append ~ensure_unique ~overcommit t k v =
    check_not_closed t;
    S.unsafe_append ~ensure_unique ~overcommit t.t k v

  let unsafe_mem t k =
    check_not_closed t;
    S.unsafe_mem t.t k

  let unsafe_find ~check_integrity t k =
    check_not_closed t;
    S.unsafe_find ~check_integrity t.t k

  let flush ?index ?index_merge t =
    check_not_closed t;
    S.flush ?index ?index_merge t.t

  let sync ?on_generation_change t =
    check_not_closed t;
    S.sync ?on_generation_change t.t

  let clear t =
    check_not_closed t;
    S.clear t.t

  let integrity_check ~offset ~length k t =
    check_not_closed t;
    S.integrity_check ~offset ~length k t.t

  let clear_caches t =
    check_not_closed t;
    S.clear_caches t.t

  let version t =
    check_not_closed t;
    S.version t.t

  let generation t =
    check_not_closed t;
    S.generation t.t

  let offset t =
    check_not_closed t;
    S.offset t.t

  let clear_keep_generation t =
    check_not_closed t;
    S.clear_keep_generation t.t
end

module Content_addressable_indexed (S : Pack.INDEXED_S) = struct
  module Indexed_S = S

  module S = struct
    include Indexed_S

    (* temporarly shadow the v function *)
    let v ?fresh:_ ?readonly:_ _ = Lwt.fail_with "not implemented"
  end

  include Content_addressable (S)

  type index = S.index

  let v ?fresh ?readonly ?lru_size ~index root =
    let+ t = Indexed_S.v ?fresh ?readonly ?lru_size ~index root in
    { closed = ref false; t }
end

module Atomic_write (AW : S.ATOMIC_WRITE_STORE) = struct
  type t = { closed : bool ref; t : AW.t }
  type key = AW.key
  type value = AW.value

  let check_not_closed t = if !(t.closed) then raise Irmin.Closed

  let mem t k =
    check_not_closed t;
    AW.mem t.t k

  let find t k =
    check_not_closed t;
    AW.find t.t k

  let set t k v =
    check_not_closed t;
    AW.set t.t k v

  let test_and_set t k ~test ~set =
    check_not_closed t;
    AW.test_and_set t.t k ~test ~set

  let remove t k =
    check_not_closed t;
    AW.remove t.t k

  let list t =
    check_not_closed t;
    AW.list t.t

  type watch = AW.watch

  let watch t ?init f =
    check_not_closed t;
    AW.watch t.t ?init f

  let watch_key t k ?init f =
    check_not_closed t;
    AW.watch_key t.t k ?init f

  let unwatch t w =
    check_not_closed t;
    AW.unwatch t.t w

  let v ?fresh ?readonly root =
    let+ t = AW.v ?fresh ?readonly root in
    { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      AW.close t.t)

  let clear t =
    check_not_closed t;
    AW.clear t.t

  let flush t =
    check_not_closed t;
    AW.flush t.t

  let clear_keep_generation t =
    check_not_closed t;
    AW.clear_keep_generation t.t
end
