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

open Lwt.Infix

module Pack (S : Pack.S) = struct
  type 'a t = { closed : bool ref; t : 'a S.t }

  type key = S.key

  type value = S.value

  type index = S.index

  let check_closed t = if !(t.closed) then raise Irmin.Closed

  let mem t k =
    check_closed t;
    S.mem t.t k

  let find t k =
    check_closed t;
    S.find t.t k

  let add t v =
    check_closed t;
    S.add t.t v

  let unsafe_add t k v =
    check_closed t;
    S.unsafe_add t.t k v

  let batch t f =
    check_closed t;
    S.batch t.t (fun w -> f { t = w; closed = t.closed })

  let v ?fresh ?readonly ?lru_size ~index root =
    S.v ?fresh ?readonly ?lru_size ~index root >|= fun t ->
    { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      S.close t.t )

  let unsafe_append t k v =
    check_closed t;
    S.unsafe_append t.t k v

  let unsafe_mem t k =
    check_closed t;
    S.unsafe_mem t.t k

  let unsafe_find t k =
    check_closed t;
    S.unsafe_find t.t k

  let sync t =
    check_closed t;
    S.sync t.t

  let integrity_check ~offset ~length k t =
    check_closed t;
    S.integrity_check ~offset ~length k t.t
end
