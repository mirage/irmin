(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESIrmin. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "irmin.layered" ~doc:"Irmin layered store"

module Log = (val Logs.src_log src : Logs.LOG)

open Lwt.Infix

module Content_addressable
    (Index : Pack_index.S)
    (Pack : Pack.S with type index = Index.t) :
  S.LAYERED_CONTENT_ADDRESSABLE_STORE
    with type key = Pack.key
     and type value = Pack.value
     and type index = Pack.index
     and module U = Pack
     and module L = Pack = struct
  type index = Pack.index

  type key = Pack.key

  type value = Pack.value

  module U = Pack
  module L = Pack

  type 'a t = { upper : [ `Read ] U.t; lower : [ `Read ] L.t }

  let v upper lower = { upper; lower }

  let add t = U.add t.upper

  let unsafe_add t = U.unsafe_add t.upper

  let unsafe_append t k v = U.unsafe_append t.upper k v

  let find t k =
    U.find t.upper k >>= function
    | None -> L.find t.lower k
    | Some v -> Lwt.return_some v

  let unsafe_find t k =
    match U.unsafe_find t.upper k with
    | None -> L.unsafe_find t.lower k
    | Some v -> Some v

  let mem t k =
    U.mem t.upper k >>= function
    | true -> Lwt.return_true
    | false -> L.mem t.lower k

  let unsafe_mem t k = U.unsafe_mem t.upper k || L.unsafe_mem t.lower k

  let flush ?index t =
    U.flush ?index t.upper;
    L.flush ?index t.lower

  let cast t = (t :> [ `Read | `Write ] t)

  let batch t f =
    f (cast t) >|= fun r ->
    flush ~index:true t;
    r

  let sync ?on_generation_change t =
    U.sync ?on_generation_change t.upper;
    L.sync ?on_generation_change t.lower

  let close t = U.close t.upper >>= fun () -> L.close t.lower

  type integrity_error = U.integrity_error

  let integrity_check ~offset ~length k t =
    let upper = t.upper in
    let lower = t.lower in
    match
      ( U.integrity_check ~offset ~length k upper,
        L.integrity_check ~offset ~length k lower )
    with
    | Ok (), Ok () -> Ok ()
    | Error `Wrong_hash, _ | _, Error `Wrong_hash -> Error `Wrong_hash
    | Error `Absent_value, _ | _, Error `Absent_value -> Error `Absent_value

  let layer_id t k =
    U.mem t.upper k >>= function
    | true -> Lwt.return `Upper
    | false -> (
        L.mem t.lower k >|= function true -> `Lower | false -> raise Not_found)

  let clear t = U.clear t.upper >>= fun () -> L.clear t.lower

  let clear_caches t =
    U.clear_caches t.upper;
    L.clear_caches t.lower

  let version t = U.version t.upper
end

module Pack_Maker
    (H : Irmin.Hash.S)
    (Index : Pack_index.S)
    (P : Pack.MAKER with type key = H.t and type index = Index.t) :
  S.LAYERED_MAKER with type key = P.key and type index = P.index = struct
  type index = P.index

  type key = P.key

  module Make (V : Pack.ELT with type hash := key) = struct
    module Upper = P.Make (V)
    include Content_addressable (Index) (Upper)
  end
end

module Atomic_write (A : S.ATOMIC_WRITE_STORE) :
  S.LAYERED_ATOMIC_WRITE_STORE with type key = A.key and type value = A.value =
struct
  type key = A.key

  type value = A.value

  type t = { upper : A.t; lower : A.t }

  module U = A
  module L = A

  let mem t k =
    U.mem t.upper k >>= function
    | true -> Lwt.return_true
    | false -> L.mem t.lower k

  let find t k =
    U.find t.upper k >>= function
    | None -> L.find t.lower k
    | Some v -> Lwt.return_some v

  let set t = U.set t.upper

  (** Copy back into upper the branch against we want to do test and set. *)
  let test_and_set t k ~test ~set =
    U.mem t.upper k >>= function
    | true -> L.test_and_set t.upper k ~test ~set
    | false -> (
        L.find t.lower k >>= function
        | None -> U.test_and_set t.upper k ~test:None ~set
        | Some v ->
            U.set t.upper k v >>= fun () -> U.test_and_set t.upper k ~test ~set)

  let remove t k = U.remove t.upper k >>= fun () -> L.remove t.lower k

  let list t =
    U.list t.upper >>= fun upper ->
    L.list t.lower >|= fun lower ->
    List.fold_left
      (fun acc b -> if List.mem b acc then acc else b :: acc)
      lower upper

  type watch = U.watch

  let watch t = U.watch t.upper

  let watch_key t = U.watch_key t.upper

  let unwatch t = U.unwatch t.upper

  let close t = U.close t.upper >>= fun () -> L.close t.lower

  let v upper lower = { upper; lower }

  let clear t = U.clear t.upper >>= fun () -> L.clear t.lower

  let flush t =
    U.flush t.upper;
    L.flush t.lower
end
