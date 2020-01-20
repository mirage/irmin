(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *                         Ioana Cristescu <ioana@tarides.com>
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

open Lwt.Infix

let src = Logs.Src.create "irmin.layered" ~doc:"Irmin layered store"

module Log = (val Logs.src_log src : Logs.LOG)

module type CA = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  module Key : Irmin.Hash.TYPED with type t = key and type value = value
end

exception Copy_error of string

module Copy
    (SRC : Irmin.CONTENT_ADDRESSABLE_STORE)
    (DST : CA with type key = SRC.key and type value = SRC.value) =
struct
  let add_to_dst name add dk (k, v) =
    add v >>= fun k' ->
    if not (Irmin.Type.equal dk k k') then
      Fmt.kstrf
        (fun x -> Lwt.fail (Copy_error x))
        "%s import error: expected %a, got %a" name
        Irmin.Type.(pp dk)
        k
        Irmin.Type.(pp dk)
        k'
    else Lwt.return_unit

  let already_in_dst ~dst k add =
    DST.mem dst k >>= function
    | true ->
        Log.debug (fun l -> l "already in dst %a" (Irmin.Type.pp DST.Key.t) k);
        Lwt.return_unit
    | false -> add k

  let copy ~src ~dst ~aux str k =
    Log.debug (fun l -> l "copy %s %a" str (Irmin.Type.pp DST.Key.t) k);
    SRC.find src k >>= function
    | None -> Lwt.return_unit
    | Some v -> aux v >>= fun () -> add_to_dst str (DST.add dst) DST.Key.t (k, v)

  let check_and_copy ~src ~dst ~aux str k =
    already_in_dst ~dst k (copy ~src ~dst ~aux str)
end

module Content_addressable
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S)
    (L : CA with type key = K.t and type value = V.t)
    (U : Irmin.CONTENT_ADDRESSABLE_STORE
           with type key = K.t
            and type value = V.t) =
struct
  type key = K.t

  type value = V.t

  type 'a t = { upper : 'a U.t; lower : [ `Read ] L.t }

  module Copy = Copy (U) (L)

  let already_in_dst = Copy.already_in_dst

  let check_and_copy t ~dst ~aux str k =
    Copy.check_and_copy ~src:t.upper ~dst ~aux str k

  let copy t ~dst ~aux str k = Copy.copy ~src:t.upper ~dst ~aux str k

  let find t k =
    U.find t.upper k >>= function
    | None -> L.find t.lower k
    | Some v -> Lwt.return_some v

  let mem t k =
    U.mem t.upper k >>= function
    | true -> Lwt.return_true
    | false -> L.mem t.lower k

  let add t = U.add t.upper

  let unsafe_add t = U.unsafe_add t.upper

  let v upper lower = { upper; lower }

  let project (upper : [ `Read | `Write ] U.t) t = { t with upper }

  let layer_id t k =
    U.mem t.upper k >>= function
    | true -> Lwt.return 1
    | false -> (
        L.mem t.lower k >|= function true -> 2 | false -> raise Not_found )

  let clear_upper t = U.clear t.upper

  let clear t = U.clear t.upper >>= fun () -> L.clear t.lower
end

module Atomic_write
    (K : Irmin.Type.S)
    (V : Irmin.Hash.S)
    (L : Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t)
    (U : Irmin.ATOMIC_WRITE_STORE with type key = K.t and type value = V.t) =
struct
  type t = { upper : U.t; lower : L.t }

  type key = K.t

  type value = V.t

  let mem t k =
    U.mem t.upper k >>= function
    | true -> Lwt.return_true
    | false -> L.mem t.lower k

  let find t k =
    U.find t.upper k >>= function
    | None -> L.find t.lower k
    | Some v -> Lwt.return_some v

  let set t = U.set t.upper

  (* we have to copy back into upper the branch against we want to do
     test and set *)
  let test_and_set t k ~test ~set =
    U.mem t.upper k >>= function
    | true -> U.test_and_set t.upper k ~test ~set
    | false -> (
        L.find t.lower k >>= function
        | None -> U.test_and_set t.upper k ~test:None ~set
        | Some v ->
            U.set t.upper k v >>= fun () -> U.test_and_set t.upper k ~test ~set
        )

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

  let clear_upper t = U.clear t.upper

  let clear t = U.clear t.upper >>= fun () -> L.clear t.lower

  let v upper lower = { upper; lower }

  let copy t commit_exists =
    U.list t.upper >>= fun branches ->
    Lwt_list.iter_p
      (fun branch ->
        U.find t.upper branch >>= function
        | None -> failwith "branch not found in src"
        | Some hash -> (
            (* Do not copy branches that point to commits not copied. *)
            commit_exists hash
            >>= function
            | true -> L.set t.lower branch hash
            | false -> Lwt.return_unit ))
      branches
end
