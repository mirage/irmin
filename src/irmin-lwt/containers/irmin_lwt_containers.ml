(*
 * Copyright (c) 2020 KC Sivaramakrishnan <kc@kcsrk.info>
 * Copyright (c) 2020 Anirudh Sunder Raj <anirudh6626@gmail.com>
 * Copyright (c) 2026 Tarides
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

(* Lwt-flavoured port of irmin-containers (Counter / Lww_register / Blob_log)
   on top of [irmin-lwt]. Each merge function is bridged from its Eio
   counterpart in [Irmin.Merge] via [Irmin_lwt.Lwt_to_eio.merge_of_eio]; the
   data-structure operations themselves use Lwt-typed [Store] ops directly. *)

open Lwt.Syntax

module Time = struct
  module type S = sig
    include Irmin_lwt.Type.S

    val now : unit -> t
  end

  module Machine : S = struct
    type t = Mtime.t

    let t =
      Irmin_lwt.Type.map ~equal:Mtime.equal ~compare:Mtime.compare
        Irmin_lwt.Type.int64 Mtime.of_uint64_ns Mtime.to_uint64_ns

    let now = Mtime_clock.now
  end
end

module Counter = struct
  module type S = sig
    module Store : Irmin_lwt.KV

    val inc :
      ?by:int64 ->
      ?info:Store.Info.f ->
      path:Store.path ->
      Store.t ->
      unit Lwt.t

    val dec :
      ?by:int64 ->
      ?info:Store.Info.f ->
      path:Store.path ->
      Store.t ->
      unit Lwt.t

    val read : path:Store.path -> Store.t -> int64 Lwt.t
  end

  module Counter_contents : Irmin_lwt.Contents.S with type t = int64 = struct
    type t = int64

    let t = Irmin_lwt.Type.int64

    let merge =
      Irmin_lwt.Lwt_to_eio.merge_of_eio
        Irmin.Type.(option int64)
        Irmin.Merge.(option counter)
  end

  module Make (Backend : Irmin_lwt.KV_maker) : S = struct
    module Store = Backend.Make (Counter_contents)

    let empty_info = Store.Info.none

    let modify by info t path fn =
      let* current = Store.find t path in
      let v = match current with Some v -> v | None -> 0L in
      Store.set_exn ~info t path (fn v by)

    let inc ?(by = 1L) ?(info = empty_info) ~path t =
      modify by info t path Int64.add

    let dec ?(by = 1L) ?(info = empty_info) ~path t =
      modify by info t path Int64.sub

    let read ~path t =
      let+ v = Store.find t path in
      match v with None -> 0L | Some v -> v
  end

  module FS = Make (Irmin_lwt_fs.KV)
  module Mem = Make (Irmin_lwt_mem.KV)
end

module Lww_register = struct
  module LWW (T : Time.S) (V : Irmin_lwt.Type.S) :
    Irmin_lwt.Contents.S with type t = V.t * T.t = struct
    type t = V.t * T.t [@@deriving irmin]

    (* The merge logic is identical to [Irmin_containers.Lww_register.LWW];
       we reproduce it here on the Lwt side because the type [t] is
       parameterised over local [T] and [V] and cannot be lifted from a
       prebuilt Eio module. The merge itself is pure (no I/O), so it is
       wrapped to a Lwt [Merge.t] without a runtime bridge. *)

    let compare_t = Irmin.Type.(unstage (compare T.t))
    let compare_v = Irmin.Type.(unstage (compare V.t))

    let compare (v1, t1) (v2, t2) =
      let res = compare_t t1 t2 in
      if res = 0 then compare_v v1 v2 else res

    let merge_eio ~old:_ v1 v2 =
      let open Irmin.Merge in
      if compare v1 v2 > 0 then ok v1 else ok v2

    let merge =
      Irmin_lwt.Lwt_to_eio.merge_of_eio (Irmin.Type.option t)
        Irmin.Merge.(option (v t merge_eio))
  end

  module type S = sig
    module Store : Irmin_lwt.KV

    type value

    val read : path:Store.path -> Store.t -> value option Lwt.t

    val write :
      ?info:Store.Info.f -> path:Store.path -> Store.t -> value -> unit Lwt.t
  end

  module Make
      (Backend : Irmin_lwt.KV_maker)
      (T : Time.S)
      (V : Irmin_lwt.Type.S) : S with type value = V.t = struct
    module Store = Backend.Make (LWW (T) (V))

    let empty_info = Store.Info.none

    type value = V.t

    let read ~path t =
      let+ x = Store.find t path in
      match x with None -> None | Some (v, _) -> Some v

    let write ?(info = empty_info) ~path t v =
      let timestamp = T.now () in
      Store.set_exn ~info t path (v, timestamp)
  end

  module FS (V : Irmin_lwt.Type.S) = Make (Irmin_lwt_fs.KV) (Time.Machine) (V)
  module Mem (V : Irmin_lwt.Type.S) = Make (Irmin_lwt_mem.KV) (Time.Machine) (V)
end

module Blob_log = struct
  module Blob_log (T : Time.S) (V : Irmin_lwt.Type.S) :
    Irmin_lwt.Contents.S with type t = (V.t * T.t) list = struct
    type t = (V.t * T.t) list [@@deriving irmin]

    let compare_t = Irmin.Type.(unstage (compare T.t))
    let compare (_, t1) (_, t2) = compare_t t1 t2

    let newer_than timestamp entries =
      let rec util acc = function
        | [] -> List.rev acc
        | (_, x) :: _ when compare_t x timestamp <= 0 -> List.rev acc
        | h :: t -> util (h :: acc) t
      in
      util [] entries

    let merge_eio ~old v1 v2 =
      let open Irmin.Merge.Infix in
      let ok = Irmin.Merge.ok in
      old () >>=* fun old ->
      let old = match old with None -> [] | Some o -> o in
      let l1, l2 =
        match old with
        | [] -> (v1, v2)
        | (_, t) :: _ -> (newer_than t v1, newer_than t v2)
      in
      let l3 = List.sort compare (List.rev_append l1 l2) in
      ok (List.rev_append l3 old)

    let merge =
      Irmin_lwt.Lwt_to_eio.merge_of_eio
        Irmin.Type.(option t)
        Irmin.Merge.(option (v t merge_eio))
  end

  module type S = sig
    module Store : Irmin_lwt.KV

    type value

    val append : path:Store.path -> Store.t -> value -> unit Lwt.t
    val read_all : path:Store.path -> Store.t -> value list Lwt.t
  end

  module Make
      (Backend : Irmin_lwt.KV_maker)
      (T : Time.S)
      (V : Irmin_lwt.Type.S) : S with type value = V.t = struct
    module Store = Backend.Make (Blob_log (T) (V))

    let empty_info = Store.Info.none

    type value = V.t

    let create_entry v = (v, T.now ())

    let append ~path t v =
      let* current = Store.find t path in
      let entry = create_entry v in
      match current with
      | None -> Store.set_exn ~info:empty_info t path [ entry ]
      | Some l -> Store.set_exn ~info:empty_info t path (entry :: l)

    let read_all ~path t =
      let+ x = Store.find t path in
      match x with None -> [] | Some l -> List.map (fun (v, _) -> v) l
  end

  module FS (V : Irmin_lwt.Type.S) = Make (Irmin_lwt_fs.KV) (Time.Machine) (V)
  module Mem (V : Irmin_lwt.Type.S) = Make (Irmin_lwt_mem.KV) (Time.Machine) (V)
end
