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

module Pool : sig
  type ('k, 'v) t
  (** Reference-counted pool of values with corresponding keys. *)

  val create : alloc:('k -> 'v) -> ('k, 'v) t
  (** Get an empty pool, given a function for allocating new instances from IDs. *)

  val take : ('k, 'v) t -> 'k -> 'v
  (** Get an instance from the pool by its key, allocating it if necessary. *)

  val drop : ('k, 'v) t -> 'k -> unit
  (** Reduce the reference count of an element, discarding it if the reference
      count drops to 0. *)
end = struct
  type 'v elt = { mutable refcount : int; instance : 'v }
  type ('k, 'v) t = { instances : ('k, 'v elt) Hashtbl.t; alloc : 'k -> 'v }

  let create ~alloc = { instances = Hashtbl.create 0; alloc }

  let take t k =
    match Hashtbl.find_opt t.instances k with
    | Some elt ->
        elt.refcount <- succ elt.refcount;
        elt.instance
    | None ->
        let instance = t.alloc k in
        Hashtbl.add t.instances k { instance; refcount = 1 };
        instance

  let drop t k =
    match Hashtbl.find_opt t.instances k with
    | None -> failwith "Pool.drop: double free"
    | Some { refcount; _ } when refcount <= 0 -> assert false
    | Some { refcount = 1; _ } -> Hashtbl.remove t.instances k
    | Some elt -> elt.refcount <- pred elt.refcount
end

module Maker (K : Irmin.Hash.S) = struct
  type key = K.t

  module Make
      (Val : Irmin_pack.Pack_value.S with type hash := K.t and type key := K.t) =
  struct
    (* TODO(craigfe): We could use the keys to skip traversal of the map on
       lookup. This wasn't done originally due to complications with implementing
       the [clear] function, but this has since been removed. (See #1794.) *)
    module Key = Irmin.Key.Of_hash (K)

    module KMap = Map.Make (struct
      type t = K.t

      let compare = Irmin.Type.(unstage (compare K.t))
    end)

    type hash = K.t
    type key = Key.t
    type value = Val.t
    type 'a t = { name : string; mutable t : value KMap.t }

    let index_direct _ h = Some h
    let index t h = Lwt.return (index_direct t h)
    let instances = Pool.create ~alloc:(fun name -> { name; t = KMap.empty })
    let v name = Lwt.return (Pool.take instances name)
    let equal_key = Irmin.Type.(unstage (equal K.t))

    let close t =
      [%log.debug "close"];
      Pool.drop instances t.name;
      Lwt.return_unit

    let cast t = (t :> read_write t)
    let batch t f = f (cast t)
    let pp_hash = Irmin.Type.pp K.t

    let check_key k v =
      let k' = Val.hash v in
      if equal_key k k' then Ok () else Error (k, k')

    let find t k =
      try
        let v = KMap.find k t.t in
        check_key k v |> Result.map (fun () -> Some v)
      with Not_found -> Ok None

    let unsafe_find ~check_integrity:_ t k =
      [%log.debug "unsafe find %a" pp_hash k];
      find t k |> function
      | Ok r -> r
      | Error (k, k') ->
          Fmt.invalid_arg "corrupted value: got %a, expecting %a" pp_hash k'
            pp_hash k

    let find t k =
      [%log.debug "find %a" pp_hash k];
      find t k |> function
      | Ok r -> Lwt.return r
      | Error (k, k') ->
          Fmt.kstr Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
            pp_hash k' pp_hash k

    let unsafe_mem t k =
      [%log.debug "mem %a" pp_hash k];
      KMap.mem k t.t

    let mem t k = Lwt.return (unsafe_mem t k)

    let unsafe_append ~ensure_unique:_ ~overcommit:_ t k v =
      [%log.debug "add -> %a" pp_hash k];
      t.t <- KMap.add k v t.t;
      k

    let unsafe_add t k v =
      Lwt.return (unsafe_append ~ensure_unique:true ~overcommit:true t k v)

    let add t v = unsafe_add t (Val.hash v) v
  end
end
