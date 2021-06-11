(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

  module Make (Val : Irmin_pack.Pack_value.S with type hash := K.t) = struct
    module KMap = Map.Make (struct
      type t = K.t

      let compare = Irmin.Type.(unstage (compare K.t))
    end)

    type key = K.t
    type value = Val.t

    type 'a t = {
      name : string;
      mutable t : value KMap.t;
      mutable generation : int63;
    }

    let instances =
      Pool.create ~alloc:(fun name ->
          { name; t = KMap.empty; generation = Int63.zero })

    let v name = Lwt.return (Pool.take instances name)
    let equal_key = Irmin.Type.(unstage (equal K.t))

    let clear_keep_generation t =
      Log.debug (fun f -> f "clear_keep_generation");
      t.t <- KMap.empty;
      Lwt.return_unit

    let clear t =
      Log.debug (fun f -> f "clear");
      t.t <- KMap.empty;
      t.generation <- Int63.succ t.generation;
      Lwt.return_unit

    let close t =
      Log.debug (fun f -> f "close");
      Pool.drop instances t.name;
      Lwt.return_unit

    let cast t = (t :> read_write t)
    let batch t f = f (cast t)
    let pp_key = Irmin.Type.pp K.t

    let check_key k v =
      let k' = Val.hash v in
      if equal_key k k' then Ok () else Error (k, k')

    let find t k =
      try
        let v = KMap.find k t.t in
        check_key k v |> Result.map (fun () -> Some v)
      with Not_found -> Ok None

    let unsafe_find ~check_integrity:_ t k =
      Log.debug (fun f -> f "unsafe find %a" pp_key k);
      find t k |> function
      | Ok r -> r
      | Error (k, k') ->
          Fmt.invalid_arg "corrupted value: got %a, expecting %a" pp_key k'
            pp_key k

    let find t k =
      Log.debug (fun f -> f "find %a" pp_key k);
      find t k |> function
      | Ok r -> Lwt.return r
      | Error (k, k') ->
          Fmt.kstrf Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
            pp_key k' pp_key k

    let unsafe_mem t k =
      Log.debug (fun f -> f "mem %a" pp_key k);
      KMap.mem k t.t

    let mem t k = Lwt.return (unsafe_mem t k)

    let unsafe_append ~ensure_unique:_ ~overcommit:_ t k v =
      Log.debug (fun f -> f "add -> %a" pp_key k);
      t.t <- KMap.add k v t.t

    let unsafe_add t k v =
      unsafe_append ~ensure_unique:true ~overcommit:true t k v;
      Lwt.return_unit

    let add t v =
      let k = Val.hash v in
      unsafe_add t k v >|= fun () -> k

    let generation t = t.generation
  end
end
