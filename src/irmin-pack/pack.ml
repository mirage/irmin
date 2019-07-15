(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = "00000001"

let ( // ) = Filename.concat

let ( -- ) = Int64.sub

type all_stats = {
  mutable pack_finds : int;
  mutable pack_cache_misses : int;
  mutable appended_hashes : int;
  mutable appended_offsets : int
}

let fresh_stats () =
  { pack_finds = 0;
    pack_cache_misses = 0;
    appended_hashes = 0;
    appended_offsets = 0
  }

let stats = fresh_stats ()

let reset_stats () =
  stats.pack_finds <- 0;
  stats.pack_cache_misses <- 0;
  stats.appended_hashes <- 0;
  stats.appended_offsets <- 0;
  ()

module type S = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val magic : char

  val encode_bin :
    dict:(string -> int) ->
    offset:(hash -> int64 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int64 -> hash) -> string -> int -> t
end

open Lwt.Infix

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t

  let hash (t : t) = Irmin.Type.short_hash K.t t

  let equal (x : t) (y : t) = Irmin.Type.equal K.t x y
end)

module Cache (K : Irmin.Type.S) = Lru.Make (struct
  type t = K.t

  let hash (t : t) = Irmin.Type.short_hash K.t t

  let equal (x : t) (y : t) = Irmin.Type.equal K.t x y
end)

exception RO_Not_Allowed = IO.RO_Not_Allowed

module IO = IO.Unix

module File (K : Irmin.Hash.S) = struct
  module Index =
    Index_unix.Make (struct
        type t = K.t

        let pp = Irmin.Type.pp K.t

        let hash = K.short_hash

        let equal = Irmin.Type.equal K.t

        let encode = Irmin.Type.to_bin_string K.t

        let decode s off = snd (Irmin.Type.decode_bin K.t s off)

        let encoded_size = K.hash_size
      end)
      (struct
        type t = int64 * int * char

        let pp = Irmin.Type.(pp (triple int64 int char))

        let encode (off, len, kind) =
          Irmin.Type.(to_bin_string (triple int64 int32 char))
            (off, Int32.of_int len, kind)

        let decode s off =
          let off, len, kind =
            snd (Irmin.Type.(decode_bin (triple int64 int32 char)) s off)
          in
          (off, Int32.to_int len, kind)

        let encoded_size = (64 / 8) + (32 / 8) + 1
      end)

  module Tbl = Table (K)

  type 'a t = {
    block : IO.t;
    index : Index.t;
    dict : Dict.t;
    lock : Lwt_mutex.t
  }

  let unsafe_clear t =
    IO.clear t.block;
    Index.clear t.index;
    Dict.clear t.dict

  let clear t =
    Lwt_mutex.with_lock t.lock (fun () ->
        unsafe_clear t;
        Lwt.return () )

  let files = Hashtbl.create 10

  let create = Lwt_mutex.create ()

  let unsafe_v ?(fresh = false) ?(readonly = false) root =
    Log.debug (fun l ->
        l "[state] v fresh=%b RO=%b root=%s" fresh readonly root );
    let root_f = root // "store.pack" in
    try
      let t = Hashtbl.find files root_f in
      if fresh then unsafe_clear t;
      t
    with Not_found ->
      let lock = Lwt_mutex.create () in
      let index =
        Index.v ~fresh ~read_only:readonly ~log_size:10_000_000
          ~fan_out_size:256 root
      in
      let dict = Dict.v ~fresh ~readonly root in
      let block = IO.v ~version:current_version ~readonly root_f in
      if fresh then if readonly then raise RO_Not_Allowed else IO.clear block;
      if IO.version block <> current_version then
        Fmt.failwith "invalid version: got %S, expecting %S" (IO.version block)
          current_version;
      let t = { block; index; lock; dict } in
      Hashtbl.add files root_f t;
      t

  let v ?fresh ?readonly root =
    Lwt_mutex.with_lock create (fun () ->
        let t = unsafe_v ?fresh ?readonly root in
        Lwt.return t )

  module Make (V : S with type hash := K.t) : sig
    include
      Irmin.CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

    val v :
      ?fresh:bool ->
      ?readonly:bool ->
      ?lru_size:int ->
      string ->
      [ `Read ] t Lwt.t

    val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

    val append : 'a t -> K.t -> V.t -> unit Lwt.t

    val unsafe_append : 'a t -> K.t -> V.t -> unit

    val unsafe_find : 'a t -> K.t -> V.t option
  end = struct
    module Tbl = Table (K)
    module Lru = Cache (K)

    type nonrec 'a t = { pack : 'a t; lru : V.t Lru.t; staging : V.t Tbl.t }

    type key = K.t

    type value = V.t

    let clear t = clear t.pack >|= fun () -> Tbl.clear t.staging

    let files = Hashtbl.create 10

    let create = Lwt_mutex.create ()

    let unsafe_v ?(fresh = false) ?(readonly = false) ?(lru_size = 10_000) root
        =
      Log.debug (fun l ->
          l "[pack] v fresh=%b RO=%b root=%s" fresh readonly root );
      try
        let t = Hashtbl.find files root in
        (if fresh then clear t else Lwt.return ()) >|= fun () -> t
      with Not_found ->
        v ~fresh ~readonly root >>= fun pack ->
        let staging = Tbl.create 127 in
        let lru = Lru.create lru_size in
        let t = { staging; lru; pack } in
        (if fresh then clear t else Lwt.return ()) >|= fun () ->
        Hashtbl.add files root t;
        t

    let v ?fresh ?readonly ?lru_size root =
      Lwt_mutex.with_lock create (fun () ->
          unsafe_v ?fresh ?readonly ?lru_size root )

    let pp_hash = Irmin.Type.pp K.t

    let unsafe_mem t k =
      Log.debug (fun l -> l "[pack] mem %a" pp_hash k);
      if Tbl.mem t.staging k then true
      else if Lru.mem t.lru k then true
      else Index.mem t.pack.index k

    let mem t k =
      Lwt_mutex.with_lock create (fun () ->
          let b = unsafe_mem t k in
          Lwt.return b )

    let check_key k v =
      let k' = V.hash v in
      if Irmin.Type.equal K.t k k' then ()
      else
        Fmt.failwith "corrupted value: got %a, expecting %a." pp_hash k'
          pp_hash k

    let buffer_for_hash = Bytes.create K.hash_size

    let io_read_and_decode ~off ~len t =
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      assert (n = len);
      let hash off =
        let n = IO.read t.pack.block ~off buffer_for_hash in
        assert (n = K.hash_size);
        let _, v =
          Irmin.Type.decode_bin ~headers:false K.t
            (* the copy is important here *)
            (Bytes.to_string buffer_for_hash)
            0
        in
        v
      in
      let dict = Dict.find t.pack.dict in
      V.decode_bin ~hash ~dict (Bytes.unsafe_to_string buf) 0

    let unsafe_find t k =
      Log.debug (fun l -> l "[pack] find %a" pp_hash k);
      stats.pack_finds <- succ stats.pack_finds;
      match Tbl.find t.staging k with
      | v ->
          Lru.add t.lru k v;
          Some v
      | exception Not_found -> (
        match Lru.find t.lru k with
        | v -> Some v
        | exception Not_found -> (
            stats.pack_cache_misses <- succ stats.pack_cache_misses;
            match Index.find t.pack.index k with
            | None -> None
            | Some (off, len, _) ->
                let v = io_read_and_decode ~off ~len t in
                check_key k v;
                Tbl.add t.staging k v;
                Lru.add t.lru k v;
                Some v ) )

    let find t k =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          let v = unsafe_find t k in
          Lwt.return v )

    let cast t = (t :> [ `Read | `Write ] t)

    let flush t =
      IO.sync (Dict.io t.pack.dict);
      Index.flush t.pack.index;
      IO.sync t.pack.block;
      Tbl.clear t.staging

    let batch t f =
      f (cast t) >>= fun r ->
      if Tbl.length t.staging = 0 then Lwt.return r
      else (
        flush t;
        Lwt.return r )

    let auto_flush = 1024

    let unsafe_append t k v =
      match unsafe_mem t k with
      | true -> ()
      | false ->
          Log.debug (fun l -> l "[pack] append %a" pp_hash k);
          let offset k =
            match Index.find t.pack.index k with
            | Some (off, _, _) ->
                stats.appended_offsets <- stats.appended_offsets + 1;
                Some off
            | None ->
                stats.appended_hashes <- stats.appended_hashes + 1;
                None
          in
          let dict = Dict.index t.pack.dict in
          let off = IO.offset t.pack.block in
          V.encode_bin ~offset ~dict v k (IO.append t.pack.block);
          let len = Int64.to_int (IO.offset t.pack.block -- off) in
          Index.replace t.pack.index k (off, len, V.magic);
          if Tbl.length t.staging >= auto_flush then flush t
          else Tbl.add t.staging k v;
          Lru.add t.lru k v

    let append t k v =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          unsafe_append t k v;
          Lwt.return () )

    let add t v =
      let k = V.hash v in
      append t k v >|= fun () -> k

    let unsafe_add t k v = append t k v
  end
end

let div_or_zero a b = if b = 0 then 0. else float_of_int a /. float_of_int b

type stats = {
  pack_cache_misses : float;
  offset_ratio : float;
  offset_significance : int
}

let stats () =
  { pack_cache_misses = div_or_zero stats.pack_cache_misses stats.pack_finds;
    offset_ratio =
      div_or_zero stats.appended_offsets
        (stats.appended_offsets + stats.appended_hashes);
    offset_significance = stats.appended_offsets + stats.appended_hashes
  }
