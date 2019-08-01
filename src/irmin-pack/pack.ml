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

module type ELT = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val magic : t -> char

  val encode_bin :
    dict:(string -> int option) ->
    offset:(hash -> int64 option) ->
    t ->
    hash ->
    (string -> unit) ->
    unit

  val decode_bin :
    dict:(int -> string option) -> hash:(int64 -> hash) -> string -> int -> t
end

module type S = sig
  include Irmin.CONTENT_ADDRESSABLE_STORE

  type index

  type 'a table

  val v :
    ?fresh:bool ->
    ?shared:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    staging_offsets:int64 table ->
    string ->
    [ `Read ] t Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val unsafe_append : 'a t -> key -> value -> unit

  val unsafe_mem : 'a t -> key -> bool

  val unsafe_find : 'a t -> key -> value option

  val sync : 'a t -> unit
end

module type MAKER = sig
  type key

  type index

  type 'a table

  module Make (V : ELT with type hash := key) :
    S
    with type key = key
     and type value = V.t
     and type index = index
     and type 'a table = 'a table
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

let with_cache = IO.with_cache

module IO = IO.Unix

module File (Index : Pack_index.S) (K : Irmin.Hash.S with type t = Index.key) =
struct
  module Tbl = Table (K)

  type index = Index.t

  type 'a table = 'a Tbl.t

  type 'a t = {
    block : IO.t;
    index : Index.t;
    dict : Dict.t;
    lock : Lwt_mutex.t;
    staging_offsets : int64 Tbl.t
  }

  let clear t =
    IO.clear t.block;
    Index.clear t.index;
    Dict.clear t.dict

  let unsafe_v ~index ~staging_offsets ~fresh ~shared:_ ~readonly file =
    let root = Filename.dirname file in
    let lock = Lwt_mutex.create () in
    let dict = Dict.v ~fresh ~readonly root in
    let block = IO.v ~fresh ~version:current_version ~readonly file in
    if IO.version block <> current_version then
      Fmt.failwith "invalid version: got %S, expecting %S" (IO.version block)
        current_version;
    { block; index; lock; dict; staging_offsets }

  let (`Staged v) =
    with_cache ~clear
      ~v:(fun (index, staging_offsets) -> unsafe_v ~index ~staging_offsets)
      "store.pack"

  type key = K.t

  module Make (V : ELT with type hash := K.t) = struct
    module Tbl = Table (K)
    module Lru = Cache (K)

    type 'a table = 'a Tbl.t

    type nonrec 'a t = { pack : 'a t; lru : V.t Lru.t; staging : V.t Tbl.t }

    type key = K.t

    type value = V.t

    type index = Index.t

    let clear t =
      clear t.pack;
      Tbl.clear t.staging;
      Tbl.clear t.pack.staging_offsets

    (* we need another cache here, as we want to share the LRU and
       staging caches too. *)

    let roots = Hashtbl.create 10

    let create = Lwt_mutex.create ()

    let unsafe_v_no_cache ~fresh ~readonly ~shared ~lru_size ~index
        ~staging_offsets root =
      let pack = v (index, staging_offsets) ~fresh ~shared ~readonly root in
      let staging = Tbl.create 127 in
      let lru = Lru.create lru_size in
      { staging; lru; pack }

    let unsafe_v ?(fresh = false) ?(shared = true) ?(readonly = false)
        ?(lru_size = 10_000) ~index ~staging_offsets root =
      if not shared then
        unsafe_v_no_cache ~fresh ~readonly ~shared ~lru_size ~index
          ~staging_offsets root
      else
        try
          let t = Hashtbl.find roots root in
          if fresh then clear t;
          t
        with Not_found ->
          let t =
            unsafe_v_no_cache ~fresh ~readonly ~shared ~lru_size ~index
              ~staging_offsets root
          in
          if fresh then clear t;
          Hashtbl.add roots root t;
          t

    let v ?fresh ?shared ?readonly ?lru_size ~index ~staging_offsets root =
      Lwt_mutex.with_lock create (fun () ->
          let t =
            unsafe_v ?fresh ?shared ?readonly ?lru_size ~index ~staging_offsets
              root
          in
          Lwt.return t )

    let pp_hash = Irmin.Type.pp K.t

    let io_read_and_decode_hash ~off t =
      let buf = Bytes.create K.hash_size in
      let n = IO.read t.pack.block ~off buf in
      assert (n = K.hash_size);
      let _, v =
        Irmin.Type.decode_bin ~headers:false K.t (Bytes.unsafe_to_string buf) 0
      in
      v

    let unsafe_mem t k =
      Log.debug (fun l -> l "[pack] mem %a" pp_hash k);
      if Tbl.mem t.staging k then true
      else if Lru.mem t.lru k then true
      else
        let rec loop = function
          | [] -> false
          | (off, _, _) :: tl ->
              let hash = io_read_and_decode_hash ~off t in
              if Irmin.Type.equal K.t k hash then true else loop tl
        in
        loop (Index.find_all t.pack.index k)

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

    let io_read_and_decode ~off ~len t =
      if not (IO.readonly t.pack.block) then
        assert (off <= IO.offset t.pack.block);
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      assert (n = len);
      let hash off = io_read_and_decode_hash ~off t in
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
        | exception Not_found ->
            stats.pack_cache_misses <- succ stats.pack_cache_misses;
            let rec loop = function
              | [] -> None
              | (off, len, _) :: tl ->
                  let hash = io_read_and_decode_hash ~off t in
                  if Irmin.Type.equal K.t k hash then (
                    let v = io_read_and_decode ~off ~len t in
                    check_key k v;
                    Lru.add t.lru k v;
                    Some v )
                  else loop tl
            in
            loop (Index.find_all t.pack.index k) )

    let find t k =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          let v = unsafe_find t k in
          Lwt.return v )

    let cast t = (t :> [ `Read | `Write ] t)

    let sync t =
      IO.sync (Dict.io t.pack.dict);
      Index.flush t.pack.index;
      IO.sync t.pack.block;
      Tbl.clear t.staging;
      Tbl.clear t.pack.staging_offsets

    let batch t f =
      f (cast t) >>= fun r ->
      if Tbl.length t.staging = 0 then Lwt.return r
      else (
        sync t;
        Lwt.return r )

    let auto_flush = 1024

    let unsafe_append t k v =
      match unsafe_mem t k with
      | true -> ()
      | false ->
          Log.debug (fun l -> l "[pack] append %a" pp_hash k);
          let offset k =
            match Tbl.find t.pack.staging_offsets k with
            | off -> Some off
            | exception Not_found ->
                let rec loop = function
                  | [] ->
                      stats.appended_hashes <- stats.appended_hashes + 1;
                      None
                  | (off, _, _) :: tl ->
                      stats.appended_offsets <- stats.appended_offsets + 1;
                      let hash = io_read_and_decode_hash ~off t in
                      if Irmin.Type.equal K.t hash k then Some off else loop tl
                in
                loop (Index.find_all t.pack.index k)
          in
          let dict = Dict.index t.pack.dict in
          let off = IO.offset t.pack.block in
          V.encode_bin ~offset ~dict v k (IO.append t.pack.block);
          let len = Int64.to_int (IO.offset t.pack.block -- off) in
          Index.add t.pack.index k (off, len, V.magic v);
          if Tbl.length t.staging >= auto_flush then sync t
          else (
            Tbl.add t.staging k v;
            Tbl.add t.pack.staging_offsets k off );
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
