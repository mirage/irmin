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

  val v :
    ?fresh:bool ->
    ?readonly:bool ->
    ?lru_size:int ->
    index:index ->
    string ->
    [ `Read ] t Lwt.t

  val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

  val unsafe_append : 'a t -> key -> value -> unit

  val unsafe_mem : 'a t -> key -> bool

  val unsafe_find : 'a t -> key -> value option

  val flush : ?index:bool -> 'a t -> unit

  val sync : 'a t -> unit

  type integrity_error = [ `Wrong_hash | `Absent_value ]

  val integrity_check :
    offset:int64 -> length:int -> key -> 'a t -> (unit, integrity_error) result

  val close : 'a t -> unit Lwt.t
end

module type MAKER = sig
  type key

  type index

  module Make (V : ELT with type hash := key) :
    S with type key = key and type value = V.t and type index = index
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
  module Dict = Pack_dict

  type index = Index.t

  type 'a t = {
    block : IO.t;
    index : Index.t;
    dict : Dict.t;
    lock : Lwt_mutex.t;
    mutable open_instances : int;
  }

  let clear t =
    IO.clear t.block;
    Index.clear t.index;
    Dict.clear t.dict

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false

  let unsafe_v ~index ~fresh ~readonly file =
    let root = Filename.dirname file in
    let lock = Lwt_mutex.create () in
    let dict = Dict.v ~fresh ~readonly root in
    let block = IO.v ~fresh ~version:current_version ~readonly file in
    if IO.version block <> current_version then
      Fmt.failwith "invalid version: got %S, expecting %S" (IO.version block)
        current_version;
    { block; index; lock; dict; open_instances = 1 }

  let (`Staged v) =
    with_cache ~clear ~valid ~v:(fun index -> unsafe_v ~index) "store.pack"

  type key = K.t

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block;
      Dict.close t.dict)

  module Make (V : ELT with type hash := K.t) = struct
    module Tbl = Table (K)
    module Lru = Cache (K)

    type nonrec 'a t = {
      pack : 'a t;
      lru : V.t Lru.t;
      staging : V.t Tbl.t;
      mutable open_instances : int;
    }

    type key = K.t

    type value = V.t

    type index = Index.t

    let clear t =
      clear t.pack;
      Tbl.clear t.staging

    (* we need another cache here, as we want to share the LRU and
       staging caches too. *)

    let roots = Hashtbl.create 10

    let create = Lwt_mutex.create ()

    let valid t =
      if t.open_instances <> 0 then (
        t.open_instances <- t.open_instances + 1;
        true)
      else false

    let flush ?(index = true) t =
      Dict.flush t.pack.dict;
      IO.flush t.pack.block;
      if index then Index.flush t.pack.index;
      Tbl.clear t.staging

    let unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index root =
      let pack = v index ~fresh ~readonly root in
      let staging = Tbl.create 127 in
      let lru = Lru.create lru_size in
      { staging; lru; pack; open_instances = 1 }

    let unsafe_v ?(fresh = false) ?(readonly = false) ?(lru_size = 10_000)
        ~index root =
      try
        let t = Hashtbl.find roots (root, readonly) in
        if valid t then (
          if fresh then clear t;
          t)
        else (
          Hashtbl.remove roots (root, readonly);
          raise Not_found)
      with Not_found ->
        let t = unsafe_v_no_cache ~fresh ~readonly ~lru_size ~index root in
        if fresh then clear t;
        Hashtbl.add roots (root, readonly) t;
        t

    let v ?fresh ?readonly ?lru_size ~index root =
      Lwt_mutex.with_lock create (fun () ->
          let t = unsafe_v ?fresh ?readonly ?lru_size ~index root in
          Lwt.return t)

    let pp_hash = Irmin.Type.pp K.t

    let decode_key = Irmin.Type.(unstage (decode_bin K.t))

    let io_read_and_decode_hash ~off t =
      let buf = Bytes.create K.hash_size in
      let n = IO.read t.pack.block ~off buf in
      assert (n = K.hash_size);
      let _, v = decode_key (Bytes.unsafe_to_string buf) 0 in
      v

    let unsafe_mem t k =
      Log.debug (fun l -> l "[pack] mem %a" pp_hash k);
      Tbl.mem t.staging k || Lru.mem t.lru k || Index.mem t.pack.index k

    let mem t k =
      Lwt_mutex.with_lock create (fun () ->
          let b = unsafe_mem t k in
          Lwt.return b)

    let check_key k v =
      let k' = V.hash v in
      if Irmin.Type.equal K.t k k' then Ok () else Error (k, k')

    exception Invalid_read

    let io_read_and_decode ~off ~len t =
      if (not (IO.readonly t.pack.block)) && off > IO.offset t.pack.block then
        raise Invalid_read;
      let buf = Bytes.create len in
      let n = IO.read t.pack.block ~off buf in
      if n <> len then raise Invalid_read;
      let hash off = io_read_and_decode_hash ~off t in
      let dict = Dict.find t.pack.dict in
      V.decode_bin ~hash ~dict (Bytes.unsafe_to_string buf) 0

    let unsafe_find t k =
      Log.debug (fun l -> l "[pack] find %a" pp_hash k);
      Stats.incr_finds ();
      match Tbl.find t.staging k with
      | v ->
          Lru.add t.lru k v;
          Some v
      | exception Not_found -> (
          match Lru.find t.lru k with
          | v -> Some v
          | exception Not_found -> (
              Stats.incr_cache_misses ();
              match Index.find t.pack.index k with
              | None -> None
              | Some (off, len, _) ->
                  let v = io_read_and_decode ~off ~len t in
                  (check_key k v |> function
                   | Ok () -> ()
                   | Error (expected, got) ->
                       Fmt.failwith "corrupted value: got %a, expecting %a."
                         pp_hash got pp_hash expected);
                  Lru.add t.lru k v;
                  Some v))

    let find t k =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          let v = unsafe_find t k in
          Lwt.return v)

    let cast t = (t :> [ `Read | `Write ] t)

    type integrity_error = [ `Wrong_hash | `Absent_value ]

    let integrity_check ~offset ~length k t =
      try
        let value = io_read_and_decode ~off:offset ~len:length t in
        match check_key k value with
        | Ok () -> Ok ()
        | Error _ -> Error `Wrong_hash
      with Invalid_read -> Error `Absent_value

    let batch t f =
      f (cast t) >>= fun r ->
      if Tbl.length t.staging = 0 then Lwt.return r
      else (
        flush t;
        Lwt.return r)

    let auto_flush = 1024

    let unsafe_append t k v =
      match unsafe_mem t k with
      | true -> ()
      | false ->
          Log.debug (fun l -> l "[pack] append %a" pp_hash k);
          let offset k =
            match Index.find t.pack.index k with
            | None ->
                Stats.incr_appended_hashes ();
                None
            | Some (off, _, _) ->
                Stats.incr_appended_offsets ();
                Some off
          in
          let dict = Dict.index t.pack.dict in
          let off = IO.offset t.pack.block in
          V.encode_bin ~offset ~dict v k (IO.append t.pack.block);
          let len = Int64.to_int (IO.offset t.pack.block -- off) in
          Index.add t.pack.index k (off, len, V.magic v);
          if Tbl.length t.staging >= auto_flush then flush t
          else Tbl.add t.staging k v;
          Lru.add t.lru k v

    let append t k v =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          unsafe_append t k v;
          Lwt.return_unit)

    let add t v =
      let k = V.hash v in
      append t k v >|= fun () -> k

    let unsafe_add t k v = append t k v

    let unsafe_close t =
      t.open_instances <- t.open_instances - 1;
      if t.open_instances = 0 then (
        Log.debug (fun l -> l "[pack] close %s" (IO.name t.pack.block));
        Tbl.clear t.staging;
        ignore (Lru.clear t.lru);
        close t.pack)

    let close t =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          unsafe_close t;
          Lwt.return_unit)

    let sync t =
      Dict.sync t.pack.dict;
      Index.sync t.pack.index
  end
end
