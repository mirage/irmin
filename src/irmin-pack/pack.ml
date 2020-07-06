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

module IO = IO.Unix
module I = Index

module File
    (Index : Pack_index.S)
    (K : Irmin.Hash.S with type t = Index.key)
    (Instance_pool : I.Cache.S) =
struct
  module Tbl = Table (K)
  module Dict = Pack_dict

  type index = Index.t

  type ro = [ `Read ]

  type rw = [ `Read | `Write ]

  type _ mode = RO : ro mode | RW : rw mode

  let pp_mode (type m) ppf (m : m mode) =
    match m with
    | RO -> Format.pp_print_string ppf "ro"
    | RW -> Format.pp_print_string ppf "rw"

  (** Wrap [Instance_pool] to allow the type of values to depend on the phantom
      type parameter in the key. *)
  module Modal_instance_pool (V : sig
    type 'a t
  end) : sig
    type 'm key := string * 'm mode

    type 'm value := 'm V.t

    type t

    val create : unit -> t

    val add : t -> 'm key -> 'm value -> unit

    val find : t -> 'm key -> 'm value option

    val remove : t -> _ key -> unit
  end = struct
    type cache = {
      readonly : (string, ro V.t) Instance_pool.t;
      readwrite : (string, rw V.t) Instance_pool.t;
    }

    let create () =
      {
        readonly = Instance_pool.create ();
        readwrite = Instance_pool.create ();
      }

    let add (type m) t (name, (mode : m mode)) (v : m V.t) =
      match mode with
      | RO -> Instance_pool.add t.readonly name v
      | RW -> Instance_pool.add t.readwrite name v

    let find (type m) t (name, (mode : m mode)) : m V.t option =
      match mode with
      | RO -> Instance_pool.find t.readonly name
      | RW -> Instance_pool.find t.readwrite name

    let remove (type m) t (name, (mode : m mode)) =
      match mode with
      | RO -> Instance_pool.remove t.readonly name
      | RW -> Instance_pool.remove t.readwrite name

    type t = cache
  end

  type _ t = {
    block : IO.t;
    index : Index.t;
    dict : Dict.t;
    lock : Lwt_mutex.t;
    mutable open_instances : int;
  }

  module Instance_pool = Modal_instance_pool (struct
    type nonrec 'a t = 'a t
  end)

  type cache = Instance_pool.t

  let empty_cache = Instance_pool.create

  let clear t =
    IO.clear t.block;
    Index.clear t.index;
    Dict.clear t.dict

  let v_no_cache (type m) ~index ~fresh ~(mode : m mode) file : m t =
    let readonly = match mode with RO -> true | RW -> false in
    let root = Filename.dirname file in
    let lock = Lwt_mutex.create () in
    let dict = Dict.v ~fresh ~readonly root in
    let block = IO.v ~fresh ~version:current_version ~readonly file in
    if IO.version block <> current_version then
      Fmt.failwith "invalid version: got %S, expecting %S" (IO.version block)
        current_version;
    { block; index; lock; dict; open_instances = 1 }

  let ( // ) = Filename.concat

  let v (type m) ~cache ~index ~fresh ~(mode : m mode) file : m t =
    let file = "store.pack" // file in
    let pool_key = (file, mode) in
    let new_instance () =
      Log.debug (fun l ->
          l "[%s] v fresh=%b mode=%a" (Filename.basename file) fresh pp_mode
            mode);
      let t = v_no_cache ~index ~fresh ~mode file in
      Instance_pool.add cache pool_key t;

      t
    in
    ( match (fresh, mode) with
    | true, RO -> invalid_arg "Read-only IO cannot be fresh"
    | _, _ -> () );

    match Instance_pool.find cache pool_key with
    | None -> new_instance ()
    | Some t ->
        Log.debug (fun l -> l "%s found in cache" file);

        if not (Sys.file_exists file) then (
          Log.debug (fun l ->
              l "[%s] does not exist anymore, cleaning up the fd cache"
                (Filename.basename file));
          Instance_pool.remove cache (file, RO);
          Instance_pool.remove cache (file, RW);
          new_instance () )
        else if t.open_instances = 0 then (
          Log.warn (fun l ->
              l "[%s] exists in cache, but doesn't have any open instances."
                (Filename.basename file));

          Instance_pool.remove cache pool_key;
          new_instance () )
        else (
          t.open_instances <- t.open_instances + 1;
          if fresh then clear t;
          t )

  type key = K.t

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.sync t.block;
      IO.close t.block;
      Dict.close t.dict )

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
        true )
      else false

    let unsafe_v ?(cache = empty_cache ()) ?(fresh = false) ?(mode = RW)
        ?(lru_size = 10_000) ~index root =
      let new_instance () =
        let t =
          let pack = v ~cache ~index ~fresh ~mode root in
          let staging = Tbl.create 127 in
          let lru = Lru.create lru_size in
          { staging; lru; pack; open_instances = 1 }
        in
        if fresh then clear t;
        Instance_pool.add cache (root, mode) t;
        t
      in
      match Instance_pool.find cache (root, mode) with
      | None -> new_instance ()
      | Some t ->
          if valid t then (
            if fresh then clear t;
            t )
          else (
            Hashtbl.remove roots (root, mode);
            new_instance () )

    let v ?cache ?fresh ?readonly ?lru_size ~index root =
      Lwt_mutex.with_lock create (fun () ->
          let t = unsafe_v ?cache ?fresh ?readonly ?lru_size ~index root in
          Lwt.return t)

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
                  Some v ) )

    let find t k =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          let v = unsafe_find t k in
          Lwt.return v)

    let cast t = (t :> [ `Read | `Write ] t)

    let sync t =
      Dict.sync t.pack.dict;
      IO.sync t.pack.block;
      Index.flush t.pack.index;
      Tbl.clear t.staging

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
        sync t;
        Lwt.return r )

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
          if Tbl.length t.staging >= auto_flush then sync t
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
        close t.pack )

    let close t =
      Lwt_mutex.with_lock t.pack.lock (fun () ->
          unsafe_close t;
          Lwt.return_unit)
  end
end
