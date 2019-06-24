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

open Lwt.Infix

let src = Logs.Src.create "irmin.pack" ~doc:"Irmin in-memory store"

module Log = (val Logs.src_log src : Logs.LOG)

let fresh_key =
  Irmin.Private.Conf.key ~doc:"Start with a fresh disk." "fresh"
    Irmin.Private.Conf.bool false

let fresh config = Irmin.Private.Conf.get config fresh_key

let root_key = Irmin.Private.Conf.root

let root config =
  match Irmin.Private.Conf.get config root_key with
  | None -> failwith "no root set"
  | Some r -> r

let config ?(fresh = false) root =
  let config = Irmin.Private.Conf.empty in
  let config = Irmin.Private.Conf.add config fresh_key fresh in
  let config = Irmin.Private.Conf.add config root_key (Some root) in
  config

let ( // ) = Filename.concat

let ( ++ ) = Int64.add

let ( -- ) = Int64.sub

module type IO = sig
  type t

  val v : string -> t

  val rename : src:t -> dst:t -> unit

  val clear : t -> unit

  val append : t -> string -> unit

  val set : t -> off:int64 -> string -> unit

  val read : t -> off:int64 -> bytes -> unit

  val offset : t -> int64

  val sync : t -> unit
end

module IO : IO = struct
  module Raw = struct
    type t = { fd : Unix.file_descr; mutable cursor : int64 }

    let v fd = { fd; cursor = 0L }

    let really_write fd buf =
      let rec aux off len =
        let w = Unix.write fd buf off len in
        if w = 0 then () else (aux [@tailcall]) (off + w) (len - w)
      in
      (aux [@tailcall]) 0 (Bytes.length buf)

    let really_read fd buf =
      let rec aux off len =
        let r = Unix.read fd buf off len in
        if r = 0 || r = len then off + r
        else (aux [@tailcall]) (off + r) (len - r)
      in
      (aux [@tailcall]) 0 (Bytes.length buf)

    let lseek t off =
      if off = t.cursor then ()
      else
        let _ = Unix.LargeFile.lseek t.fd off Unix.SEEK_SET in
        t.cursor <- off

    let unsafe_write t ~off buf =
      lseek t off;
      let buf = Bytes.unsafe_of_string buf in
      really_write t.fd buf;
      t.cursor <- off ++ Int64.of_int (Bytes.length buf)

    let unsafe_read t ~off buf =
      lseek t off;
      let n = really_read t.fd buf in
      t.cursor <- off ++ Int64.of_int n

    let unsafe_set_offset fd n =
      let buf = Irmin.Type.(to_bin_string int64) n in
      unsafe_write fd ~off:0L buf

    let unsafe_get_offset fd =
      let buf = Bytes.create 8 in
      unsafe_read fd ~off:0L buf;
      match Irmin.Type.(of_bin_string int64) (Bytes.unsafe_to_string buf) with
      | Ok t -> t
      | Error (`Msg e) -> Fmt.failwith "get_offset: %s" e
  end

  type t = {
    file : string;
    mutable raw : Raw.t;
    mutable offset : int64;
    mutable flushed : int64;
    buf : Buffer.t
  }

  let header = 8L

  let sync t =
    Log.debug (fun l -> l "IO sync %s" t.file);
    let buf = Buffer.contents t.buf in
    let offset = t.offset in
    Buffer.clear t.buf;
    if buf = "" then ()
    else (
      Raw.unsafe_write t.raw ~off:t.flushed buf;
      Raw.unsafe_set_offset t.raw offset;
      (* concurrent append might happen so here t.offset might differ
         from offset *)
      if not (t.flushed ++ Int64.of_int (String.length buf) = header ++ offset)
      then
        Fmt.failwith "sync error: %s flushed=%Ld offset+header=%Ld\n%!" t.file
          t.flushed (offset ++ header);
      t.flushed <- offset ++ header )

  let rename ~src ~dst =
    sync src;
    Unix.close dst.raw.fd;
    Log.debug (fun l -> l "IO rename %s => %s" src.file dst.file);
    Unix.rename src.file dst.file;
    dst.offset <- src.offset;
    dst.flushed <- src.flushed;
    dst.raw <- src.raw

  let auto_flush_limit = 100_000L

  let append t buf =
    Buffer.add_string t.buf buf;
    let len = Int64.of_int (String.length buf) in
    t.offset <- t.offset ++ len;
    if t.offset -- t.flushed > auto_flush_limit then sync t

  let set t ~off buf =
    sync t;
    Raw.unsafe_write t.raw ~off:(header ++ off) buf;
    let len = Int64.of_int (String.length buf) in
    let off = header ++ off ++ len in
    assert (off <= t.flushed)

  let read t ~off buf =
    assert (header ++ off <= t.flushed);
    Raw.unsafe_read t.raw ~off:(header ++ off) buf

  let offset t = t.offset

  (*  let file t = t.file *)

  let protect_unix_exn = function
    | Unix.Unix_error _ as e -> failwith (Printexc.to_string e)
    | e -> raise e

  let ignore_enoent = function
    | Unix.Unix_error (Unix.ENOENT, _, _) -> ()
    | e -> raise e

  let protect f x = try f x with e -> protect_unix_exn e

  let safe f x = try f x with e -> ignore_enoent e

  let mkdir dirname =
    let rec aux dir k =
      if Sys.file_exists dir && Sys.is_directory dir then k ()
      else (
        if Sys.file_exists dir then safe Unix.unlink dir;
        (aux [@tailcall]) (Filename.dirname dir) @@ fun () ->
        protect (Unix.mkdir dir) 0o755 )
    in
    (aux [@tailcall]) dirname (fun () -> ())

  let clear t =
    t.offset <- 0L;
    t.flushed <- header;
    Buffer.clear t.buf

  let buffers = Hashtbl.create 256

  let buffer file =
    try
      let buf = Hashtbl.find buffers file in
      Buffer.clear buf;
      buf
    with Not_found ->
      let buf = Buffer.create (4 * 1024) in
      Hashtbl.add buffers file buf;
      buf

  let v file =
    let v ~offset raw =
      { file; offset; raw; buf = buffer file; flushed = header ++ offset }
    in
    mkdir (Filename.dirname file);
    match Sys.file_exists file with
    | false ->
        let x = Unix.openfile file Unix.[ O_CREAT; O_RDWR ] 0o644 in
        let raw = Raw.v x in
        Raw.unsafe_set_offset raw 0L;
        v ~offset:0L raw
    | true ->
        let x = Unix.openfile file Unix.[ O_EXCL; O_RDWR ] 0o644 in
        let raw = Raw.v x in
        let offset = Raw.unsafe_get_offset raw in
        v ~offset raw
end

module Lru (H : Hashtbl.HashedType) = struct
  (* Extracted from https://github.com/pqwy/lru
     Copyright (c) 2016 David Kaloper Meršinjak *)

  module HT = Hashtbl.Make (H)

  module Q = struct
    type 'a node = {
      value : 'a;
      mutable next : 'a node option;
      mutable prev : 'a node option
    }

    type 'a t = {
      mutable first : 'a node option;
      mutable last : 'a node option
    }

    let clear t =
      t.first <- None;
      t.last <- None

    let detach t n =
      let np = n.prev and nn = n.next in
      ( match np with
      | None -> t.first <- nn
      | Some x ->
          x.next <- nn;
          n.prev <- None );
      match nn with
      | None -> t.last <- np
      | Some x ->
          x.prev <- np;
          n.next <- None

    let append t n =
      let on = Some n in
      match t.last with
      | Some x as l ->
          x.next <- on;
          t.last <- on;
          n.prev <- l
      | None ->
          t.first <- on;
          t.last <- on

    let node x = { value = x; prev = None; next = None }

    let create () = { first = None; last = None }
  end

  module M = struct
    type key = HT.key

    type 'a t = {
      ht : (key * 'a) Q.node HT.t;
      q : (key * 'a) Q.t;
      mutable cap : int;
      mutable w : int
    }

    let weight t = t.w

    let create cap = { cap; w = 0; ht = HT.create cap; q = Q.create () }

    let drop_lru t =
      match t.q.Q.first with
      | None -> ()
      | Some ({ Q.value = k, _; _ } as n) ->
          t.w <- t.w - 1;
          HT.remove t.ht k;
          Q.detach t.q n

    let rec trim t =
      if weight t > t.cap then (
        drop_lru t;
        trim t )

    let remove k t =
      try
        let n = HT.find t.ht k in
        t.w <- t.w - 1;
        HT.remove t.ht k;
        Q.detach t.q n
      with Not_found -> ()

    let clear t =
      HT.clear t.ht;
      Q.clear t.q

    let add k v t =
      remove k t;
      let n = Q.node (k, v) in
      t.w <- t.w + 1;
      HT.add t.ht k n;
      Q.append t.q n

    let promote k t =
      try
        let n = HT.find t.ht k in
        Q.(
          detach t.q n;
          append t.q n)
      with Not_found -> ()

    let find k t =
      try Some (snd (HT.find t.ht k).Q.value) with Not_found -> None

    let mem k t = HT.mem t.ht k
  end

  include M

  let create x = M.create x

  let add t k v =
    M.add k v t;
    M.trim t

  let find t k =
    match M.find k t with
    | None -> raise Not_found
    | Some v ->
        M.promote k t;
        v

  let mem t k =
    match M.mem k t with
    | false -> false
    | true ->
        M.promote k t;
        true
end

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t

  let hash (t : t) = Irmin.Type.short_hash K.t t

  let equal (x : t) (y : t) = Irmin.Type.equal K.t x y
end)

module Cache (K : Irmin.Type.S) = Lru (struct
  type t = K.t

  let hash (t : t) = Irmin.Type.short_hash K.t t

  let equal (x : t) (y : t) = Irmin.Type.equal K.t x y
end)

module Pool : sig
  type t

  val v : length:int -> lru_size:int -> IO.t -> t

  val read : t -> off:int64 -> len:int -> bytes * int

  val clear : t -> unit
end = struct
  module Lru = Lru (struct
    include Int64

    let hash = Hashtbl.hash
  end)

  type t = {
    mutable pages : bytes Lru.t;
    length : int;
    lru_size : int;
    io : IO.t
  }

  let v ~length ~lru_size io =
    let pages = Lru.create lru_size in
    { pages; length; io; lru_size }

  let rec read t ~off ~len =
    let l = Int64.of_int t.length in
    let page_off = Int64.(mul (div off l) l) in
    let ioff = Int64.to_int (off -- page_off) in
    match Lru.M.find page_off t.pages with
    | Some buf ->
        if t.length - ioff < len then (
          Lru.M.remove page_off t.pages;
          (read [@tailcall]) t ~off ~len )
        else (
          Lru.M.promote page_off t.pages;
          Lru.M.trim t.pages;
          (buf, ioff) )
    | None ->
        let length = max t.length (ioff + len) in
        let length =
          if page_off ++ Int64.of_int length > IO.offset t.io then
            Int64.to_int (IO.offset t.io -- page_off)
          else length
        in
        let buf = Bytes.create length in
        IO.read t.io ~off:page_off buf;
        Lru.M.add page_off buf t.pages;
        Lru.M.trim t.pages;
        (buf, ioff)

  let clear t = Lru.M.clear t.pages
end

module Dict = struct
  type t = {
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    block : IO.t;
    lock : Lwt_mutex.t
  }

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = Irmin.Type.(to_bin_string int32 len) ^ v in
    IO.append t.block buf

  let unsafe_index t v =
    Log.debug (fun l -> l "[dict] index %S" v);
    try Lwt.return (Hashtbl.find t.cache v)
    with Not_found ->
      let id = Hashtbl.length t.cache in
      append_string t v;
      Hashtbl.add t.cache v id;
      Hashtbl.add t.index id v;
      Lwt.return id

  let index t v = Lwt_mutex.with_lock t.lock (fun () -> unsafe_index t v)

  let find t id =
    Log.debug (fun l -> l "[dict] find %d" id);
    let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
    Lwt.return v

  let clear t =
    IO.clear t.block;
    Hashtbl.clear t.cache;
    Hashtbl.clear t.index

  let files = Hashtbl.create 10

  let create = Lwt_mutex.create ()

  let unsafe_v ?(fresh = false) root =
    let root = root // "store.dict" in
    Log.debug (fun l -> l "[dict] v fresh=%b root=%s" fresh root);
    try
      let t = Hashtbl.find files root in
      if fresh then clear t;
      t
    with Not_found ->
      let block = IO.v root in
      if fresh then IO.clear block;
      let cache = Hashtbl.create 997 in
      let index = Hashtbl.create 997 in
      let len = Int64.to_int (IO.offset block) in
      let raw = Bytes.create len in
      IO.read block ~off:0L raw;
      let raw = Bytes.unsafe_to_string raw in
      let rec aux n offset k =
        if offset >= len then k ()
        else
          let _, v = Irmin.Type.(decode_bin int32) raw offset in
          let len = Int32.to_int v in
          let v = String.sub raw (offset + 4) len in
          Hashtbl.add cache v n;
          Hashtbl.add index n v;
          (aux [@tailcall]) (n + 1) (offset + 4 + len) k
      in
      (aux [@tailcall]) 0 0 @@ fun () ->
      let t = { index; cache; block; lock = Lwt_mutex.create () } in
      Hashtbl.add files root t;
      t

  let v ?fresh root =
    Lwt_mutex.with_lock create (fun () ->
        let t = unsafe_v ?fresh root in
        Lwt.return t )
end

module Index (H : Irmin.Hash.S) = struct
  type entry = { hash : H.t; offset : int64; len : int }

  let offset_size = 64 / 8

  let length_size = 32 / 8

  let pp_hash = Irmin.Type.pp H.t

  let pad = H.hash_size + offset_size + length_size

  (* last allowed offset *)
  let log_size = 500_000 * pad

  let log_sizeL = Int64.of_int log_size

  let entry = Irmin.Type.(triple H.t int64 int32)

  let decode_entry buf off =
    let _, (hash, offset, len) =
      Irmin.Type.decode_bin entry (Bytes.unsafe_to_string buf) off
    in
    { hash; offset; len = Int32.to_int len }

  let encode_entry { hash; offset; len } =
    let len = Int32.of_int len in
    Irmin.Type.to_bin_string entry (hash, offset, len)

  module Tbl = Table (H)

  let lru_size = 3_000

  let page_size = 10_000 * pad

  let fan_out_size = 256

  type t = {
    cache : entry Tbl.t;
    pages : Pool.t array;
    offsets : (int64, entry) Hashtbl.t;
    log : IO.t;
    index : IO.t array;
    entries : H.t Bloomf.t;
    root : string;
    lock : Lwt_mutex.t
  }

  let clear t =
    IO.clear t.log;
    let (_ : unit list) =
      List.init (Array.length t.index) (fun i -> IO.clear t.index.(i))
    in
    Bloomf.clear t.entries;
    Array.iter (fun p -> Pool.clear p) t.pages;
    Tbl.clear t.cache;
    Hashtbl.clear t.offsets

  let files = Hashtbl.create 10

  let create = Lwt_mutex.create ()

  let log_path root = root // "store.log"

  let index_path root = root // "store.index"

  let map_io f io =
    let max_offset = IO.offset io in
    let rec aux offset =
      if offset >= max_offset then ()
      else
        let raw = Bytes.create page_size in
        IO.read io ~off:offset raw;
        let page = Bytes.unsafe_to_string raw in
        let rec read_page page off =
          if off = page_size then ()
          else
            let _, (hash, offset, len) =
              Irmin.Type.decode_bin entry page off
            in
            f { hash; offset; len = Int32.to_int len };
            (read_page [@tailcall]) page (off + pad)
        in
        let () = read_page page 0 in
        (aux [@tailcall]) (offset ++ Int64.of_int page_size)
    in
    (aux [@tailcall]) 0L

  let unsafe_v ?(fresh = false) root =
    let log_path = log_path root in
    let index_path = index_path root in
    Log.debug (fun l ->
        l "[index] v fresh=%b log=%s index=%s" fresh log_path index_path );
    try
      let t = Hashtbl.find files root in
      if fresh then clear t;
      t
    with Not_found ->
      let entries = Bloomf.create ~error_rate:0.01 100_000_000 in
      let cache = Tbl.create log_size in
      let log = IO.v log_path in
      let index =
        Array.init fan_out_size (fun i ->
            let index_path = Printf.sprintf "%s.%d" index_path i in
            let index = IO.v index_path in
            if fresh then IO.clear index;
            map_io (fun e -> Bloomf.add entries e.hash) index;
            index )
      in
      if fresh then IO.clear log;
      map_io
        (fun e ->
          Tbl.add cache e.hash e;
          Bloomf.add entries e.hash )
        log;
      let t =
        { cache;
          root;
          offsets = Hashtbl.create 127;
          pages =
            Array.init fan_out_size (fun i ->
                Pool.v ~length:page_size ~lru_size index.(i) );
          log;
          index;
          lock = Lwt_mutex.create ();
          entries
        }
      in
      Hashtbl.add files root t;
      t

  let v ?fresh root =
    Lwt_mutex.with_lock create (fun () ->
        let t = unsafe_v ?fresh root in
        Lwt.return t )

  let get_entry t i off =
    let page, ioff = Pool.read t.pages.(i) ~off ~len:pad in
    decode_entry page ioff

  let padf = float_of_int pad

  let get_entry_iff_needed t i off = function
    | Some e -> e
    | None -> get_entry t i (Int64.of_float off)

  let interpolation_search t i key =
    let hashed_key = H.short_hash key in
    Log.debug (fun l -> l "interpolation_search %a (%d)" pp_hash key hashed_key);
    let hashed_key = float_of_int hashed_key in
    let low = 0. in
    let high = Int64.to_float (IO.offset t.index.(i)) -. padf in
    let rec search low high lowest_entry highest_entry =
      let lowest_entry = get_entry_iff_needed t i low lowest_entry in
      let highest_entry = get_entry_iff_needed t i high highest_entry in
      if high = low then
        if Irmin.Type.equal H.t lowest_entry.hash key then Some lowest_entry
        else None
      else
        let lowest_hash = float_of_int (H.short_hash lowest_entry.hash) in
        let highest_hash = float_of_int (H.short_hash highest_entry.hash) in
        if high < low || lowest_hash > hashed_key || highest_hash < hashed_key
        then None
        else
          let doff =
            floor
              ( (high -. low)
              *. (hashed_key -. lowest_hash)
              /. (highest_hash -. lowest_hash) )
          in
          let off = low +. doff -. mod_float doff padf in
          let offL = Int64.of_float off in
          let e = get_entry t i offL in
          if Irmin.Type.equal H.t e.hash key then Some e
          else if float_of_int (H.short_hash e.hash) < hashed_key then
            (search [@tailcall]) (off +. padf) high None (Some highest_entry)
          else (search [@tailcall]) low (off -. padf) (Some lowest_entry) None
    in
    if high < 0. then None else (search [@tailcall]) low high None None

  (*  let dump_entry ppf e = Fmt.pf ppf "[offset:%Ld len:%d]" e.offset e.len *)

  let unsafe_find t key =
    Log.debug (fun l -> l "[index] find %a" pp_hash key);
    if not (Bloomf.mem t.entries key) then None
    else
      match Tbl.find t.cache key with
      | e -> Some e
      | exception Not_found ->
          let i = H.short_hash key land (fan_out_size - 1) in
          interpolation_search t i key

  let find t key =
    Lwt_mutex.with_lock t.lock (fun () ->
        let v = unsafe_find t key in
        Lwt.return v )

  let unsafe_mem t key =
    if not (Bloomf.mem t.entries key) then false
    else match unsafe_find t key with None -> false | Some _ -> true

  let mem t key =
    Lwt_mutex.with_lock t.lock (fun () ->
        let b = unsafe_mem t key in
        Lwt.return b )

  let append_entry t e = IO.append t (encode_entry e)

  module HashMap = Map.Make (struct
    type t = H.t

    let compare a b = compare (H.short_hash a) (H.short_hash b)
  end)

  let fan_out_cache t n =
    let caches = Array.make n [] in
    let () =
      Tbl.iter
        (fun k v ->
          let index = H.short_hash k land (n - 1) in
          caches.(index) <- (k, v) :: caches.(index) )
        t.cache
    in
    Array.map
      (List.sort (fun (k, _) (k', _) ->
           compare (H.short_hash k) (H.short_hash k') ))
      caches

  let merge_with log t i tmp =
    let offset = ref 0L in
    let get_index_entry = function
      | Some e -> Some e
      | None ->
          if !offset >= IO.offset t.index.(i) then None
          else
            let e = get_entry t i !offset in
            offset := !offset ++ Int64.of_int pad;
            Some e
    in
    let rec go last_read l =
      match get_index_entry last_read with
      | None -> List.iter (fun (_, e) -> append_entry tmp e) l
      | Some e -> (
        match l with
        | (k, v) :: r ->
            let last, rst =
              if Irmin.Type.equal H.t e.hash k then (
                append_entry tmp e;
                (None, r) )
              else
                let hashed_e = H.short_hash e.hash in
                let hashed_k = H.short_hash k in
                if hashed_e = hashed_k then (
                  append_entry tmp e;
                  append_entry tmp v;
                  (None, r) )
                else if hashed_e < hashed_k then (
                  append_entry tmp e;
                  (None, l) )
                else (
                  append_entry tmp v;
                  (Some e, r) )
            in
            if !offset >= IO.offset t.index.(i) && last = None then
              List.iter (fun (_, e) -> append_entry tmp e) rst
            else (go [@tailcall]) last rst
        | [] ->
            append_entry tmp e;
            if !offset >= IO.offset t.index.(i) then ()
            else
              let len = IO.offset t.index.(i) -- !offset in
              let buf = Bytes.create (Int64.to_int len) in
              IO.read t.index.(i) ~off:!offset buf;
              IO.append tmp (Bytes.unsafe_to_string buf) )
    in
    (go [@tailcall]) None log

  let merge t =
    IO.sync t.log;
    let log = fan_out_cache t fan_out_size in
    let tmp_path = t.root // "store.index.tmp" in
    let (_ : unit list) =
      List.init fan_out_size (fun i ->
          let tmp_path = Format.sprintf "%s.%d" tmp_path i in
          let tmp = IO.v tmp_path in
          merge_with log.(i) t i tmp;
          IO.rename ~src:tmp ~dst:t.index.(i) )
    in
    (* reset the log *)
    IO.clear t.log;
    Tbl.clear t.cache;
    Array.iter (fun p -> Pool.clear p) t.pages;
    Hashtbl.clear t.offsets

  (* do not check for duplicates *)
  let append t key ~off ~len =
    Log.debug (fun l ->
        l "[index] append %a off=%Ld len=%d" pp_hash key off len );
    let entry = { hash = key; offset = off; len } in
    append_entry t.log entry;
    Tbl.add t.cache key entry;
    Hashtbl.add t.offsets entry.offset entry;
    Bloomf.add t.entries key;
    if Int64.compare (IO.offset t.log) log_sizeL > 0 then merge t
end

module type S = sig
  include Irmin.Type.S

  type hash

  val hash : t -> hash

  val to_bin :
    dict:(string -> int Lwt.t) ->
    offset:(hash -> int64 option Lwt.t) ->
    t ->
    hash ->
    string Lwt.t

  val decode_bin :
    dict:(int -> string option Lwt.t) ->
    hash:(int64 -> hash Lwt.t) ->
    string ->
    int ->
    t Lwt.t
end

module Pack (K : Irmin.Hash.S) = struct
  module Index = Index (K)
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

  let unsafe_v ?(fresh = false) root =
    Log.debug (fun l -> l "[state] v fresh=%b root=%s" fresh root);
    let root_f = root // "store.pack" in
    try
      let t = Hashtbl.find files root_f in
      if fresh then unsafe_clear t;
      t
    with Not_found ->
      let lock = Lwt_mutex.create () in
      let index = Index.unsafe_v ~fresh root in
      let dict = Dict.unsafe_v ~fresh root in
      let block = IO.v root_f in
      if fresh then IO.clear block;
      let t = { block; index; lock; dict } in
      Hashtbl.add files root_f t;
      t

  let v ?fresh root =
    Lwt_mutex.with_lock create (fun () ->
        let t = unsafe_v ?fresh root in
        Lwt.return t )

  module Make (V : S with type hash := K.t) : sig
    include
      Irmin.CONTENT_ADDRESSABLE_STORE with type key = K.t and type value = V.t

    val v : ?fresh:bool -> string -> [ `Read ] t Lwt.t

    val batch : [ `Read ] t -> ([ `Read | `Write ] t -> 'a Lwt.t) -> 'a Lwt.t

    val append : 'a t -> K.t -> V.t -> unit Lwt.t
  end = struct
    module Tbl = Table (K)
    module Lru = Cache (K)

    let lru_size = 30_000

    let page_size = 4 * 1024

    type nonrec 'a t = {
      pack : 'a t;
      lru : V.t Lru.t;
      staging : V.t Tbl.t;
      pages : Pool.t
    }

    type key = K.t

    type value = V.t

    let clear t = clear t.pack >|= fun () -> Tbl.clear t.staging

    let files = Hashtbl.create 10

    let create = Lwt_mutex.create ()

    let unsafe_v ?(fresh = false) root =
      Log.debug (fun l -> l "[pack] v fresh=%b root=%s" fresh root);
      try
        let t = Hashtbl.find files root in
        (if fresh then clear t else Lwt.return ()) >|= fun () -> t
      with Not_found ->
        v ~fresh root >>= fun pack ->
        let staging = Tbl.create 127 in
        let lru = Lru.create 10_000 in
        let t =
          { staging;
            lru;
            pack;
            pages = Pool.v ~lru_size ~length:page_size pack.block
          }
        in
        (if fresh then clear t else Lwt.return ()) >|= fun () ->
        Hashtbl.add files root t;
        t

    let v ?fresh root =
      Lwt_mutex.with_lock create (fun () -> unsafe_v ?fresh root)

    let pp_hash = Irmin.Type.pp K.t

    let mem t k =
      Log.debug (fun l -> l "[pack] mem %a" pp_hash k);
      if Tbl.mem t.staging k then Lwt.return true
      else if Lru.mem t.lru k then Lwt.return true
      else Index.mem t.pack.index k

    let check_key k v =
      let k' = V.hash v in
      if Irmin.Type.equal K.t k k' then Lwt.return ()
      else
        Fmt.kstrf Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a."
          pp_hash k' pp_hash k

    let unsafe_find t k =
      Log.debug (fun l -> l "[pack] find %a" pp_hash k);
      match Tbl.find t.staging k with
      | v ->
          Lru.add t.lru k v;
          Lwt.return (Some v)
      | exception Not_found -> (
        match Lru.find t.lru k with
        | v -> Lwt.return (Some v)
        | exception Not_found -> (
            Index.find t.pack.index k >>= function
            | None -> Lwt.return None
            | Some e ->
                let buf, pos = Pool.read t.pages ~off:e.offset ~len:e.len in
                let hash off =
                  match Hashtbl.find t.pack.index.offsets off with
                  | e -> Lwt.return e.hash
                  | exception Not_found ->
                      let buf, pos = Pool.read t.pages ~off ~len:K.hash_size in
                      let _, v =
                        Irmin.Type.decode_bin ~headers:false K.t
                          (Bytes.unsafe_to_string buf)
                          pos
                      in
                      Lwt.return v
                in
                let dict = Dict.find t.pack.dict in
                V.decode_bin ~hash ~dict (Bytes.unsafe_to_string buf) pos
                >>= fun v ->
                check_key k v >|= fun () ->
                Tbl.add t.staging k v;
                Lru.add t.lru k v;
                Some v ) )

    let find t k = Lwt_mutex.with_lock t.pack.lock (fun () -> unsafe_find t k)

    let cast t = (t :> [ `Read | `Write ] t)

    let batch t f =
      f (cast t) >>= fun r ->
      if Tbl.length t.staging = 0 then Lwt.return r
      else (
        IO.sync t.pack.dict.block;
        IO.sync t.pack.index.log;
        IO.sync t.pack.block;
        Tbl.clear t.staging;
        Pool.clear t.pages;
        Lwt.return r )

    let unsafe_append t k v =
      mem t k >>= function
      | true -> Lwt.return ()
      | false ->
          Log.debug (fun l -> l "[pack] append %a" pp_hash k);
          let offset k =
            Index.find t.pack.index k >|= function
            | Some e -> Some e.offset
            | None -> None
          in
          let dict = Dict.index t.pack.dict in
          V.to_bin ~offset ~dict v k >>= fun buf ->
          let off = IO.offset t.pack.block in
          IO.append t.pack.block buf;
          Index.append t.pack.index k ~off ~len:(String.length buf);
          Tbl.add t.staging k v;
          Lru.add t.lru k v;
          Lwt.return ()

    let append t k v =
      Lwt_mutex.with_lock t.pack.lock (fun () -> unsafe_append t k v)

    let add t v =
      let k = V.hash v in
      append t k v >|= fun () -> k
  end
end

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) = struct
  module Tbl = Table (K)
  module W = Irmin.Private.Watch.Make (K) (V)

  type key = K.t

  type value = V.t

  type watch = W.watch

  type t = {
    index : int64 Tbl.t;
    cache : V.t Tbl.t;
    block : IO.t;
    lock : Lwt_mutex.t;
    w : W.t
  }

  let read_length32 ~off block =
    let page = Bytes.create 4 in
    IO.read block ~off page;
    let n, v = Irmin.Type.(decode_bin int32) (Bytes.unsafe_to_string page) 0 in
    assert (n = 4);
    Int32.to_int v

  let entry = Irmin.Type.(pair (string_of `Int32) V.t)

  let set_entry t ?off k v =
    let k = Irmin.Type.to_bin_string K.t k in
    let buf = Irmin.Type.to_bin_string entry (k, v) in
    match off with
    | None -> IO.append t.block buf
    | Some off -> IO.set t.block buf ~off

  let pp_branch = Irmin.Type.pp K.t

  let unsafe_find t k =
    Log.debug (fun l -> l "[branches] find %a" pp_branch k);
    try Lwt.return (Some (Tbl.find t.cache k))
    with Not_found -> Lwt.return None

  let find t k = Lwt_mutex.with_lock t.lock (fun () -> unsafe_find t k)

  let unsafe_mem t k =
    Log.debug (fun l -> l "[branches] mem %a" pp_branch k);
    try Lwt.return (Tbl.mem t.cache k) with Not_found -> Lwt.return false

  let mem t v = Lwt_mutex.with_lock t.lock (fun () -> unsafe_mem t v)

  let zero =
    match Irmin.Type.of_bin_string V.t (String.make V.hash_size '\000') with
    | Ok x -> x
    | Error _ -> assert false

  let unsafe_remove t k =
    Tbl.remove t.cache k;
    try
      let off = Tbl.find t.index k in
      set_entry t ~off k zero
    with Not_found -> ()

  let remove t k =
    Log.debug (fun l -> l "[branches] remove %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () ->
        unsafe_remove t k;
        Lwt.return () )
    >>= fun () -> W.notify t.w k None

  let clear t =
    W.clear t.w >|= fun () ->
    IO.clear t.block;
    Tbl.clear t.cache;
    Tbl.clear t.index

  let files = Hashtbl.create 10

  let create = Lwt_mutex.create ()

  let watches = W.v ()

  let unsafe_v ?(fresh = false) root =
    let root = root // "store.branches" in
    Log.debug (fun l -> l "[branches] v fresh=%b root=%s" fresh root);
    try
      let t = Hashtbl.find files root in
      (if fresh then clear t else Lwt.return ()) >|= fun () -> t
    with Not_found ->
      let block = IO.v root in
      if fresh then IO.clear block;
      let cache = Tbl.create 997 in
      let index = Tbl.create 997 in
      let len = IO.offset block in
      let rec aux offset k =
        if offset >= len then k ()
        else
          let len = read_length32 ~off:offset block in
          let buf = Bytes.create (len + V.hash_size) in
          let off = offset ++ 4L in
          IO.read block ~off buf;
          let buf = Bytes.unsafe_to_string buf in
          let h =
            let h = String.sub buf 0 len in
            match Irmin.Type.of_bin_string K.t h with
            | Ok k -> k
            | Error (`Msg e) -> failwith e
          in
          let n, v = Irmin.Type.decode_bin V.t buf len in
          assert (n = len + V.hash_size);
          if not (Irmin.Type.equal V.t v zero) then Tbl.add cache h v;
          Tbl.add index h offset;
          (aux [@tailcall]) (off ++ Int64.(of_int @@ (len + V.hash_size))) k
      in
      (aux [@tailcall]) 0L @@ fun () ->
      let t =
        { cache; index; block; w = watches; lock = Lwt_mutex.create () }
      in
      Hashtbl.add files root t;
      Lwt.return t

  let v ?fresh root =
    Lwt_mutex.with_lock create (fun () -> unsafe_v ?fresh root)

  let unsafe_set t k v =
    try
      let off = Tbl.find t.index k in
      Tbl.replace t.cache k v;
      set_entry t ~off k v
    with Not_found ->
      let offset = IO.offset t.block in
      set_entry t k v;
      Tbl.add t.cache k v;
      Tbl.add t.index k offset

  let set t k v =
    Log.debug (fun l -> l "[branches] set %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () ->
        unsafe_set t k v;
        Lwt.return () )
    >>= fun () -> W.notify t.w k (Some v)

  let unsafe_test_and_set t k ~test ~set =
    let v = try Some (Tbl.find t.cache k) with Not_found -> None in
    if not (Irmin.Type.(equal (option V.t)) v test) then Lwt.return false
    else
      let return () = Lwt.return true in
      match set with
      | None -> unsafe_remove t k |> return
      | Some v -> unsafe_set t k v |> return

  let test_and_set t k ~test ~set =
    Log.debug (fun l -> l "[branches] test-and-set %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () -> unsafe_test_and_set t k ~test ~set)
    >>= function
    | true -> W.notify t.w k set >|= fun () -> true
    | false -> Lwt.return false

  let list t =
    Log.debug (fun l -> l "[branches] list");
    let keys = Tbl.fold (fun k _ acc -> k :: acc) t.cache [] in
    Lwt.return keys

  let watch_key t = W.watch_key t.w

  let watch t = W.watch t.w

  let unwatch t = W.unwatch t.w
end

module Make_ext
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S
            with type metadata = M.t
             and type hash = H.t
             and type step = P.step)
    (Commit : Irmin.Private.Commit.S with type hash = H.t) =
struct
  module Pack = Pack (H)

  module X = struct
    module Hash = H

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        include Pack.Make (struct
          include Val

          let hash t = H.hash (Irmin.Type.pre_hash Val.t t)

          let with_hash_t = Irmin.Type.(pair H.t Val.t)

          let to_bin ~dict:_ ~offset:_ t k =
            let s = Irmin.Type.to_bin_string with_hash_t (k, t) in
            Lwt.return s

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, (_, t) =
              Irmin.Type.decode_bin ~headers:false with_hash_t s off
            in
            Lwt.return t
        end)
      end

      include Irmin.Contents.Store (CA)
    end

    module Inode = struct
      module Conf = struct
        let max_values = 32

        let max_inodes = 32
      end

      module Val = struct
        type entry =
          | Node of { name : P.step; node : H.t }
          | Contents of { metadata : M.t; name : P.step; node : H.t }
          | Inode of { index : int; node : H.t }

        let entry : entry Irmin.Type.t =
          let open Irmin.Type in
          variant "Inode.entry" (fun node contents inode -> function
            | Node n -> node (n.name, n.node)
            | Contents c -> contents (c.metadata, c.name, c.node)
            | Inode i -> inode (i.index, i.node) )
          |~ case1 "Node" (pair P.step_t H.t) (fun (name, node) ->
                 Node { name; node } )
          |~ case1 "Contents" (triple M.t P.step_t H.t)
               (fun (metadata, name, node) -> Contents { metadata; name; node })
          |~ case1 "Inode" (pair int H.t) (fun (index, node) ->
                 Inode { index; node } )
          |> sealv

        (* hash is the hash of the corresponding sub-node *)
        type t = { hash : H.t; entries : entry list }

        let hash x = H.hash (Irmin.Type.pre_hash Node.t x)

        let empty = { hash = hash Node.empty; entries = [] }

        let t =
          let open Irmin.Type in
          record "Inode.t" (fun hash entries -> { hash; entries })
          |+ field "hash" H.t (fun t -> t.hash)
          |+ field "entries" (list entry) (fun t -> t.entries)
          |> sealr

        let entry_of_value name v =
          match v with
          | `Node node -> Node { name; node }
          | `Contents (node, metadata) -> Contents { metadata; name; node }

        let entries_of_values l =
          let entries =
            List.fold_left
              (fun acc (s, v) -> entry_of_value s v :: acc)
              [] (List.rev l)
          in
          let hash = hash (Node.v l) in
          { hash; entries }

        let entry_of_inode index node = Inode { index; node }
      end

      module Tree = struct
        module StepMap = struct
          include Map.Make (struct
            type t = P.step

            let compare = Irmin.Type.compare P.step_t
          end)

          let of_list l =
            List.fold_left (fun acc (k, v) -> add k v acc) empty l
        end

        type t = Values of Node.value StepMap.t | Nodes of t array | Empty

        let empty : t = Empty

        let list (t : t) =
          let rec aux acc = function
            | Empty -> acc
            | Values t ->
                List.fold_left (fun acc e -> e :: acc) acc (StepMap.bindings t)
            | Nodes n -> Array.fold_left aux acc n
          in
          aux [] t

        let hash_node t = H.hash (Irmin.Type.pre_hash Node.t t)

        let hash_values vs = hash_node (Node.v vs)

        let hash_entries es = H.hash Irmin.Type.(pre_hash (list Val.entry) es)

        let index ~seed k =
          abs (Irmin.Type.short_hash P.step_t ~seed k) mod Conf.max_inodes

        let singleton s v = Values (StepMap.singleton s v)

        let rec add_values : type a.
            seed:_ -> _ -> _ -> _ -> _ -> (t -> a) -> a =
         fun ~seed t vs s v k ->
          let values = StepMap.add s v vs in
          if values == vs then k t
          else if StepMap.cardinal values <= Conf.max_values then
            k (Values values)
          else
            (nodes_of_values [@tailcall]) ~seed values @@ fun n -> k (Nodes n)

        and nodes_of_values : type a. seed:_ -> _ -> (t array -> a) -> a =
         fun ~seed entries k ->
          let n = Array.make Conf.max_inodes empty in
          StepMap.iter
            (fun s e -> (update_nodes [@tailcall]) ~seed n s e)
            entries;
          k n

        and with_node : type a.
            seed:_ -> copy:_ -> _ -> _ -> _ -> (int -> t -> t -> a) -> a =
         fun ~seed ~copy n s v k ->
          let i = index ~seed s in
          let x = n.(i) in
          match x with
          | Empty -> k i x (singleton s v)
          | Values vs ->
              (add_values [@tailcall]) ~seed:(seed + 1) x vs s v @@ fun y ->
              k i x y
          | Nodes n ->
              (add_nodes [@tailcall]) ~seed:(seed + 1) ~copy x n s v
              @@ fun y -> k i x y

        and update_nodes ~seed n s e =
          with_node ~seed ~copy:false n s e @@ fun i x y ->
          if x != y then n.(i) <- y

        and add_nodes : type a.
            seed:_ -> copy:_ -> t -> _ -> _ -> _ -> (t -> a) -> a =
         fun ~seed ~copy t n s e k ->
          with_node ~seed ~copy n s e @@ fun i x y ->
          if x == y then k t
          else
            let n = if copy then Array.copy n else n in
            n.(i) <- y;
            k (Nodes n)

        let values l = Values (StepMap.of_list l)

        let v l : t =
          if List.length l < Conf.max_values then values l
          else
            let aux t (s, v) =
              match t with
              | Empty -> singleton s v
              | Values vs ->
                  (add_values [@tailcall]) ~seed:0 t vs s v (fun x -> x)
              | Nodes n ->
                  (add_nodes [@tailcall]) ~seed:0 ~copy:false t n s v (fun x ->
                      x )
            in
            List.fold_left aux empty l

        let fold f t init =
          let rec inode t k =
            match t with
            | Empty -> k Val.empty
            | Values vs ->
                let values = StepMap.bindings vs in
                assert (List.length values <= Conf.max_values);
                k (Val.entries_of_values values)
            | Nodes n ->
                (inodes [@tailcall]) n 0 @@ fun entries ->
                let hash = hash_entries entries in
                k { Val.hash; entries }
          and inodes n i k =
            if i >= Array.length n then k []
            else
              match n.(i) with
              | Empty -> inodes n (i + 1) k
              | Values es when StepMap.cardinal es = 1 ->
                  let s, v = StepMap.choose es in
                  (inodes [@tailcall]) n (i + 1) @@ fun entries ->
                  let entries = Val.entry_of_value s v :: entries in
                  k entries
              | _ ->
                  (inode [@tailcall]) n.(i) @@ fun t ->
                  f t.hash t @@ fun () ->
                  (inodes [@tailcall]) n (i + 1) @@ fun entries ->
                  let entries = Val.entry_of_inode i t.hash :: entries in
                  k entries
          in
          inode t (fun v ->
              let values = list t in
              let hash = hash_values values in
              let v = { v with hash } in
              init hash v )

        let save ~add t =
          fold
            (fun k v f -> add k v >>= f)
            t
            (fun k v -> add k v >|= fun () -> k)

        let load ~find h =
          let rec inode ~seed h k =
            find h >>= function
            | None -> k None
            | Some { Val.entries = []; _ } -> k (Some empty)
            | Some { entries = i; _ } ->
                let vs, is =
                  List.fold_left
                    (fun (vs, is) -> function
                      | Val.Node n -> (StepMap.add n.name (`Node n.node) vs, is)
                      | Contents c ->
                          ( StepMap.add c.name
                              (`Contents (c.node, c.metadata))
                              vs,
                            is ) | Inode i -> (vs, (i.index, i.node) :: is) )
                    (StepMap.empty, []) i
                in
                if is = [] then
                  let t = Values vs in
                  k (Some t)
                else
                  (nodes_of_values [@tailcall]) ~seed vs @@ fun n ->
                  (inodes [@tailcall]) ~seed n is @@ fun n ->
                  k (Some (Nodes n))
          and inodes ~seed n l k =
            match l with
            | [] -> k n
            | (i, h) :: t -> (
                (inode [@tailcall]) ~seed:(seed + 1) h @@ fun v ->
                (inodes [@tailcall]) ~seed n t @@ fun n ->
                match v with
                | None -> k n
                | Some v ->
                    assert (i < Conf.max_inodes);
                    assert (n.(i) = Empty);
                    n.(i) <- v;
                    k n )
          in
          inode ~seed:0 h (function
            | None -> Lwt.return None
            | Some x -> Lwt.return (Some x) )
      end

      module Compress = struct
        type name = Indirect of int | Direct of P.step

        type address = Indirect of int64 | Direct of H.t

        type entry =
          | Contents of name * address * M.t
          | Node of name * address
          | Inode of int * address

        let entry : entry Irmin.Type.t =
          let open Irmin.Type in
          variant "Compress.entry"
            (fun contents_ii
            node_ii
            inode_i
            contents_id
            node_id
            inode_d
            contents_di
            node_di
            contents_dd
            node_dd
            -> function
            | Contents (Indirect n, Indirect h, m) -> contents_ii (n, h, m)
            | Node (Indirect n, Indirect h) -> node_ii (n, h)
            | Inode (n, Indirect h) -> inode_i (n, h)
            | Contents (Indirect n, Direct h, m) -> contents_id (n, h, m)
            | Node (Indirect n, Direct h) -> node_id (n, h)
            | Inode (n, Direct h) -> inode_d (n, h)
            | Contents (Direct n, Indirect h, m) -> contents_di (n, h, m)
            | Node (Direct n, Indirect h) -> node_di (n, h)
            | Contents (Direct n, Direct h, m) -> contents_dd (n, h, m)
            | Node (Direct n, Direct h) -> node_dd (n, h) )
          |~ case1 "contents-ii" (triple int int64 M.t) (fun (n, i, m) ->
                 Contents (Indirect n, Indirect i, m) )
          |~ case1 "node-ii" (pair int int64) (fun (n, i) ->
                 Node (Indirect n, Indirect i) )
          |~ case1 "inode-i" (pair int int64) (fun (n, i) ->
                 Inode (n, Indirect i) )
          |~ case1 "contents-id" (triple int H.t M.t) (fun (n, h, m) ->
                 Contents (Indirect n, Direct h, m) )
          |~ case1 "node-id" (pair int H.t) (fun (n, h) ->
                 Node (Indirect n, Direct h) )
          |~ case1 "inode-d" (pair int H.t) (fun (n, h) -> Inode (n, Direct h))
          |~ case1 "contents-di" (triple P.step_t int64 M.t) (fun (n, i, m) ->
                 Contents (Direct n, Indirect i, m) )
          |~ case1 "node-di" (pair P.step_t int64) (fun (n, i) ->
                 Node (Direct n, Indirect i) )
          |~ case1 "contents-dd" (triple P.step_t H.t M.t) (fun (n, i, m) ->
                 Contents (Direct n, Direct i, m) )
          |~ case1 "node-dd" (pair P.step_t H.t) (fun (n, i) ->
                 Node (Direct n, Direct i) )
          |> sealv

        let t = Irmin.Type.(pair H.t (list entry))
      end

      include Pack.Make (struct
        include Val

        let hash t = t.hash

        let to_bin ~dict ~offset (t : t) k =
          assert (Irmin.Type.equal H.t k t.hash);
          let step s : Compress.name Lwt.t =
            let str = Irmin.Type.to_string P.step_t s in
            if String.length str <= 4 then Lwt.return (Direct s : Compress.name)
            else dict str >|= fun s -> (Indirect s : Compress.name)
          in
          let hash h =
            offset h >|= function
            | None -> Compress.Direct h
            | Some off -> Compress.Indirect off
          in
          let inode : entry -> Compress.entry Lwt.t = function
            | Contents c ->
                step c.name >>= fun s ->
                hash c.node >|= fun v -> Compress.Contents (s, v, c.metadata)
            | Node n ->
                step n.name >>= fun s ->
                hash n.node >|= fun v -> Compress.Node (s, v)
            | Inode i -> hash i.node >|= fun v -> Compress.Inode (i.index, v)
          in
          Lwt_list.map_p inode t.entries >|= fun inodes ->
          let res = Irmin.Type.to_bin_string Compress.t (k, inodes) in
          res

        exception Exit of [ `Msg of string ]

        let decode_bin ~dict ~hash t off : t Lwt.t =
          let _, (h, inodes) =
            Irmin.Type.decode_bin ~headers:false Compress.t t off
          in
          let step : Compress.name -> P.step Lwt.t = function
            | Direct n -> Lwt.return n
            | Indirect s -> (
                dict s >|= function
                | None -> raise_notrace (Exit (`Msg "dict"))
                | Some s -> (
                  match Irmin.Type.of_bin_string P.step_t s with
                  | Error e -> raise_notrace (Exit e)
                  | Ok v -> v ) )
          in
          let hash : Compress.address -> H.t Lwt.t = function
            | Indirect off -> hash off
            | Direct n -> Lwt.return n
          in
          let inode : Compress.entry -> entry Lwt.t = function
            | Contents (n, h, metadata) ->
                step n >>= fun name ->
                hash h >|= fun node -> Contents { name; node; metadata }
            | Node (n, h) ->
                step n >>= fun name ->
                hash h >|= fun node -> Node { name; node }
            | Inode (index, h) -> hash h >|= fun node -> Inode { index; node }
          in
          Lwt.catch
            (fun () ->
              Lwt_list.map_p inode inodes >|= fun entries ->
              { hash = h; entries } )
            (function Exit (`Msg e) -> Lwt.fail_with e | e -> Lwt.fail e)
      end)
    end

    module Node = struct
      module CA = struct
        module Val = Node
        module Key = H

        type 'a t = 'a Inode.t

        type key = Key.t

        type value = Val.t

        let mem t k = Inode.mem t k

        let find t k =
          Inode.Tree.load ~find:(Inode.find t) k >|= function
          | None -> None
          | Some t -> Some (Val.v (Inode.Tree.list t))

        let add t v =
          let n = Val.list v in
          let v = Inode.Tree.v n in
          Inode.Tree.save ~add:(Inode.append t) v

        let batch = Inode.batch

        let v = Inode.v
      end

      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        include Pack.Make (struct
          include Val

          let hash t = H.hash (Irmin.Type.pre_hash Val.t t)

          let with_hash_t = Irmin.Type.(pair H.t Val.t)

          let to_bin ~dict:_ ~offset:_ t k =
            Lwt.return (Irmin.Type.to_bin_string with_hash_t (k, t))

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, (_, v) =
              Irmin.Type.decode_bin ~headers:false with_hash_t s off
            in
            Lwt.return v
        end)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      include Atomic_write (Key) (Val)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.CA.t;
        node : [ `Read ] Node.CA.t;
        commit : [ `Read ] Commit.CA.t;
        branch : Branch.t
      }

      let contents_t t : 'a Contents.t = t.contents

      let node_t t : 'a Node.t = (contents_t t, t.node)

      let commit_t t : 'a Commit.t = (node_t t, t.commit)

      let branch_t t = t.branch

      let batch t f =
        Commit.CA.batch t.commit (fun commit ->
            Node.CA.batch t.node (fun node ->
                Contents.CA.batch t.contents (fun contents ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit ) ) )

      let v config =
        let root = root config in
        let fresh = fresh config in
        Contents.CA.v ~fresh root >>= fun contents ->
        Node.CA.v ~fresh root >>= fun node ->
        Commit.CA.v ~fresh root >>= fun commit ->
        Branch.v ~fresh root >|= fun branch ->
        { contents; node; commit; branch; config }
    end
  end

  include Irmin.Of_private (X)
end

module Hash = Irmin.Hash.SHA1
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None

module Make
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module XNode = Irmin.Private.Node.Make (H) (P) (M)
  module XCommit = Irmin.Private.Commit.Make (H)
  include Make_ext (M) (C) (P) (B) (H) (XNode) (XCommit)
end

module KV (C : Irmin.Contents.S) =
  Make (Metadata) (C) (Path) (Irmin.Branch.String) (Hash)
