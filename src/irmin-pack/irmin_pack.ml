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

let current_version = "00000001"

module Log = (val Logs.src_log src : Logs.LOG)

let fresh_key =
  Irmin.Private.Conf.key ~doc:"Start with a fresh disk." "fresh"
    Irmin.Private.Conf.bool false

let lru_size_key =
  Irmin.Private.Conf.key ~doc:"Size of the LRU cache for pack entries."
    "lru-size" Irmin.Private.Conf.int 10_000

let readonly_key =
  Irmin.Private.Conf.key ~doc:"Start with a read-only disk." "readonly"
    Irmin.Private.Conf.bool false

let fresh config = Irmin.Private.Conf.get config fresh_key

let lru_size config = Irmin.Private.Conf.get config lru_size_key

let readonly config = Irmin.Private.Conf.get config readonly_key

let root_key = Irmin.Private.Conf.root

let root config =
  match Irmin.Private.Conf.get config root_key with
  | None -> failwith "no root set"
  | Some r -> r

let config ?(fresh = false) ?(readonly = false) ?(lru_size = 10_000) root =
  let config = Irmin.Private.Conf.empty in
  let config = Irmin.Private.Conf.add config fresh_key fresh in
  let config = Irmin.Private.Conf.add config root_key (Some root) in
  let config = Irmin.Private.Conf.add config lru_size_key lru_size in
  let config = Irmin.Private.Conf.add config readonly_key readonly in
  config

let ( // ) = Filename.concat

let ( ++ ) = Int64.add

let ( -- ) = Int64.sub

exception RO_Not_Allowed

type all_stats = {
  mutable index_appends : int;
  mutable index_mems : int;
  mutable index_finds : int;
  mutable index_bloomf_misses : int;
  mutable index_bloomf_mems : int;
  mutable index_is : int;
  mutable index_is_steps : int;
  mutable pack_finds : int;
  mutable pack_cache_misses : int;
  mutable pack_page_read : int;
  mutable pack_page_miss : int;
  mutable index_page_read : int;
  mutable index_page_miss : int;
  mutable appended_hashes : int;
  mutable appended_offsets : int
}

let fresh_stats () =
  { index_appends = 0;
    index_mems = 0;
    index_finds = 0;
    index_bloomf_misses = 0;
    index_bloomf_mems = 0;
    index_is = 0;
    index_is_steps = 0;
    pack_finds = 0;
    pack_cache_misses = 0;
    pack_page_read = 0;
    pack_page_miss = 0;
    index_page_read = 0;
    index_page_miss = 0;
    appended_hashes = 0;
    appended_offsets = 0
  }

let stats = fresh_stats ()

let reset_stats () =
  stats.index_appends <- 0;
  stats.index_finds <- 0;
  stats.index_mems <- 0;
  stats.index_mems <- 0;
  stats.index_bloomf_misses <- 0;
  stats.index_bloomf_mems <- 0;
  stats.index_is <- 0;
  stats.index_is_steps <- 0;
  stats.pack_finds <- 0;
  stats.pack_cache_misses <- 0;
  stats.pack_page_read <- 0;
  stats.pack_page_miss <- 0;
  stats.index_page_read <- 0;
  stats.index_page_miss <- 0;
  stats.appended_hashes <- 0;
  stats.appended_offsets <- 0;
  ()

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

module Dict = struct
  type t = {
    cache : (string, int) Hashtbl.t;
    index : (int, string) Hashtbl.t;
    block : IO.t
  }

  let append_string t v =
    let len = Int32.of_int (String.length v) in
    let buf = Irmin.Type.(to_bin_string int32 len) ^ v in
    IO.append t.block buf

  let index t v =
    Log.debug (fun l -> l "[dict] index %S" v);
    try Hashtbl.find t.cache v
    with Not_found ->
      let id = Hashtbl.length t.cache in
      append_string t v;
      Hashtbl.add t.cache v id;
      Hashtbl.add t.index id v;
      id

  let find t id =
    Log.debug (fun l -> l "[dict] find %d" id);
    let v = try Some (Hashtbl.find t.index id) with Not_found -> None in
    v

  let clear t =
    IO.clear t.block;
    Hashtbl.clear t.cache;
    Hashtbl.clear t.index

  let files = Hashtbl.create 10

  let v ?(fresh = false) ?(readonly = false) root =
    let root = root // "store.dict" in
    Log.debug (fun l -> l "[dict] v fresh=%b RO=%b root=%s" fresh readonly root);
    try
      let t = Hashtbl.find files root in
      if fresh then clear t;
      t
    with Not_found ->
      let block = IO.v ~version:current_version ~readonly root in
      if fresh then IO.clear block;
      let cache = Hashtbl.create 997 in
      let index = Hashtbl.create 997 in
      let len = Int64.to_int (IO.offset block) in
      let raw = Bytes.create len in
      let n = IO.read block ~off:0L raw in
      assert (n = len);
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
      let t = { index; cache; block } in
      Hashtbl.add files root t;
      t
end

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

module Pack (K : Irmin.Hash.S) = struct
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
      IO.sync t.pack.dict.block;
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

  let page = Bytes.create 4

  let read_length32 ~off block =
    let n = IO.read block ~off page in
    assert (n = 4);
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

  let unsafe_v ?(fresh = false) ?(readonly = false) root =
    let root = root // "store.branches" in
    Log.debug (fun l ->
        l "[branches] v fresh=%b RO=%b root=%s" fresh readonly root );
    try
      let t = Hashtbl.find files root in
      (if fresh then clear t else Lwt.return ()) >|= fun () -> t
    with Not_found ->
      let block = IO.v ~version:current_version ~readonly root in
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
          let n = IO.read block ~off buf in
          assert (n = Bytes.length buf);
          let buf = Bytes.unsafe_to_string buf in
          let h =
            let h = String.sub buf 0 len in
            match Irmin.Type.of_bin_string K.t h with
            | Ok k -> k
            | Error (`Msg e) -> failwith e
          in
          let n, v = Irmin.Type.decode_bin V.t buf len in
          assert (n = String.length buf);
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

  let v ?fresh ?readonly root =
    Lwt_mutex.with_lock create (fun () -> unsafe_v ?fresh ?readonly root)

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

    type 'a value = { magic : char; hash : H.t; v : 'a }

    let value a =
      let open Irmin.Type in
      record "value" (fun hash magic v -> { magic; hash; v })
      |+ field "hash" H.t (fun v -> v.hash)
      |+ field "magic" char (fun v -> v.magic)
      |+ field "v" a (fun v -> v.v)
      |> sealr

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        include Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let magic = 'B'

          let value = value Val.t

          let encode_bin ~dict:_ ~offset:_ v hash =
            Irmin.Type.encode_bin value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, t = Irmin.Type.decode_bin ~headers:false value s off in
            t.v
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
        type t = entry list value

        module H = Irmin.Hash.Typed (H) (Node)

        let hash = H.hash

        let magic = 'T'

        let empty = { magic; hash = hash Node.empty; v = [] }

        let v ~hash v = { magic; hash; v }

        let t = value (Irmin.Type.list entry)

        let entry_of_value name v =
          match v with
          | `Node node -> Node { name; node }
          | `Contents (node, metadata) -> Contents { metadata; name; node }

        let entries_of_values l =
          let v =
            List.fold_left
              (fun acc (s, v) -> entry_of_value s v :: acc)
              [] (List.rev l)
          in
          let hash = hash (Node.v l) in
          { magic; hash; v }

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

        module H_entries =
          Irmin.Hash.Typed
            (H)
            (struct
              type t = Val.entry list

              let t = Irmin.Type.list Val.entry
            end)

        let hash_entries = H_entries.hash

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

        let fold f t ~hash init =
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
                k (Val.v ~hash entries)
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
            | Some { v = []; _ } -> k (Some empty)
            | Some { v = i; _ } ->
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

        let t = value (Irmin.Type.list entry)
      end

      include Pack.Make (struct
        include Val

        let hash t = t.hash

        let encode_bin ~dict ~offset (t : t) k =
          assert (Irmin.Type.equal H.t k t.hash);
          let step s : Compress.name =
            let str = Irmin.Type.to_bin_string P.step_t s in
            if String.length str <= 4 then Direct s
            else
              let s = dict str in
              Indirect s
          in
          let hash h : Compress.address =
            match offset h with
            | None -> Compress.Direct h
            | Some off -> Compress.Indirect off
          in
          let inode : entry -> Compress.entry = function
            | Contents c ->
                let s = step c.name in
                let v = hash c.node in
                Compress.Contents (s, v, c.metadata)
            | Node n ->
                let s = step n.name in
                let v = hash n.node in
                Compress.Node (s, v)
            | Inode i ->
                let v = hash i.node in
                Compress.Inode (i.index, v)
          in
          (* List.map is fine here as the number of entries is small *)
          let inodes = List.map inode t.v in
          Irmin.Type.encode_bin Compress.t (Val.v ~hash:k inodes)

        exception Exit of [ `Msg of string ]

        let decode_bin ~dict ~hash t off : t =
          let _, i = Irmin.Type.decode_bin ~headers:false Compress.t t off in
          let step : Compress.name -> P.step = function
            | Direct n -> n
            | Indirect s -> (
              match dict s with
              | None -> raise_notrace (Exit (`Msg "dict"))
              | Some s -> (
                match Irmin.Type.of_bin_string P.step_t s with
                | Error e -> raise_notrace (Exit e)
                | Ok v -> v ) )
          in
          let hash : Compress.address -> H.t = function
            | Indirect off -> hash off
            | Direct n -> n
          in
          let inode : Compress.entry -> entry = function
            | Contents (n, h, metadata) ->
                let name = step n in
                let node = hash h in
                Contents { name; node; metadata }
            | Node (n, h) ->
                let name = step n in
                let node = hash h in
                Node { name; node }
            | Inode (index, h) ->
                let node = hash h in
                Inode { index; node }
          in
          try
            (* List.map is fine here as the number of inodes is small. *)
            let entries = List.map inode i.v in
            Val.v ~hash:i.hash entries
          with Exit (`Msg e) -> failwith e
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
          Inode.Tree.load
            ~find:(fun k ->
              let v = Inode.unsafe_find t k in
              Lwt.return v )
            k
          >|= function
          | None -> None
          | Some t -> Some (Val.v (Inode.Tree.list t))

        module H_node = Irmin.Hash.Typed (H) (Val)

        let hash_node = H_node.hash

        let add t v =
          let n = Val.list v in
          let hash = hash_node v in
          let v = Inode.Tree.v n in
          Inode.Tree.save ~hash
            ~add:(fun k v ->
              Inode.unsafe_append t k v;
              Lwt.return () )
            v

        let unsafe_add t k v =
          let n = Val.list v in
          let v = Inode.Tree.v n in
          Inode.Tree.save ~hash:k
            ~add:(fun k v ->
              Inode.unsafe_append t k v;
              Lwt.return () )
            v
          >|= fun _ -> ()

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
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let value = value Val.t

          let magic = 'C'

          let encode_bin ~dict:_ ~offset:_ v hash =
            Irmin.Type.encode_bin value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, v = Irmin.Type.decode_bin ~headers:false value s off in
            v.v
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
        let lru_size = lru_size config in
        let readonly = readonly config in
        Contents.CA.v ~fresh ~readonly ~lru_size root >>= fun contents ->
        Node.CA.v ~fresh ~readonly ~lru_size root >>= fun node ->
        Commit.CA.v ~fresh ~readonly ~lru_size root >>= fun commit ->
        Branch.v ~fresh ~readonly root >|= fun branch ->
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

let div_or_zero a b = if b = 0 then 0. else float_of_int a /. float_of_int b

type stats = {
  bf_misses : float;
  pack_page_faults : float;
  index_page_faults : float;
  pack_cache_misses : float;
  search_steps : float;
  offset_ratio : float;
  offset_significance : int
}

let stats () =
  { bf_misses = div_or_zero stats.index_bloomf_misses stats.index_bloomf_mems;
    pack_page_faults = div_or_zero stats.pack_page_miss stats.pack_page_read;
    index_page_faults = div_or_zero stats.index_page_miss stats.index_page_read;
    pack_cache_misses = div_or_zero stats.pack_cache_misses stats.pack_finds;
    search_steps = div_or_zero stats.index_is_steps stats.index_is;
    offset_ratio =
      div_or_zero stats.appended_offsets
        (stats.appended_offsets + stats.appended_hashes);
    offset_significance = stats.appended_offsets + stats.appended_hashes
  }
