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

module Default = struct
  let fresh = false

  let lru_size = 100_000

  let index_log_size = 500_000

  let readonly = false
end

let fresh_key =
  Irmin.Private.Conf.key ~doc:"Start with a fresh disk." "fresh"
    Irmin.Private.Conf.bool Default.fresh

let lru_size_key =
  Irmin.Private.Conf.key ~doc:"Size of the LRU cache for pack entries."
    "lru-size" Irmin.Private.Conf.int Default.lru_size

let index_log_size_key =
  Irmin.Private.Conf.key ~doc:"Size of index logs." "index-log-size"
    Irmin.Private.Conf.int Default.index_log_size

let readonly_key =
  Irmin.Private.Conf.key ~doc:"Start with a read-only disk." "readonly"
    Irmin.Private.Conf.bool Default.readonly

let fresh config = Irmin.Private.Conf.get config fresh_key

let lru_size config = Irmin.Private.Conf.get config lru_size_key

let readonly config = Irmin.Private.Conf.get config readonly_key

let index_log_size config = Irmin.Private.Conf.get config index_log_size_key

let root_key = Irmin.Private.Conf.root

let root config =
  match Irmin.Private.Conf.get config root_key with
  | None -> failwith "no root set"
  | Some r -> r

let config ?(fresh = Default.fresh) ?(readonly = Default.readonly)
    ?(lru_size = Default.lru_size) ?(index_log_size = Default.index_log_size)
    root =
  let config = Irmin.Private.Conf.empty in
  let config = Irmin.Private.Conf.add config fresh_key fresh in
  let config = Irmin.Private.Conf.add config root_key (Some root) in
  let config = Irmin.Private.Conf.add config lru_size_key lru_size in
  let config =
    Irmin.Private.Conf.add config index_log_size_key index_log_size
  in
  let config = Irmin.Private.Conf.add config readonly_key readonly in
  config

let ( ++ ) = Int64.add

let with_cache = IO.with_cache

open Lwt.Infix
module Pack = Pack
module Dict = Pack_dict
module Index = Pack_index
module IO = IO.Unix

exception RO_Not_Allowed = IO.RO_Not_Allowed

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t

  let hash (t : t) = Irmin.Type.short_hash K.t t

  let equal (x : t) (y : t) = Irmin.Type.equal K.t x y
end)

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
    w : W.t;
    mutable open_instances : int;
  }

  let decode_bin = Irmin.Type.(unstage (decode_bin int32))

  let read_length32 ~off block =
    let buf = Bytes.create 4 in
    let n = IO.read block ~off buf in
    assert (n = 4);
    let n, v = decode_bin (Bytes.unsafe_to_string buf) 0 in
    assert (n = 4);
    Int32.to_int v

  let entry = Irmin.Type.(pair (string_of `Int32) V.t)

  let key_to_bin_string = Irmin.Type.(unstage (to_bin_string K.t))

  let key_of_bin_string = Irmin.Type.(unstage (of_bin_string K.t))

  let entry_to_bin_string = Irmin.Type.(unstage (to_bin_string entry))

  let value_of_bin_string = Irmin.Type.(unstage (of_bin_string V.t))

  let value_decode_bin = Irmin.Type.(unstage (decode_bin V.t))

  let set_entry t ?off k v =
    let k = key_to_bin_string k in
    let buf = entry_to_bin_string (k, v) in
    match off with
    | None -> IO.append t.block buf
    | Some off -> IO.set t.block buf ~off

  let pp_branch = Irmin.Type.pp K.t

  let zero =
    match value_of_bin_string (String.make V.hash_size '\000') with
    | Ok x -> x
    | Error _ -> assert false

  let refill t ~from =
    let len = IO.force_offset t.block in
    let rec aux offset =
      if offset >= len then ()
      else
        let len = read_length32 ~off:offset t.block in
        let buf = Bytes.create (len + V.hash_size) in
        let off = offset ++ 4L in
        let n = IO.read t.block ~off buf in
        assert (n = Bytes.length buf);
        let buf = Bytes.unsafe_to_string buf in
        let h =
          let h = String.sub buf 0 len in
          match key_of_bin_string h with
          | Ok k -> k
          | Error (`Msg e) -> failwith e
        in
        let n, v = value_decode_bin buf len in
        assert (n = String.length buf);
        if not (Irmin.Type.equal V.t v zero) then Tbl.add t.cache h v;
        Tbl.add t.index h offset;
        (aux [@tailcall]) (off ++ Int64.(of_int @@ (len + V.hash_size)))
    in
    aux from

  let sync_offset t =
    let former_log_offset = IO.offset t.block in
    let log_offset = IO.force_offset t.block in
    if log_offset > former_log_offset then refill t ~from:former_log_offset

  let unsafe_find t k =
    Log.debug (fun l -> l "[branches] find %a" pp_branch k);
    if IO.readonly t.block then sync_offset t;
    try Lwt.return_some (Tbl.find t.cache k) with Not_found -> Lwt.return_none

  let find t k = Lwt_mutex.with_lock t.lock (fun () -> unsafe_find t k)

  let unsafe_mem t k =
    Log.debug (fun l -> l "[branches] mem %a" pp_branch k);
    try Lwt.return (Tbl.mem t.cache k) with Not_found -> Lwt.return_false

  let mem t v = Lwt_mutex.with_lock t.lock (fun () -> unsafe_mem t v)

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
        Lwt.return_unit)
    >>= fun () -> W.notify t.w k None

  let unsafe_clear t =
    Lwt.async (fun () -> W.clear t.w);
    IO.clear t.block;
    Tbl.clear t.cache;
    Tbl.clear t.index

  let create = Lwt_mutex.create ()

  let watches = W.v ()

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false

  let unsafe_v ~fresh ~readonly file =
    let block = IO.v ~fresh ~version:current_version ~readonly file in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let t =
      {
        cache;
        index;
        block;
        w = watches;
        lock = Lwt_mutex.create ();
        open_instances = 1;
      }
    in
    refill t ~from:0L;
    t

  let (`Staged unsafe_v) =
    with_cache ~clear:unsafe_clear ~valid
      ~v:(fun () -> unsafe_v)
      "store.branches"

  let v ?fresh ?readonly file =
    Lwt_mutex.with_lock create (fun () ->
        let v = unsafe_v () ?fresh ?readonly file in
        Lwt.return v)

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
        Lwt.return_unit)
    >>= fun () -> W.notify t.w k (Some v)

  let unsafe_test_and_set t k ~test ~set =
    let v = try Some (Tbl.find t.cache k) with Not_found -> None in
    if not (Irmin.Type.(equal (option V.t)) v test) then Lwt.return_false
    else
      let return () = Lwt.return_true in
      match set with
      | None -> unsafe_remove t k |> return
      | Some v -> unsafe_set t k v |> return

  let test_and_set t k ~test ~set =
    Log.debug (fun l -> l "[branches] test-and-set %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () -> unsafe_test_and_set t k ~test ~set)
    >>= function
    | true -> W.notify t.w k set >|= fun () -> true
    | false -> Lwt.return_false

  let list t =
    Log.debug (fun l -> l "[branches] list");
    let keys = Tbl.fold (fun k _ acc -> k :: acc) t.cache [] in
    Lwt.return keys

  let watch_key t = W.watch_key t.w

  let watch t = W.watch t.w

  let unwatch t = W.unwatch t.w

  let unsafe_close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      Tbl.reset t.index;
      Tbl.reset t.cache;
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block;
      W.clear t.w)
    else Lwt.return_unit

  let close t = Lwt_mutex.with_lock t.lock (fun () -> unsafe_close t)
end

module type CONFIG = Inode.CONFIG

module type Stores_extra = sig
  type repo

  val integrity_check :
    ?ppf:Format.formatter ->
    auto_repair:bool ->
    repo ->
    ( [> `Fixed of int | `No_error ],
      [> `Cannot_fix of string | `Corrupted of int ] )
    result

  val sync : repo -> unit
end

module Make_ext
    (Config : CONFIG)
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
  module Index = Pack_index.Make (H)
  module Pack = Pack.File (Index) (H)

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

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let magic = 'B'

          let value = value Val.t

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, t = decode_value s off in
            t.v

          let magic _ = magic
        end)

        include Closeable.Pack (CA_Pack)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module CA = Inode.Make (Config) (H) (Pack) (Node)
      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let value = value Val.t

          let magic = 'C'

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, v = decode_value s off in
            v.v

          let magic _ = magic
        end)

        include Closeable.Pack (CA_Pack)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      module AW = Atomic_write (Key) (Val)
      include Closeable.Atomic_write (AW)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.CA.t;
        node : [ `Read ] Node.CA.t;
        commit : [ `Read ] Commit.CA.t;
        branch : Branch.t;
        index : Index.t;
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
                    f contents node commit)))

      let v config =
        let root = root config in
        let fresh = fresh config in
        let lru_size = lru_size config in
        let readonly = readonly config in
        let log_size = index_log_size config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~auto_flush_callback:(fun () -> !f ())
              (* backpatching to add pack flush before an index flush *)
            ~fresh ~readonly ~log_size root
        in
        Contents.CA.v ~fresh ~readonly ~lru_size ~index root >>= fun contents ->
        Node.CA.v ~fresh ~readonly ~lru_size ~index root >>= fun node ->
        Commit.CA.v ~fresh ~readonly ~lru_size ~index root >>= fun commit ->
        Branch.v ~fresh ~readonly root >|= fun branch ->
        (* Stores share instances in memory, one flush is enough. In case of a
           system crash, the auto_flush_callback might not make with the disk.
           In this case, when the store is reopened, [integrity_check] needs to
           be called to repair the store. *)
        (f := fun () -> Contents.CA.flush ~index:false contents);
        { contents; node; commit; branch; config; index }

      let close t =
        Index.close t.index;
        Contents.CA.close (contents_t t) >>= fun () ->
        Node.CA.close (snd (node_t t)) >>= fun () ->
        Commit.CA.close (snd (commit_t t)) >>= fun () -> Branch.close t.branch

      (** stores share instances in memory, one sync is enough *)
      let sync t = Contents.CA.sync (contents_t t)
    end
  end

  let null =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> "/dev/null"
    | "Win32" -> "NUL"
    | _ -> invalid_arg "invalid os type"

  let integrity_check ?ppf ~auto_repair t =
    let ppf =
      match ppf with
      | Some p -> p
      | None -> open_out null |> Format.formatter_of_out_channel
    in
    Fmt.pf ppf "Running the integrity_check.\n%!";
    let nb_commits = ref 0 in
    let nb_nodes = ref 0 in
    let nb_contents = ref 0 in
    let nb_absent = ref 0 in
    let nb_corrupted = ref 0 in
    let exception Cannot_fix in
    let contents = X.Repo.contents_t t in
    let nodes = X.Repo.node_t t |> snd in
    let commits = X.Repo.commit_t t |> snd in
    let pp_stats () =
      Fmt.pf ppf "\t%dk contents / %dk nodes / %dk commits\n%!"
        (!nb_contents / 1000) (!nb_nodes / 1000) (!nb_commits / 1000)
    in
    let count_increment count =
      incr count;
      if !count mod 1000 = 0 then pp_stats ()
    in
    let f (k, (offset, length, m)) =
      match m with
      | 'B' ->
          count_increment nb_contents;
          X.Contents.CA.integrity_check ~offset ~length k contents
      | 'N' | 'I' ->
          count_increment nb_nodes;
          X.Node.CA.integrity_check ~offset ~length k nodes
      | 'C' ->
          count_increment nb_commits;
          X.Commit.CA.integrity_check ~offset ~length k commits
      | _ -> invalid_arg "unknown content type"
    in
    if auto_repair then
      try
        Index.filter t.index (fun binding ->
            match f binding with
            | Ok () -> true
            | Error `Wrong_hash -> raise Cannot_fix
            | Error `Absent_value ->
                incr nb_absent;
                false);
        if !nb_absent = 0 then Ok `No_error else Ok (`Fixed !nb_absent)
      with Cannot_fix -> Error (`Cannot_fix "Not implemented")
    else (
      Index.iter
        (fun k v ->
          match f (k, v) with
          | Ok () -> ()
          | Error `Wrong_hash -> incr nb_corrupted
          | Error `Absent_value -> incr nb_absent)
        t.index;
      if !nb_absent = 0 && !nb_corrupted = 0 then Ok `No_error
      else Error (`Corrupted (!nb_corrupted + !nb_absent)))

  include Irmin.Of_private (X)

  let sync = X.Repo.sync
end

module Hash = Irmin.Hash.BLAKE2B
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None

module Make
    (Config : CONFIG)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module XNode = Irmin.Private.Node.Make (H) (P) (M)
  module XCommit = Irmin.Private.Commit.Make (H)
  include Make_ext (Config) (M) (C) (P) (B) (H) (XNode) (XCommit)
end

module KV (Config : CONFIG) (C : Irmin.Contents.S) =
  Make (Config) (Metadata) (C) (Path) (Irmin.Branch.String) (Hash)
module Stats = Stats
