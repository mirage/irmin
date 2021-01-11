include Store_intf

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

exception RO_Not_Allowed = IO.Unix.RO_Not_Allowed
exception Unsupported_version of IO.version

let ( ++ ) = Int64.add

module Cache = IO.Cache
open Lwt.Infix
module Pack = Pack
module Dict = Pack_dict
module Index = Pack_index

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = Irmin.Type.(unstage (short_hash K.t)) ?seed:None
  let equal = Irmin.Type.(unstage (equal K.t))
end)

let pp_version = IO.pp_version

module Atomic_write
    (K : Irmin.Type.S)
    (V : Irmin.Hash.S)
    (IO_version : IO.VERSION) =
struct
  let current_version = IO_version.io_version

  module Tbl = Table (K)
  module W = Irmin.Private.Watch.Make (K) (V)
  module IO = IO.Unix

  type key = K.t
  type value = V.t
  type watch = W.watch

  type t = {
    index : int64 Tbl.t;
    cache : V.t Tbl.t;
    mutable block : IO.t;
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

  let equal_val = Irmin.Type.(unstage (equal V.t))

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
        if not (equal_val v zero) then Tbl.add t.cache h v;
        Tbl.add t.index h offset;
        (aux [@tailcall]) (off ++ Int64.(of_int @@ (len + V.hash_size)))
    in
    aux from

  let sync_offset t =
    let former_generation = IO.generation t.block in
    let generation = IO.force_generation t.block in
    if former_generation <> generation then (
      Log.debug (fun l -> l "[branches] generation changed, refill buffers");
      IO.close t.block;
      let io =
        IO.v ~fresh:false ~readonly:true ~version:(Some current_version)
          (IO.name t.block)
      in
      t.block <- io;
      Tbl.clear t.cache;
      Tbl.clear t.index;
      refill t ~from:0L)
    else
      let former_log_offset = IO.offset t.block in
      let log_offset = IO.force_offset t.block in
      if log_offset > former_log_offset then refill t ~from:former_log_offset

  let unsafe_find t k =
    Log.debug (fun l -> l "[branches] find %a" pp_branch k);
    if IO.readonly t.block then sync_offset t;
    try Some (Tbl.find t.cache k) with Not_found -> None

  let find t k = Lwt.return (unsafe_find t k)

  let unsafe_mem t k =
    Log.debug (fun l -> l "[branches] mem %a" pp_branch k);
    try Tbl.mem t.cache k with Not_found -> false

  let mem t v = Lwt.return (unsafe_mem t v)

  let unsafe_remove t k =
    Tbl.remove t.cache k;
    try
      let off = Tbl.find t.index k in
      set_entry t ~off k zero
    with Not_found -> ()

  let remove t k =
    Log.debug (fun l -> l "[branches] remove %a" pp_branch k);
    unsafe_remove t k;
    W.notify t.w k None

  let unsafe_clear ?keep_generation t =
    Lwt.async (fun () -> W.clear t.w);
    match current_version with
    | `V1 -> IO.truncate t.block
    | `V2 ->
        IO.clear ?keep_generation t.block;
        Tbl.clear t.cache;
        Tbl.clear t.index

  let clear t =
    Log.debug (fun l -> l "[branches] clear");
    unsafe_clear t;
    Lwt.return_unit

  let clear_keep_generation t =
    Log.debug (fun l -> l "[branches] clear");
    unsafe_clear ~keep_generation:() t;
    Lwt.return_unit

  let watches = W.v ()

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false

  let unsafe_v ~fresh ~readonly file =
    let block = IO.v ~fresh ~version:(Some current_version) ~readonly file in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let t = { cache; index; block; w = watches; open_instances = 1 } in
    refill t ~from:0L;
    t

  let Cache.{ v = unsafe_v } =
    Cache.memoize ~clear:unsafe_clear ~valid
      ~v:(fun () -> unsafe_v)
      Layout.branch

  let v ?fresh ?readonly file = Lwt.return (unsafe_v () ?fresh ?readonly file)

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
    Log.debug (fun l -> l "[branches %s] set %a" (IO.name t.block) pp_branch k);
    unsafe_set t k v;
    W.notify t.w k (Some v)

  let equal_v_opt = Irmin.Type.(unstage (equal (option V.t)))

  let unsafe_test_and_set t k ~test ~set =
    let v = try Some (Tbl.find t.cache k) with Not_found -> None in
    if not (equal_v_opt v test) then Lwt.return_false
    else
      let return () = Lwt.return_true in
      match set with
      | None -> unsafe_remove t k |> return
      | Some v -> unsafe_set t k v |> return

  let test_and_set t k ~test ~set =
    Log.debug (fun l -> l "[branches] test-and-set %a" pp_branch k);
    unsafe_test_and_set t k ~test ~set >>= function
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

  let close t = unsafe_close t
  let flush t = IO.flush t.block
end

module IO = IO.Unix

let latest_version = `V2

(** Migrate data from the IO [src] (with [name] in path [root_old]) into the
    temporary dir [root_tmp], then swap in the replaced version. *)
let migrate_io_to_v2 ~progress src =
  IO.migrate ~progress src `V2 |> function
  | Ok () -> IO.close src
  | Error (`Msg s) -> invalid_arg s

let migrate config =
  if Config.readonly config then raise RO_Not_Allowed;
  Log.debug (fun l -> l "[%s] migrate" (Config.root config));
  Layout.stores ~root:(Config.root config)
  |> List.map (fun store ->
         let io = IO.v ~version:None ~fresh:false ~readonly:true store in
         let version = IO.version io in
         (store, io, version))
  |> List.partition (fun (_, _, v) -> v = latest_version)
  |> function
  | migrated, [] ->
      Log.info (fun l ->
          l "Store at %s is already in current version (%a)"
            (Config.root config) pp_version latest_version);
      List.iter (fun (_, io, _) -> IO.close io) migrated
  | migrated, to_migrate ->
      List.iter (fun (_, io, _) -> IO.close io) migrated;
      (match migrated with
      | [] -> ()
      | _ :: _ ->
          let pp_ios = Fmt.(Dump.list (using (fun (n, _, _) -> n) string)) in
          Log.warn (fun l ->
              l
                "Store is in an inconsistent state: files %a have already been \
                 upgraded, but %a have not. Upgrading the remaining files now."
                pp_ios migrated pp_ios to_migrate));
      let total =
        to_migrate
        |> List.map (fun (_, io, _) -> IO.offset io)
        |> List.fold_left Int64.add 0L
      in
      let bar, progress =
        Utils.Progress.counter ~total ~sampling_interval:100
          ~message:"Migrating store" ~pp_count:Utils.pp_bytes ()
      in
      List.iter (fun (_, io, _) -> migrate_io_to_v2 ~progress io) to_migrate;
      Utils.Progress.finalise bar

module Checks (Index : Pack_index.S) = struct
  let null =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> "/dev/null"
    | "Win32" -> "NUL"
    | _ -> invalid_arg "invalid os type"

  let integrity_check ?ppf ~auto_repair ~check index =
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
          check ~kind:`Contents ~offset ~length k
      | 'N' | 'I' ->
          count_increment nb_nodes;
          check ~kind:`Node ~offset ~length k
      | 'C' ->
          count_increment nb_commits;
          check ~kind:`Commit ~offset ~length k
      | _ -> invalid_arg "unknown content type"
    in
    if auto_repair then
      try
        Index.filter index (fun binding ->
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
        index;
      if !nb_absent = 0 && !nb_corrupted = 0 then Ok `No_error
      else Error (`Corrupted (!nb_corrupted + !nb_absent)))
end
