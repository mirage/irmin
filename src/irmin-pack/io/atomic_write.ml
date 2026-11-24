open Import
include Irmin_pack.Atomic_write

module Table (K : Irmin.Type.S) = struct
  module K = struct
    include K

    type t = K.t [@@deriving irmin ~short_hash ~equal]

    let hash = short_hash ?seed:None
    let equal = Irmin.Type.(unstage (equal K.t))
  end

  include Kcas_data.Hashtbl

  let create min_buckets = create ~hashed_type:(module K) ~min_buckets ()
end

module Make_persistent (Io : Io_intf.S) (K : Irmin.Type.S) (V : Value.S) =
struct
  module Tbl = Table (K)
  module W = Irmin.Backend.Watch.Make (K) (V)
  module Io_errors = Io_errors.Make (Io)

  type key = K.t [@@deriving irmin ~pp ~to_bin_string ~of_bin_string]
  type value = V.t [@@deriving irmin ~equal ~decode_bin ~of_bin_string]
  type watch = W.watch

  type t = {
    index : (K.t, int63) Tbl.t;
    cache : (K.t, V.t) Tbl.t;
    block : Io.t;
    lock : Eio.Mutex.t;
    block_size : int63 Atomic.t;
    w : W.t;
  }

  let dead_header_size = 16
  let decode_bin = Irmin.Type.(unstage (decode_bin int32))
  let length_size = 4

  let read_length32 ~file_pos block =
    let buf = Bytes.create length_size in
    Io.read_exn block ~off:!file_pos ~len:length_size buf;
    (file_pos := Int63.Syntax.(!file_pos + Int63.of_int length_size));
    let pos_ref = ref 0 in
    let v = decode_bin (Bytes.unsafe_to_string buf) pos_ref in
    assert (!pos_ref = length_size);
    Int32.to_int v

  let entry = Irmin.Type.(pair (string_of `Int32) V.t)
  let entry_to_bin_string = Irmin.Type.(unstage (to_bin_string entry))
  let block_size block = Io_errors.raise_if_error (Io.read_size block)

  let set_entry t ?off k v =
    let k = key_to_bin_string k in
    let buf = entry_to_bin_string (k, v) in
    let len = String.length buf in
    let off = match off with None -> block_size t.block | Some off -> off in
    Io.write_exn t.block ~off ~len buf

  let value_encoded_size =
    match Irmin.Type.Size.of_value V.t with
    | Repr.Size.Static n -> n
    | Dynamic _ | Unknown ->
        failwith
          "Irmin_pack.Atomic_write: supplied value type must have a \
           fixed-width binary encoding"

  (* Read the entries from [t.block_size] to [to_]. [t.block_size] moves to
     the end of each complete entry, so that a next refill starts after the
     entries read before an error. A truncated entry at the end may be in the
     middle of its write: it is read later. The lengths come from the file:
     they are checked before use. *)
  let refill t ~to_ =
    let decoding_error () = Io_errors.raise_error `Decoding_error in
    let file_pos = ref (Atomic.get t.block_size) in
    let remaining () = Int63.to_int Int63.Syntax.(to_ - !file_pos) in
    let rec aux () =
      let start = !file_pos in
      if remaining () < length_size then ()
      else
        let key_encoded_size = read_length32 ~file_pos t.block in
        if key_encoded_size < 0 then decoding_error ()
        else if key_encoded_size > remaining () - value_encoded_size then
          [%log.warn
            "[branches] %a: entry beyond the end of the file at offset %a, not \
             read yet"
            Eio.Path.pp (Io.path t.block) Int63.pp start]
        else
          let buf_size = key_encoded_size + value_encoded_size in
          let buf =
            let buf = Bytes.create buf_size in
            Io.read_exn t.block ~off:!file_pos ~len:buf_size buf;
            let open Int63.Syntax in
            file_pos := !file_pos + Int63.of_int buf_size;
            Bytes.unsafe_to_string buf
          in
          let key =
            match String.sub buf 0 key_encoded_size |> key_of_bin_string with
            | Ok k -> k
            | Error (`Msg e) ->
                [%log.err "[branches] cannot decode a key: %s" e];
                decoding_error ()
          in
          let value =
            let pos_ref = ref key_encoded_size in
            let v = decode_bin_value buf pos_ref in
            if !pos_ref <> buf_size then decoding_error ();
            v
          in
          if not (equal_value value V.null) then Tbl.add t.cache key value;
          Tbl.add t.index key start;
          Atomic.set t.block_size !file_pos;
          (aux [@tailcall]) ()
    in
    aux ()

  let sync_offset t =
    let newer_offset = block_size t.block in
    if newer_offset > Atomic.get t.block_size then
      (* An exception raised under the lock would poison it: it is raised
         again once the lock is released *)
      let result =
        Eio.Mutex.use_rw ~protect:true t.lock @@ fun () ->
        if newer_offset > Atomic.get t.block_size then
          match refill t ~to_:newer_offset with
          | () -> Ok ()
          | exception exn -> Error (exn, Printexc.get_raw_backtrace ())
        else Ok ()
      in
      match result with
      | Ok () -> ()
      | Error (exn, bt) -> Printexc.raise_with_backtrace exn bt

  let unsafe_find t k =
    [%log.debug "[branches] find %a" pp_key k];
    if Io.readonly t.block then sync_offset t;
    try Some (Tbl.find t.cache k) with Not_found -> None

  let find t k = unsafe_find t k

  let unsafe_mem t k =
    [%log.debug "[branches] mem %a" pp_key k];
    try Tbl.mem t.cache k with Not_found -> false

  let mem t v = unsafe_mem t v

  let unsafe_remove t k =
    Tbl.remove t.cache k;
    try
      let off = Tbl.find t.index k in
      set_entry t ~off k V.null
    with Not_found -> ()

  let remove t k =
    [%log.debug "[branches] remove %a" pp_key k];
    unsafe_remove t k;
    W.notify t.w k None

  let watches = W.v ()

  let v ~sw ?(fresh = false) ?(readonly = false) file =
    let block =
      if
        (not readonly)
        && (fresh || Io.classify_path file = `No_such_file_or_directory)
      then (
        let io =
          Io_errors.raise_if_error (Io.create ~sw ~path:file ~overwrite:true)
        in
        Io.write_exn io ~off:Int63.zero ~len:dead_header_size
          (String.make dead_header_size '\000');
        io)
      else Io_errors.raise_if_error (Io.open_ ~sw ~path:file ~readonly)
    in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let size = block_size block in
    let block_size = Atomic.make (Int63.of_int dead_header_size) in
    let lock = Eio.Mutex.create () in
    let t = { cache; index; block; block_size; lock; w = watches } in
    refill t ~to_:size;
    (* A writer appends after the end of the file: it cannot leave a truncated
       entry, from a crash, before the next ones *)
    if Atomic.get t.block_size <> size && not readonly then (
      [%log.err
        "[branches] %a: truncated entry at offset %a" Eio.Path.pp file Int63.pp
          (Atomic.get t.block_size)];
      Io_errors.raise_error `Decoding_error);
    t

  let clear _ = Fmt.failwith "Unsupported operation"

  let unsafe_set t k v =
    try
      let off = Tbl.find t.index k in
      Tbl.replace t.cache k v;
      set_entry t ~off k v
    with Not_found ->
      let offset = block_size t.block in
      set_entry t k v;
      Tbl.add t.cache k v;
      Tbl.add t.index k offset

  let set t k v =
    [%log.debug "[branches %a] set %a" Eio.Path.pp (Io.path t.block) pp_key k];
    unsafe_set t k v;
    W.notify t.w k (Some v)

  let equal_v_opt = Irmin.Type.(unstage (equal (option V.t)))

  let unsafe_test_and_set t k ~test ~set =
    let v = try Some (Tbl.find t.cache k) with Not_found -> None in
    if not (equal_v_opt v test) then false
    else
      match set with
      | None ->
          unsafe_remove t k;
          true
      | Some v ->
          unsafe_set t k v;
          true

  let test_and_set t k ~test ~set =
    [%log.debug "[branches] test-and-set %a" pp_key k];
    match unsafe_test_and_set t k ~test ~set with
    | true ->
        W.notify t.w k set;
        true
    | false -> false

  let list t =
    [%log.debug "[branches] list"];
    Tbl.fold (fun k _ acc -> k :: acc) t.cache []

  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let unsafe_close t =
    Tbl.reset t.index;
    Tbl.reset t.cache;
    Io_errors.raise_if_error (Io.close t.block);
    W.clear t.w

  let close t = unsafe_close t
  let flush _t = ()
end
