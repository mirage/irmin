open Import
include Irmin_pack.Atomic_write

module Table (K : Irmin.Type.S) = struct
  module K = (struct
    include K

    type t = K.t [@@deriving irmin ~short_hash ~equal]

    let hash = short_hash ?seed:None
    let equal = Irmin.Type.(unstage (equal K.t))
  end)

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
    mutable block_size : int63;
    w : W.t;
  }

  let dead_header_size = 16
  let decode_bin = Irmin.Type.(unstage (decode_bin int32))

  let read_length32 ~file_pos block =
    let len = 4 in
    let buf = Bytes.create len in
    Io.read_exn block ~off:!file_pos ~len buf;
    (file_pos := Int63.Syntax.(!file_pos + Int63.of_int len));
    let pos_ref = ref 0 in
    let v = decode_bin (Bytes.unsafe_to_string buf) pos_ref in
    assert (!pos_ref = len);
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

  let refill t ~to_ ~from =
    let file_pos = ref from in
    let rec aux () =
      if !file_pos >= to_ then ()
      else
        let start = !file_pos in
        let key_encoded_size = read_length32 ~file_pos t.block in
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
          | Error (`Msg e) -> failwith e
        in
        let value =
          let pos_ref = ref key_encoded_size in
          let v = decode_bin_value buf pos_ref in
          assert (!pos_ref = buf_size);
          v
        in
        if not (equal_value value V.null) then Tbl.add t.cache key value;
        Tbl.add t.index key start;
        (aux [@tailcall]) ()
    in
    aux ()

  let sync_offset t =
    let former_offset = t.block_size in
    t.block_size <- block_size t.block;
    if t.block_size > former_offset then
      refill t ~to_:t.block_size ~from:former_offset

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

  let v ?(fresh = false) ?(readonly = false) file =
    let block =
      if
        (not readonly)
        && (fresh || Io.classify_path file = `No_such_file_or_directory)
      then (
        let io =
          Io_errors.raise_if_error (Io.create ~path:file ~overwrite:true)
        in
        Io.write_exn io ~off:Int63.zero ~len:dead_header_size
          (String.make dead_header_size '\000');
        io)
      else Io_errors.raise_if_error (Io.open_ ~path:file ~readonly)
    in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let block_size = block_size block in
    let t = { cache; index; block; block_size; w = watches } in
    refill t ~to_:block_size ~from:(Int63.of_int dead_header_size);
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
    [%log.debug "[branches %s] set %a" (Io.path t.block) pp_key k];
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
