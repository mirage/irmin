open Import
include Irmin_pack.Atomic_write

let current_version = `V1

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t [@@deriving irmin ~short_hash ~equal]

  let hash = short_hash ?seed:None
end)

module Make_persistent (K : Irmin.Type.S) (V : Value.S) = struct
  module Tbl = Table (K)
  module W = Irmin.Backend.Watch.Make (K) (V)
  module Io_legacy = Io_legacy.Unix

  type key = K.t [@@deriving irmin ~pp ~to_bin_string ~of_bin_string]
  type value = V.t [@@deriving irmin ~equal ~decode_bin ~of_bin_string]
  type watch = W.watch

  type t = {
    index : int63 Tbl.t;
    cache : V.t Tbl.t;
    block : Io_legacy.t;
    w : W.t;
  }

  let decode_bin = Irmin.Type.(unstage (decode_bin int32))

  let read_length32 ~file_pos block =
    let buf = Bytes.create 4 in
    let n = Io_legacy.read block ~off:!file_pos buf in
    assert (n = 4);
    (file_pos := Int63.Syntax.(!file_pos + Int63.of_int 4));
    let pos_ref = ref 0 in
    (* Bytes.unsafe_to_string usage: We assume Io_legacy.read_block returns unique
       ownership of buf back to this function (this assumption holds currently; subsequent
       modifications of that code need to ensure this remains the case); then in call to
       Bytes.unsafe_to_string we give up ownership of buf (we do not modify the buffer
       afterwards) and get ownership of resulting string; so this use is safe. *)
    let v = decode_bin (Bytes.unsafe_to_string buf) pos_ref in
    assert (!pos_ref = 4);
    Int32.to_int v

  let entry = Irmin.Type.(pair (string_of `Int32) V.t)
  let entry_to_bin_string = Irmin.Type.(unstage (to_bin_string entry))

  let set_entry t ?off k v =
    let k = key_to_bin_string k in
    let buf = entry_to_bin_string (k, v) in
    let () =
      match off with
      | None -> Io_legacy.append t.block buf
      | Some off -> Io_legacy.set t.block buf ~off
    in
    Io_legacy.flush t.block

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
          let n = Io_legacy.read t.block ~off:!file_pos buf in
          assert (n = buf_size);
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
    let former_offset = Io_legacy.offset t.block in
    let offset = Io_legacy.force_offset t.block in
    if offset > former_offset then refill t ~to_:offset ~from:former_offset

  let unsafe_find t k =
    [%log.debug "[branches] find %a" pp_key k];
    if Io_legacy.readonly t.block then sync_offset t;
    try Some (Tbl.find t.cache k) with Not_found -> None

  let find t k = Lwt.return (unsafe_find t k)

  let unsafe_mem t k =
    [%log.debug "[branches] mem %a" pp_key k];
    try Tbl.mem t.cache k with Not_found -> false

  let mem t v = Lwt.return (unsafe_mem t v)

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
      Io_legacy.v ~fresh ~version:(Some current_version) ~readonly file
    in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let t = { cache; index; block; w = watches } in
    let offset = Io_legacy.force_offset block in
    refill t ~to_:offset ~from:Int63.zero;
    Lwt.return t

  let clear _ = Fmt.failwith "Unsupported operation"

  let unsafe_set t k v =
    try
      let off = Tbl.find t.index k in
      Tbl.replace t.cache k v;
      set_entry t ~off k v
    with Not_found ->
      let offset = Io_legacy.offset t.block in
      set_entry t k v;
      Tbl.add t.cache k v;
      Tbl.add t.index k offset

  let set t k v =
    [%log.debug "[branches %s] set %a" (Io_legacy.name t.block) pp_key k];
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
    [%log.debug "[branches] test-and-set %a" pp_key k];
    unsafe_test_and_set t k ~test ~set >>= function
    | true -> W.notify t.w k set >|= fun () -> true
    | false -> Lwt.return_false

  let list t =
    [%log.debug "[branches] list"];
    let keys = Tbl.fold (fun k _ acc -> k :: acc) t.cache [] in
    Lwt.return keys

  let watch_key t = W.watch_key t.w
  let watch t = W.watch t.w
  let unwatch t = W.unwatch t.w

  let unsafe_close t =
    Tbl.reset t.index;
    Tbl.reset t.cache;
    if not (Io_legacy.readonly t.block) then Io_legacy.flush t.block;
    Io_legacy.close t.block;
    W.clear t.w

  let close t = unsafe_close t
  let flush t = Io_legacy.flush t.block
end
