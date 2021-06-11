(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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

open! Import
include Atomic_write_intf
module Cache = IO.Cache

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t

  let hash = Irmin.Type.(unstage (short_hash K.t)) ?seed:None
  let equal = Irmin.Type.(unstage (equal K.t))
end)

module Make_persistent
    (Current : Version.S)
    (K : Irmin.Type.S)
    (V : Irmin.Hash.S) =
struct
  module Tbl = Table (K)
  module W = Irmin.Private.Watch.Make (K) (V)
  module IO = IO.Unix

  type key = K.t
  type value = V.t
  type watch = W.watch

  type t = {
    index : int63 Tbl.t;
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

  let refill t ~to_ ~from =
    let rec aux offset =
      if offset >= to_ then ()
      else
        let len = read_length32 ~off:offset t.block in
        let buf = Bytes.create (len + V.hash_size) in
        let off = offset ++ Int63.of_int 4 in
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
        (aux [@tailcall]) (off ++ Int63.(of_int @@ (len + V.hash_size)))
    in
    aux from

  let sync_offset t =
    let former_offset = IO.offset t.block in
    let former_generation = IO.generation t.block in
    let h = IO.force_headers t.block in
    if former_generation <> h.generation then (
      Log.debug (fun l -> l "[branches] generation changed, refill buffers");
      IO.close t.block;
      let io =
        IO.v ~fresh:false ~readonly:true ~version:(Some Current.version)
          (IO.name t.block)
      in
      t.block <- io;
      Tbl.clear t.cache;
      Tbl.clear t.index;
      refill t ~to_:h.offset ~from:Int63.zero)
    else if h.offset > former_offset then
      refill t ~to_:h.offset ~from:former_offset

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
    match Current.version with
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
    let block = IO.v ~fresh ~version:(Some Current.version) ~readonly file in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let t = { cache; index; block; w = watches; open_instances = 1 } in
    let h = IO.force_headers block in
    refill t ~to_:h.offset ~from:Int63.zero;
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

(* FIXME: remove code duplication with irmin/atomic_write *)
module Closeable (AW : S) = struct
  type t = { closed : bool ref; t : AW.t }
  type key = AW.key
  type value = AW.value

  let check_not_closed t = if !(t.closed) then raise Irmin.Closed

  let mem t k =
    check_not_closed t;
    AW.mem t.t k

  let find t k =
    check_not_closed t;
    AW.find t.t k

  let set t k v =
    check_not_closed t;
    AW.set t.t k v

  let test_and_set t k ~test ~set =
    check_not_closed t;
    AW.test_and_set t.t k ~test ~set

  let remove t k =
    check_not_closed t;
    AW.remove t.t k

  let list t =
    check_not_closed t;
    AW.list t.t

  type watch = AW.watch

  let watch t ?init f =
    check_not_closed t;
    AW.watch t.t ?init f

  let watch_key t k ?init f =
    check_not_closed t;
    AW.watch_key t.t k ?init f

  let unwatch t w =
    check_not_closed t;
    AW.unwatch t.t w

  let make_closeable t = { closed = ref false; t }

  let close t =
    if !(t.closed) then Lwt.return_unit
    else (
      t.closed := true;
      AW.close t.t)

  let clear t =
    check_not_closed t;
    AW.clear t.t

  let flush t =
    check_not_closed t;
    AW.flush t.t

  let clear_keep_generation t =
    check_not_closed t;
    AW.clear_keep_generation t.t
end
