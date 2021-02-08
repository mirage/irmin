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

let src = Logs.Src.create "irmin.pack.mem" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)
open Lwt.Infix

let current_version = `V2

module IO_cache = Irmin_pack.Private.IO.Cache
module IO = Irmin_pack.Private.IO.Unix

module File (K : Irmin.Hash.S) (IO_version : Irmin_pack.VERSION) :
  Irmin_pack.Pack.MAKER with type key = K.t = struct
  type key = K.t
  type 'a t = { mutable block : IO.t; mutable open_instances : int }

  let clear ?keep_generation t = IO.clear ?keep_generation t.block
  let flush t = IO.flush t.block
  let offset t = IO.offset t.block
  let generation t = IO.generation t.block

  let valid t =
    if t.open_instances <> 0 then (
      t.open_instances <- t.open_instances + 1;
      true)
    else false

  let unsafe_v ~fresh ~readonly file =
    let block = IO.v ~version:(Some current_version) ~fresh ~readonly file in
    { block; open_instances = 1 }

  let IO_cache.{ v } =
    IO_cache.memoize ~clear ~valid
      ~v:(fun () -> unsafe_v)
      Irmin_pack.Layout.pack

  let close t =
    t.open_instances <- t.open_instances - 1;
    if t.open_instances = 0 then (
      if not (IO.readonly t.block) then IO.flush t.block;
      IO.close t.block)

  module Make (V : Irmin_pack.Pack.ELT with type hash := K.t) :
    Irmin_pack.Pack.S with type key = K.t and type value = V.t = struct
    module KMap = Hashtbl.Make (struct
      type t = K.t

      let hash = Hashtbl.hash
      let equal = Irmin.Type.(unstage (equal K.t))
    end)

    type key = K.t

    let equal_key = Irmin.Type.(unstage (equal K.t))

    type value = V.t

    type nonrec 'a t = {
      t : value KMap.t;
      root : string;
      pack : 'a t;
      readonly : bool;
      mutable open_instances : int;
    }

    let _decode_value = Irmin.Type.(unstage (decode_bin V.t))
    let encode_key = Irmin.Type.(unstage (encode_bin K.t))
    let encode_value = Irmin.Type.(unstage (encode_bin V.t))
    let encode_magic = Irmin.Type.(unstage (encode_bin char))
    let pp_key = Irmin.Type.pp K.t

    let sync ?on_generation_change:_ t =
      let former_generation = IO.generation t.pack.block in
      let generation = IO.force_generation t.pack.block in
      if former_generation <> generation then (
        Log.debug (fun l -> l "[%s] generation changed, refill buffers" t.root);
        KMap.clear t.t;
        IO.close t.pack.block;
        let block =
          IO.v ~fresh:false ~version:(Some current_version) ~readonly:true
            (IO.name t.pack.block)
        in
        t.pack.block <- block)

    let _refill t =
      let former_offset = IO.offset t.pack.block in
      let offset = IO.force_offset t.pack.block in
      Log.debug (fun l -> l "former = %Ld now = %Ld" former_offset offset);
      (former_offset, offset, t.pack.block)

    let roots = Hashtbl.create 10

    let unsafe_clear ?keep_generation t =
      Log.debug (fun f -> f "[%s] clear" t.root);
      KMap.clear t.t;
      clear ?keep_generation t.pack

    let clear t =
      unsafe_clear t;
      Lwt.return_unit

    let clear_keep_generation t =
      unsafe_clear ~keep_generation:() t;
      Lwt.return_unit

    let valid t =
      if t.open_instances <> 0 then (
        t.open_instances <- t.open_instances + 1;
        true)
      else false

    let unsafe_v_no_cache ~fresh ~readonly root =
      Log.debug (fun l -> l "[%s] v fresh =%b readonly =%b" root fresh readonly);
      let pack = v () ~fresh ~readonly root in
      { t = KMap.create 0; root; pack; readonly; open_instances = 1 }

    let unsafe_v ?(fresh = false) ?(readonly = false) root =
      try
        let t = Hashtbl.find roots (root, readonly) in
        if valid t then (
          if fresh then unsafe_clear t;
          t)
        else (
          Hashtbl.remove roots (root, readonly);
          raise Not_found)
      with Not_found ->
        let t = unsafe_v_no_cache ~fresh ~readonly root in
        if fresh then unsafe_clear t;
        Hashtbl.add roots (root, readonly) t;
        t

    let v ?fresh ?readonly root =
      let t = unsafe_v ?fresh ?readonly root in
      Lwt.return t

    let flush ?index:_ ?index_merge:_ t = flush t.pack

    let unsafe_close t =
      t.open_instances <- t.open_instances - 1;
      if t.open_instances = 0 then (
        Log.debug (fun f -> f "[%s] close" t.root);
        KMap.clear t.t;
        close t.pack)

    let close t =
      unsafe_close t;
      Lwt.return_unit

    let offset t = offset t.pack
    let generation t = generation t.pack
    let cast t = (t :> [ `Read | `Write ] t)

    let batch t f =
      f (cast t) >>= fun r ->
      flush t;
      Lwt.return r

    let check_key k v =
      let k' = V.hash v in
      if equal_key k k' then Ok () else Error (k, k')

    let find t k =
      try
        let v = KMap.find t.t k in
        check_key k v |> Result.map (fun () -> Some v)
      with Not_found -> Ok None

    let unsafe_find ~check_integrity:_ t k =
      Log.debug (fun f -> f "[%s] unsafe find %a" t.root pp_key k);
      find t k |> function
      | Ok r -> r
      | Error (k, k') ->
          Fmt.invalid_arg "corrupted value: got %a, expecting %a" pp_key k'
            pp_key k

    let find t k =
      Log.debug (fun f -> f "[%s] find %a" t.root pp_key k);
      find t k |> function
      | Ok r -> Lwt.return r
      | Error (k, k') ->
          Fmt.kstrf Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
            pp_key k' pp_key k

    let unsafe_mem t k =
      Log.debug (fun f -> f "[%s] mem %a" t.root pp_key k);
      KMap.mem t.t k

    let mem t k =
      let b = unsafe_mem t k in
      Lwt.return b

    let unsafe_append ~ensure_unique:_ ~overcommit:_ t k v =
      Log.debug (fun f -> f "[%s] add -> %a" t.root pp_key k);
      KMap.add t.t k v;
      encode_key k (IO.append t.pack.block);
      encode_magic (V.magic v) (IO.append t.pack.block);
      encode_value v (IO.append t.pack.block)

    let unsafe_add t k v =
      unsafe_append ~ensure_unique:true ~overcommit:true t k v;
      Lwt.return_unit

    let add t v =
      let k = V.hash v in
      unsafe_add t k v >|= fun () -> k

    let _add_in_mem t k v = KMap.add t.t k v
    let version _ = current_version
    let clear_caches t = KMap.clear t.t
    let integrity_check ~offset:_ ~length:_ _k _t = Ok ()
  end
end
