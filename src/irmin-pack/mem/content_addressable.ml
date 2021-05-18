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

module Maker (V : Irmin_pack.Version.S) (K : Irmin.Hash.S) = struct
  type key = K.t

  module Make (Val : Irmin_pack.Pack_value.S with type hash := K.t) = struct
    (* This code is duplicated from irmin-mem except that it exposes non-lwt
       functions to be used by the inodes. It is different from the irmin-pack
       backend:
          - no support for readonly instances;
          - all opened instances share the same map. *)

    module KMap = Map.Make (struct
      type t = K.t

      let compare = Irmin.Type.(unstage (compare K.t))
    end)

    type key = K.t
    type value = Val.t
    type 'a t = { mutable t : value KMap.t }

    let map = { t = KMap.empty }
    let v () = Lwt.return map
    let equal_key = Irmin.Type.(unstage (equal K.t))

    let clear t =
      Log.debug (fun f -> f "clear");
      t.t <- KMap.empty;
      Lwt.return_unit

    let close _ =
      Log.debug (fun f -> f "close");
      Lwt.return_unit

    let cast t = (t :> read_write t)
    let batch t f = f (cast t)
    let pp_key = Irmin.Type.pp K.t

    let check_key k v =
      let k' = Val.hash v in
      if equal_key k k' then Ok () else Error (k, k')

    let find t k =
      try
        let v = KMap.find k t.t in
        check_key k v |> Result.map (fun () -> Some v)
      with Not_found -> Ok None

    let unsafe_find ~check_integrity:_ t k =
      Log.debug (fun f -> f "unsafe find %a" pp_key k);
      find t k |> function
      | Ok r -> r
      | Error (k, k') ->
          Fmt.invalid_arg "corrupted value: got %a, expecting %a" pp_key k'
            pp_key k

    let find t k =
      Log.debug (fun f -> f "find %a" pp_key k);
      find t k |> function
      | Ok r -> Lwt.return r
      | Error (k, k') ->
          Fmt.kstrf Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
            pp_key k' pp_key k

    let unsafe_mem t k =
      Log.debug (fun f -> f "mem %a" pp_key k);
      KMap.mem k t.t

    let mem t k =
      let b = unsafe_mem t k in
      Lwt.return b

    let unsafe_append ~ensure_unique:_ ~overcommit:_ t k v =
      Log.debug (fun f -> f "add -> %a" pp_key k);
      t.t <- KMap.add k v t.t

    let unsafe_add t k v =
      unsafe_append ~ensure_unique:true ~overcommit:true t k v;
      Lwt.return_unit

    let add t v =
      let k = Val.hash v in
      unsafe_add t k v >|= fun () -> k

    let flush ?index:_ ?index_merge:_ _t = ()

    let sync ?on_generation_change:_ _t =
      failwith "Readonly instances not supported"

    let version _ = V.version
    let offset _ = Int63.zero
    let generation _ = Int63.zero
    let integrity_check ~offset:_ ~length:_ _k _t = Ok ()
    let clear_caches t = t.t <- KMap.empty
    let clear_keep_generation t = clear t
  end
end
