(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
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

include Inode_layers_intf
open! Import

let src =
  Logs.Src.create "irmin.pack.i.layers"
    ~doc:"layered inodes for the irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)
module I = Inode

module Make
    (Conf : Irmin_pack.Config.S)
    (H : Irmin.Hash.S)
    (Pack_maker : S.LAYERED_PACK_MAKER
                    with type key = H.t
                     and type index = Pack_index.Make(H).t)
    (Node : Irmin.Private.Node.S with type hash = H.t) =
struct
  type index = Pack_maker.index

  module Inter = Inode.Make_intermediate (Conf) (H) (Node)
  module P = Pack_maker.Make (Inter.Elt)
  module Val = Inter
  module Key = H

  type 'a t = 'a P.t
  type key = Key.t
  type value = Val.t

  let mem t k = P.mem t k
  let unsafe_find = P.unsafe_find

  let find t k =
    P.find t k >|= function
    | None -> None
    | Some v ->
        let find =
          let f = unsafe_find ~check_integrity:true t in
          fun k -> Lwt.return (f k)
        in
        let v = Inter.of_bin find v in
        Some v

  let hash v = Inter.hash v
  let equal_hash = Irmin.Type.(unstage (equal H.t))

  let check_hash expected got =
    if equal_hash expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Inter.pp_hash
        expected Inter.pp_hash got

  let batch = P.batch
  let v = P.v
  let integrity_check = P.integrity_check
  let close = P.close
  let sync = P.sync
  let clear = P.clear
  let clear_caches = P.clear_caches

  let save t v =
    let add =
      let f = P.unsafe_append ~ensure_unique:true ~overcommit:false t in
      fun k v -> Lwt.return (f k v)
    in
    let mem =
      let f = P.unsafe_mem t in
      fun k -> Lwt.return (f k)
    in
    Inter.save ~add ~mem v

  let add t v =
    let+ () = save t v in
    hash v

  let unsafe_add t k v =
    check_hash k (hash v);
    save t v

  let clear_caches_next_upper = P.clear_caches_next_upper

  module U = P.U
  module L = P.L

  let layer_id = P.layer_id
  let mem_lower = P.mem_lower
  let lower = P.lower
  let mem_next = P.mem_next
  let flip_upper = P.flip_upper
  let next_upper = P.next_upper
  let current_upper = P.current_upper
  let consume_newies = P.consume_newies
  let update_flip = P.update_flip
  let flush ?index t = P.flush ?index t

  type 'a layer_type =
    | Upper : read U.t layer_type
    | Lower : read L.t layer_type

  let copy_from_lower ~dst t = P.copy_from_lower t "Node" ~dst

  let copy : type l. l layer_type * l -> read t -> key -> unit =
   fun (layer, dst) t ->
    match layer with
    | Lower -> P.copy (Lower, dst) t "Node"
    | Upper -> P.copy (Upper, dst) t "Node"

  let check = P.check

  let decode_bin ~dict ~hash buff off =
    Inter.decode_bin ~dict ~hash buff off |> fst

  let integrity_check_inodes _ _ = failwith "TODO"
end
