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
open Lwt.Infix

let src =
  Logs.Src.create "irmin.pack.i.layers"
    ~doc:"layered inodes for the irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

module I = Inode

module Make
    (Conf : Config.S)
    (H : Irmin.Hash.S)
    (Pack : Pack.LAYERED_MAKER
              with type key = H.t
               and type index = Pack_index.Make(H).t)
    (Node : Irmin.Private.Node.S with type hash = H.t) =
struct
  type index = Pack.index

  include Inode.Make_intermediate (Conf) (H) (Node)

  module Inode = struct
    include Inode
    include Pack.Make (Elt)
  end

  module Key = H

  type 'a t = 'a Inode.t

  type key = Key.t

  type value = Val.t

  let mem t k = Inode.mem t k

  let unsafe_find t k =
    match Inode.unsafe_find t k with
    | None -> None
    | Some v ->
        let v = Inode.Val.of_bin v in
        Some v

  let find t k =
    Inode.find t k >|= function
    | None -> None
    | Some v ->
        let v = Inode.Val.of_bin v in
        let find = unsafe_find t in
        Some { Val.find; v }

  let hash v = Inode.Val.hash v.Val.v

  let check_hash expected got =
    if Irmin.Type.equal H.t expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Inode.pp_hash
        expected Inode.pp_hash got

  let batch = Inode.batch

  let v = Inode.v

  let integrity_check = Inode.integrity_check

  let close = Inode.close

  let sync = Inode.sync

  let clear = Inode.clear

  let clear_caches = Inode.clear_caches

  let save t v =
    let add k v = Inode.unsafe_append t k v in
    Inode.Val.save_lwt ~add ~mem:(Inode.unsafe_mem t) v

  let add t v = save t v.Val.v >|= fun () -> hash v

  let unsafe_add t k v =
    check_hash k (hash v);
    save t v.Val.v

  let clear_caches_next_upper = Inode.clear_caches_next_upper

  module U = Inode.U
  module L = Inode.L

  let layer_id = Inode.layer_id

  let mem_lower = Inode.mem_lower

  let lower = Inode.lower

  let mem_next = Inode.mem_next

  let flip_upper = Inode.flip_upper

  let next_upper = Inode.next_upper

  let copy_newies_to_next_upper = Inode.copy_newies_to_next_upper

  let copy_last_newies_to_next_upper = Inode.copy_last_newies_to_next_upper

  let update_flip = Inode.update_flip

  let clear_previous_upper = Inode.clear_previous_upper

  let unsafe_find t k =
    match Inode.unsafe_find t k with
    | None -> None
    | Some v ->
        let v = Inode.Val.of_bin v in
        Some v

  let lift t v =
    let v = Inode.Val.of_bin v in
    let find = unsafe_find t in
    { Val.find; v }

  type 'a layer_type =
    | Upper : [ `Read ] U.t layer_type
    | Lower : [ `Read ] L.t layer_type

  let pause = Lwt.pause

  let copy ~add ~mem t k =
    Log.debug (fun l -> l "copy Node %a" (Irmin.Type.pp Key.t) k);
    Inode.U.find (Inode.current_upper t) k >>= function
    | None -> pause ()
    | Some v ->
        pause () >>= fun () ->
        let v' = lift t v in
        (* copy is called right after [of_bin] and the inodes have empty cached
           trees. We call [list] here to add the cached tree to the inodes, so that
           they are copied in the dst layer. *)
        List.iter ignore (Val.list v');
        let add k v =
          Irmin_layers.Stats.copy_nodes ();
          add k v;
          pause ()
        in
        let mem k = mem k |> Lwt.return in
        pause () >>= fun () -> Inode.Val.save_lwt ~add ~mem v'.Val.v

  let copy_to_lower ~dst t k =
    let add k v = Inode.L.unsafe_append dst k v in
    let mem k = Inode.L.unsafe_mem dst k in
    copy ~add ~mem t k

  let copy_to_next ~dst t k =
    let add k v = Inode.U.unsafe_append dst k v in
    let mem k = Inode.U.unsafe_mem dst k in
    copy ~add ~mem t k

  let copy : type l. l layer_type * l -> [ `Read ] t -> key -> unit Lwt.t =
   fun (ltype, dst) ->
    match ltype with Lower -> copy_to_lower ~dst | Upper -> copy_to_next ~dst
end
