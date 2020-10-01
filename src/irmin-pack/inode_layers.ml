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

open Lwt.Infix

let src =
  Logs.Src.create "irmin.pack.i.layers"
    ~doc:"layered inodes for the irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

module I = Inode

module Make_ext
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S with type hash = H.t)
    (Inode : I.INODE_EXT with type hash = H.t)
    (Val : I.VAL_INTER
             with type hash = H.t
              and type inode_val = Inode.Val.t
              and type metadata = Node.metadata
              and type step = Node.step) :
  I.S_EXT with type key = H.t and type 'a t = 'a Inode.t and type value = Val.t =
struct
  include I.Make_ext (H) (Node) (Inode) (Val)
end

module Make
    (Conf : Config.S)
    (H : Irmin.Hash.S)
    (Pack : S.LAYERED_MAKER
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

  include Make_ext (H) (Node) (Inode) (Val)

  let save t v =
    let add k v = Inode.unsafe_append t k v in
    Inode.Val.save_lwt ~add ~mem:(Inode.unsafe_mem t) v

  let add t v = save t v.Val.v >|= fun () -> hash v

  let unsafe_add t k v =
    check_hash k (hash v);
    save t v.Val.v

  let v = Inode.v

  let sync ?on_generation_change ?on_generation_change_next_upper t =
    Inode.sync ?on_generation_change ?on_generation_change_next_upper t

  let clear_caches = Inode.clear_caches

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
    Irmin_layers.Stats.copy_nodes ();
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
