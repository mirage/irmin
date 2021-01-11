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

open Irmin_pack

let src = Logs.Src.create "irmin.layers" ~doc:"Irmin layered store"

module Log = (val Logs.src_log src : Logs.LOG)
open Lwt.Infix

module type CA = sig
  include Pack.S
  module Key : Irmin.Hash.TYPED with type t = key and type value = value
end

let stats = function
  | "Contents" -> Irmin_layers.Stats.copy_contents ()
  | "Node" -> Irmin_layers.Stats.copy_nodes ()
  | "Commit" -> Irmin_layers.Stats.copy_commits ()
  | _ -> failwith "unexpected type in stats"

module Copy
    (Key : Irmin.Hash.S)
    (SRC : Pack.S with type key = Key.t)
    (DST : Pack.S with type key = SRC.key and type value = SRC.value) =
struct
  let ignore_lwt _ = Lwt.return_unit

  let copy ~src ~dst str k =
    Log.debug (fun l -> l "copy %s %a" str (Irmin.Type.pp Key.t) k);
    match SRC.unsafe_find ~check_integrity:false src k with
    | None ->
        Log.warn (fun l ->
            l "Attempt to copy %s %a not contained in upper." str
              (Irmin.Type.pp Key.t) k)
    | Some v ->
        stats str;
        DST.unsafe_append ~ensure_unique:false ~overcommit:true dst k v

  let check ~src ?(some = ignore_lwt) ?(none = ignore_lwt) k =
    SRC.find src k >>= function None -> none () | Some v -> some v
end

let pp_during_freeze ppf = function
  | true -> Fmt.string ppf " during freeze"
  | false -> ()

let pp_layer_id = Irmin_layers.Layer_id.pp
let pp_current_upper ppf t = pp_layer_id ppf (if t then `Upper1 else `Upper0)
let pp_next_upper ppf t = pp_layer_id ppf (if t then `Upper0 else `Upper1)

module Content_addressable
    (H : Irmin.Hash.S)
    (Index : Private.Pack_index.S)
    (U : Pack.S with type index = Index.t and type key = H.t)
    (L : Pack.S
           with type index = U.index
            and type key = U.key
            and type value = U.value) =
struct
  type index = U.index
  type key = U.key
  type value = U.value

  type 'a t = {
    lower : [ `Read ] L.t option;
    mutable flip : bool;
    uppers : [ `Read ] U.t * [ `Read ] U.t;
    freeze_in_progress : unit -> bool;
    mutable newies : key list;
  }

  module U = U
  module L = L

  let v upper1 upper0 lower ~flip ~freeze_in_progress =
    Log.debug (fun l -> l "v flip = %b" flip);
    { lower; flip; uppers = (upper1, upper0); freeze_in_progress; newies = [] }

  let next_upper t = if t.flip then snd t.uppers else fst t.uppers
  let current_upper t = if t.flip then fst t.uppers else snd t.uppers
  let lower t = Option.get t.lower
  let pp_current_upper ppf t = pp_current_upper ppf t.flip
  let pp_next_upper ppf t = pp_next_upper ppf t.flip

  let mem_lower t k =
    match t.lower with None -> Lwt.return false | Some lower -> L.mem lower k

  let mem_next t k = U.mem (next_upper t) k

  let consume_newies t =
    let newies = t.newies in
    t.newies <- [];
    newies

  let add t v =
    let freeze = t.freeze_in_progress () in
    Log.debug (fun l ->
        l "add in %a%a" pp_current_upper t pp_during_freeze freeze);
    Irmin_layers.Stats.add ();
    let upper = current_upper t in
    U.add upper v >|= fun k ->
    if freeze then t.newies <- k :: t.newies;
    k

  let unsafe_add t k v =
    let freeze = t.freeze_in_progress () in
    Log.debug (fun l ->
        l "unsafe_add in %a%a" pp_current_upper t pp_during_freeze freeze);
    Irmin_layers.Stats.add ();
    let upper = current_upper t in
    U.unsafe_add upper k v >|= fun () ->
    if freeze then t.newies <- k :: t.newies

  let unsafe_append ~ensure_unique ~overcommit t k v =
    let freeze = t.freeze_in_progress () in
    Log.debug (fun l ->
        l "unsafe_append in %a%a" pp_current_upper t pp_during_freeze freeze);
    Irmin_layers.Stats.add ();
    let upper = current_upper t in
    U.unsafe_append ~ensure_unique ~overcommit upper k v;
    if freeze then t.newies <- k :: t.newies

  (** Everything is in current upper, no need to look in next upper. *)
  let find t k =
    let current = current_upper t in
    Log.debug (fun l -> l "find in %a" pp_current_upper t);
    U.find current k >>= function
    | Some v -> Lwt.return_some v
    | None -> (
        match t.lower with
        | None -> Lwt.return_none
        | Some lower ->
            Log.debug (fun l -> l "find in lower");
            L.find lower k)

  let unsafe_find ~check_integrity t k =
    let current = current_upper t in
    Log.debug (fun l -> l "unsafe_find in %a" pp_current_upper t);
    match U.unsafe_find ~check_integrity current k with
    | Some v -> Some v
    | None -> (
        match t.lower with
        | None -> None
        | Some lower ->
            Log.debug (fun l -> l "unsafe_find in lower");
            L.unsafe_find ~check_integrity lower k)

  let mem t k =
    let current = current_upper t in
    U.mem current k >>= function
    | true -> Lwt.return_true
    | false -> (
        match t.lower with
        | None -> Lwt.return_false
        | Some lower -> L.mem lower k)

  let unsafe_mem t k =
    let current = current_upper t in
    U.unsafe_mem current k
    || match t.lower with None -> false | Some lower -> L.unsafe_mem lower k

  (** Only flush current upper, to prevent concurrent flushing and appends
      during copy. Next upper and lower are flushed at the end of a freeze. *)
  let flush ?index ?index_merge t =
    let current = current_upper t in
    U.flush ?index ?index_merge current

  let flush_next_lower t =
    let next = next_upper t in
    U.flush ~index_merge:true next;
    match t.lower with None -> () | Some x -> L.flush ~index_merge:true x

  let cast t = (t :> [ `Read | `Write ] t)

  let batch t f =
    f (cast t) >|= fun r ->
    flush ~index:true t;
    r

  (** If the generation changed, then the upper changed too. TODO: This
      assumption is ok for now, but does not hold if:

      - the RW store is opened after the RO,
      - if RW is closed in the meantime,
      - if the RW freezes an even number of times before an RO sync.

      See https://github.com/mirage/irmin/issues/1225 *)
  let sync ?on_generation_change ?on_generation_change_next_upper t =
    Log.debug (fun l -> l "sync %a" pp_current_upper t);
    (* a first implementation where only the current upper is synced *)
    let current = current_upper t in
    let former_generation = U.generation current in
    U.sync ?on_generation_change current;
    let generation = U.generation current in
    if former_generation <> generation then (
      Log.debug (fun l -> l "generation change, RO updates upper");
      t.flip <- not t.flip;
      let current = current_upper t in
      U.sync ?on_generation_change:on_generation_change_next_upper current;
      match t.lower with None -> () | Some x -> L.sync ?on_generation_change x);
    t.flip

  let update_flip ~flip t = t.flip <- flip

  let close t =
    U.close (fst t.uppers) >>= fun () ->
    U.close (snd t.uppers) >>= fun () ->
    match t.lower with None -> Lwt.return_unit | Some x -> L.close x

  let integrity_check ~offset ~length ~layer k t =
    match layer with
    | `Upper1 -> U.integrity_check ~offset ~length k (fst t.uppers)
    | `Upper0 -> U.integrity_check ~offset ~length k (snd t.uppers)
    | `Lower -> L.integrity_check ~offset ~length k (lower t)

  let layer_id t k =
    let current, upper =
      if t.flip then (fst t.uppers, `Upper1) else (snd t.uppers, `Upper0)
    in
    U.mem current k >>= function
    | true -> Lwt.return upper
    | false -> (
        match t.lower with
        | None -> raise Not_found
        | Some lower -> (
            L.mem lower k >|= function
            | true -> `Lower
            | false -> raise Not_found))

  let clear t =
    U.clear (fst t.uppers) >>= fun () ->
    U.clear (snd t.uppers) >>= fun () ->
    match t.lower with None -> Lwt.return_unit | Some x -> L.clear x

  let clear_keep_generation t =
    U.clear_keep_generation (fst t.uppers) >>= fun () ->
    U.clear_keep_generation (snd t.uppers) >>= fun () ->
    match t.lower with
    | None -> Lwt.return_unit
    | Some x -> L.clear_keep_generation x

  let clear_caches t =
    let current = current_upper t in
    U.clear_caches current

  let clear_caches_next_upper t =
    let next = next_upper t in
    U.clear_caches next

  (** After clearing the previous upper, we also needs to flush current upper to
      disk, otherwise values are not found by the RO. *)
  let clear_previous_upper ?keep_generation t =
    let previous = next_upper t in
    let current = current_upper t in
    U.flush current;
    match keep_generation with
    | Some () -> U.clear_keep_generation previous
    | None -> U.clear previous

  let version t = U.version (fst t.uppers)

  let generation t =
    let current = current_upper t in
    U.generation current

  let offset t =
    let current = current_upper t in
    U.offset current

  let flip_upper t =
    Log.debug (fun l -> l "flip_upper to %a" pp_next_upper t);
    t.flip <- not t.flip

  module CopyUpper = Copy (H) (U) (U)
  module CopyLower = Copy (H) (U) (L)

  type 'a layer_type =
    | Upper : [ `Read ] U.t layer_type
    | Lower : [ `Read ] L.t layer_type

  let copy_to_lower t ~dst str k =
    CopyLower.copy ~src:(current_upper t) ~dst str k

  let copy_to_next t ~dst str k =
    CopyUpper.copy ~src:(current_upper t) ~dst str k

  let check t ?none ?some k =
    CopyUpper.check ~src:(current_upper t) ?none ?some k

  let copy : type l. l layer_type * l -> [ `Read ] t -> string -> key -> unit =
   fun (ltype, dst) ->
    match ltype with Lower -> copy_to_lower ~dst | Upper -> copy_to_next ~dst

  (** The object [k] can be in either lower or upper. If already in upper then
      do not copy it. *)
  let copy_from_lower t ~dst ?(aux = fun _ -> Lwt.return_unit) str k =
    (* FIXME(samoht): why does this function need to be different from the previous one? *)
    let lower = lower t in
    let current = current_upper t in
    U.find current k >>= function
    | Some v -> aux v
    | None -> (
        L.find lower k >>= function
        | Some v ->
            aux v >>= fun () ->
            stats str;
            U.unsafe_add dst k v
        | None -> Fmt.failwith "%s %a not found" str (Irmin.Type.pp H.t) k)
end

module Pack_Maker
    (H : Irmin.Hash.S)
    (Index : Private.Pack_index.S)
    (P : Pack.MAKER with type key = H.t and type index = Index.t) =
struct
  type index = P.index
  type key = P.key

  module Make (V : Pack.ELT with type hash := key) = struct
    module Upper = P.Make (V)
    include Content_addressable (H) (Index) (Upper) (Upper)
  end
end

module Atomic_write
    (K : Irmin.Branch.S)
    (U : S.ATOMIC_WRITE_STORE with type key = K.t)
    (L : S.ATOMIC_WRITE_STORE with type key = U.key and type value = U.value) =
struct
  type key = U.key
  type value = U.value

  module U = U
  module L = L

  type t = {
    lower : L.t option;
    mutable flip : bool;
    uppers : U.t * U.t;
    freeze_in_progress : unit -> bool;
    mutable newies : (key * value option) list;
  }

  let current_upper t = if t.flip then fst t.uppers else snd t.uppers
  let next_upper t = if t.flip then snd t.uppers else fst t.uppers
  let pp_current_upper ppf t = pp_current_upper ppf t.flip
  let pp_next_upper ppf t = pp_next_upper ppf t.flip
  let pp_branch = Irmin.Type.pp K.t

  let mem t k =
    let current = current_upper t in
    Log.debug (fun l ->
        l "[branches] mem %a in %a" pp_branch k pp_current_upper t);
    U.mem current k >>= function
    | true -> Lwt.return_true
    | false -> (
        match t.lower with
        | None -> Lwt.return_false
        | Some lower ->
            Log.debug (fun l -> l "[branches] mem in lower");
            L.mem lower k)

  let find t k =
    let current = current_upper t in
    Log.debug (fun l -> l "[branches] find in %a" pp_current_upper t);
    U.find current k >>= function
    | Some v -> Lwt.return_some v
    | None -> (
        match t.lower with
        | None -> Lwt.return_none
        | Some lower ->
            Log.debug (fun l -> l "[branches] find in lower");
            L.find lower k)

  let set t k v =
    let freeze = t.freeze_in_progress () in
    Log.debug (fun l ->
        l "[branches] set %a in %a%a" pp_branch k pp_current_upper t
          pp_during_freeze freeze);
    let upper = current_upper t in
    U.set upper k v >|= fun () ->
    if freeze then t.newies <- (k, Some v) :: t.newies

  (** Copy back into upper the branch against we want to do test and set. *)
  let test_and_set t k ~test ~set =
    let freeze = t.freeze_in_progress () in
    Log.debug (fun l ->
        l "[branches] test_and_set %a in %a%a" pp_branch k pp_current_upper t
          pp_during_freeze freeze);
    let current = current_upper t in
    let find_in_lower () =
      (match t.lower with
      | None -> Lwt.return_none
      | Some lower -> L.find lower k)
      >>= function
      | None -> U.test_and_set current k ~test:None ~set
      | Some v ->
          U.set current k v >>= fun () -> U.test_and_set current k ~test ~set
    in
    (U.mem current k >>= function
     | true -> U.test_and_set current k ~test ~set
     | false -> find_in_lower ())
    >|= fun update ->
    if update && freeze then t.newies <- (k, set) :: t.newies;
    update

  let remove t k =
    let freeze = t.freeze_in_progress () in
    Log.debug (fun l ->
        l "[branches] remove %a in %a%a" pp_branch k pp_current_upper t
          pp_during_freeze freeze);
    U.remove (fst t.uppers) k >>= fun () ->
    U.remove (snd t.uppers) k >>= fun () ->
    if freeze then t.newies <- (k, None) :: t.newies;
    match t.lower with
    | None -> Lwt.return_unit
    | Some lower -> L.remove lower k

  let list t =
    let current = current_upper t in
    U.list current >>= fun upper ->
    (match t.lower with None -> Lwt.return_nil | Some lower -> L.list lower)
    >|= fun lower ->
    List.fold_left
      (fun acc b -> if List.mem b acc then acc else b :: acc)
      lower upper

  type watch = U.watch

  let watch t = U.watch (current_upper t)
  let watch_key t = U.watch_key (current_upper t)
  let unwatch t = U.unwatch (current_upper t)

  let close t =
    U.close (fst t.uppers) >>= fun () ->
    U.close (snd t.uppers) >>= fun () ->
    match t.lower with None -> Lwt.return_unit | Some x -> L.close x

  let v upper1 upper0 lower ~flip ~freeze_in_progress =
    { lower; flip; uppers = (upper1, upper0); freeze_in_progress; newies = [] }

  let clear t =
    U.clear (fst t.uppers) >>= fun () ->
    U.clear (snd t.uppers) >>= fun () ->
    match t.lower with None -> Lwt.return_unit | Some x -> L.clear x

  let flush t =
    let current = current_upper t in
    U.flush current

  (** Do not copy branches that point to commits not copied. *)
  let copy ~mem_commit_lower ~mem_commit_upper t =
    let next = next_upper t in
    let current = current_upper t in
    U.list current >>= fun branches ->
    Lwt_list.iter_p
      (fun branch ->
        U.find current branch >>= function
        | None -> Lwt.fail_with "branch not found in current upper"
        | Some hash -> (
            (match t.lower with
            | None -> Lwt.return_unit
            | Some lower -> (
                mem_commit_lower hash >>= function
                | true ->
                    Log.debug (fun l ->
                        l "[branches] copy to lower %a" (Irmin.Type.pp K.t)
                          branch);
                    Irmin_layers.Stats.copy_branches ();
                    L.set lower branch hash
                | false -> Lwt.return_unit))
            >>= fun () ->
            mem_commit_upper hash >>= function
            | true ->
                Log.debug (fun l ->
                    l "[branches] copy to next %a" (Irmin.Type.pp K.t) branch);
                Irmin_layers.Stats.copy_branches ();
                U.set next branch hash
            | false ->
                Log.debug (fun l ->
                    l "branch %a not copied" (Irmin.Type.pp K.t) branch);
                Lwt.return_unit))
      branches

  let flip_upper t =
    Log.debug (fun l -> l "[branches] flip to %a" pp_next_upper t);
    t.flip <- not t.flip

  (** After clearing the previous upper, we also needs to flush current upper to
      disk, otherwise values are not found by the RO. *)
  let clear_previous_upper ?keep_generation t =
    let current = current_upper t in
    let previous = next_upper t in
    U.flush current;
    match keep_generation with
    | Some () -> U.clear_keep_generation previous
    | None -> U.clear previous

  let flush_next_lower t =
    let next = next_upper t in
    U.flush next;
    match t.lower with None -> () | Some x -> L.flush x

  let copy_newies_to_next_upper t =
    Log.debug (fun l ->
        l "[branches] copy %d newies to %a" (List.length t.newies) pp_next_upper
          t);
    let next = next_upper t in
    let newies = t.newies in
    t.newies <- [];
    Lwt_list.iter_s
      (fun (k, v) ->
        match v with None -> U.remove next k | Some v -> U.set next k v)
      (List.rev newies)

  (** RO syncs the branch store at every find call, but it still needs to update
      the upper in use.*)
  let update_flip ~flip t = t.flip <- flip

  let clear_keep_generation t =
    U.clear_keep_generation (fst t.uppers) >>= fun () ->
    U.clear_keep_generation (snd t.uppers) >>= fun () ->
    match t.lower with
    | None -> Lwt.return_unit
    | Some x -> L.clear_keep_generation x
end
