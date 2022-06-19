(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
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
module Payload = Control_file.Latest_payload

let buffer_size = 8192
let buffer_size_63 = Int63.of_int 8192

module type Args = sig
  module Fm : File_manager.S
  module Dict : Dict.S with module Fm = Fm
  module Errs : Errors.S with module Io = Fm.Io

  type hash
  type key = hash Irmin_pack.Pack_key.t [@@deriving irmin]

  module Hash : sig
    val hash_size : int
  end

  module Node_value : sig
    type t
    type step

    val pred :
      t ->
      (step option * [ `Contents of key | `Inode of key | `Node of key ]) list
  end

  module Node_store : sig
    type 'a t

    val v : config:Irmin.Backend.Conf.t -> fm:Fm.t -> dict:Dict.t -> read t

    val unsafe_find :
      check_integrity:bool -> [< read ] t -> key -> Node_value.t option
  end

  module Commit_value : sig
    type t

    val node : t -> key
    val parents : t -> key list
  end

  module Commit_store :
    Pack_store.S
      with type value = Commit_value.t
       and type key = key
       and type file_manager = Fm.t
       and type dict = Dict.t
       and type hash = hash
end

module type S = sig
  module Args : Args

  val run : string -> Args.key -> (int, [> Args.Errs.t ]) result
end

module Make (Args : Args) : S with module Args := Args = struct
  open Args

  type abort_error =
    [ `Node_or_contents_key_is_indexed of string | `Dangling_key of string ]

  exception Abort_gc of abort_error

  module Io = Fm.Io

  type action = Follow | No_follow

  let string_of_key = Irmin.Type.to_string key_t

  let rec iter_from_node_key node_key node_store ~f k =
    match Node_store.unsafe_find ~check_integrity:false node_store node_key with
    | None -> raise (Abort_gc (`Dangling_key (string_of_key node_key)))
    | Some node ->
        iter_from_node_children node_store ~f (Node_value.pred node) k

  and iter_from_node_children node_store ~f children k =
    match children with
    | [] -> k ()
    | (_step, kinded_key) :: tl -> (
        let k () = iter_from_node_children node_store ~f tl k in
        match kinded_key with
        | `Contents key ->
            let (_ : action) = f key in
            k ()
        | `Inode key | `Node key -> (
            match f key with
            | No_follow -> k ()
            | Follow -> iter_from_node_key key node_store ~f k))

  (** [iter_from_commit_key commit_key _ _ ~f] calls [f] with:

      - [commit_key],
      - the key of the commit parents of [commit_key] and
      - the key of the node and commit entries in the tree of [commit_key].

      [f k] returns [Follow] or [No_follow], indicating the iteration algorithm
      if the children of [k] should be traversed or skiped.

      The parent(s) of [commit_key] must be included in the iteration because,
      when decoding the [Commit_value.t] at [commit_key], the parents will have
      to be read in order to produce a key for them. *)
  let iter_from_commit_key :
      Commit_store.key ->
      read Commit_store.t ->
      read Node_store.t ->
      f:(key -> action) ->
      unit =
   fun commit_key commit_store node_store ~f ->
    let (_ : action) = f commit_key in
    match
      Commit_store.unsafe_find ~check_integrity:false commit_store commit_key
    with
    | None -> raise (Abort_gc (`Dangling_key (string_of_key commit_key)))
    | Some commit ->
        List.iter
          (fun parent_commit_key ->
            let (_ : action) = f parent_commit_key in
            ())
          (Commit_value.parents commit);
        let node_key = Commit_value.node commit in
        let (_ : action) = f node_key in
        iter_from_node_key node_key node_store ~f (fun () -> ())

  let char = Pack_value.Kind.to_magic Pack_value.Kind.Gced

  let fill ~io ~count =
    let open Result_syntax in
    let buffer = String.make buffer_size char in
    let buffer_size = Int63.of_int buffer_size in
    let rec aux off count =
      let ( < ) a b = Int63.compare a b < 0 in
      let ( - ) = Int63.sub in
      let ( + ) = Int63.add in
      let ( === ) = Int63.equal in
      if count === Int63.zero then Ok ()
      else if count < buffer_size then
        let buffer = String.make (Int63.to_int count) char in
        Io.write_string io ~off buffer
      else
        let* () = Io.write_string io ~off buffer in
        let off = off + buffer_size in
        let count = count - buffer_size in
        aux off count
    in
    aux Int63.zero count

  let run root commit_key =
    let open Result_syntax in
    let config =
      Irmin_pack.Conf.init ~fresh:false ~readonly:true ~lru_size:0 root
    in

    (* Step 1. Open the files *)
    [%log.debug "GC: opening files in RO mode"];
    let* fm = Fm.open_ro config in
    let* dict = Dict.v fm in
    let node_store = Node_store.v ~config ~fm ~dict in
    let commit_store = Commit_store.v ~config ~fm ~dict in

    (* Step 2. Determine if the store allows GC *)
    let pl : Payload.t = Fm.Control.payload (Fm.control fm) in
    let* generation =
      match pl.status with
      | From_v1_v2_post_upgrade _ | From_v3_used_non_minimal_indexing_strategy
        ->
          Error `Gc_disallowed
      | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
      | T15 ->
          (* Unreachable *)
          assert false
      | From_v3_no_gc_yet -> Ok 1
      | From_v3_gced x -> Ok (x.generation + 1)
    in
    [%log.debug "GC: next generation will be %d" generation];

    (* Step 3. Make [commit_key] [Direct] *)
    let* commit_key =
      let state : _ Irmin_pack.Pack_key.state =
        Irmin_pack.Pack_key.inspect commit_key
      in
      match state with
      | Direct _ -> Ok commit_key
      | Indexed h -> (
          match Commit_store.index_direct_with_kind commit_store h with
          | None ->
              Error
                (`Commit_key_is_indexed_and_dangling (string_of_key commit_key))
          | Some (k, _kind) -> Ok k)
    in
    let commit_offset, commit_len =
      let state : _ Irmin_pack.Pack_key.state =
        Irmin_pack.Pack_key.inspect commit_key
      in
      match state with
      | Indexed _ -> assert false
      | Direct x -> (x.offset, x.length)
    in

    (* Step 4. Create the new suffix and prepare 2 functions for read and write
       operations. *)
    let suffix_path = Irmin_pack.Layout.V3.suffix ~root ~generation in
    [%log.debug "GC: creating %S" suffix_path];
    let* dst_io = Io.create ~path:suffix_path ~overwrite:false in
    let src_ao = Fm.suffix fm in
    let buffer = Bytes.create buffer_size in
    let hash_size = Int63.of_int Hash.hash_size in
    let already_copied_exn off =
      (* Read the [kind] byte, which lies just after the hash *)
      let ( + ) = Int63.add in
      Io.read_exn dst_io ~off:(off + hash_size) ~len:1 buffer;
      Bytes.get buffer 0 <> char
    in
    let rec transfer_exn (off : int63) (len_remaining : int63) =
      let ( + ) = Int63.add in
      let ( - ) = Int63.sub in
      let ( < ) a b = Int63.compare a b < 0 in
      let ( > ) a b = Int63.compare a b > 0 in
      let min a b = if a < b then a else b in
      let len : int63 = min buffer_size_63 len_remaining in
      let len' = Int63.to_int len in
      Fm.Suffix.read_exn src_ao ~off ~len:len' buffer;
      Io.write_exn dst_io ~off ~len:len' (Bytes.unsafe_to_string buffer);
      let len_remaining : int63 = len_remaining - len in
      if len_remaining > Int63.zero then transfer_exn (off + len) len_remaining
    in

    (* Step 5. *)
    [%log.debug "GC: transfering to the right side"];
    let right_size =
      let total_size = pl.entry_offset_suffix_end in
      let ( - ) = Int63.sub in
      let ( >= ) a b = Int63.compare a b >= 0 in
      let x = total_size - commit_offset in
      assert (x >= Int63.of_int commit_len);
      x
    in
    let* () = Errs.catch (fun () -> transfer_exn commit_offset right_size) in

    (* Step 6. *)
    [%log.debug "GC: filling the left side"];
    let* () = fill ~io:dst_io ~count:commit_offset in

    (* Step 7. *)
    [%log.debug "GC: transfering to the left side"];
    let f key =
      let state : _ Irmin_pack.Pack_key.state =
        Irmin_pack.Pack_key.inspect key
      in
      match state with
      | Indexed _ ->
          raise
            (Abort_gc (`Node_or_contents_key_is_indexed (string_of_key key)))
      | Direct { offset; length; _ } ->
          if already_copied_exn offset then No_follow
          else (
            transfer_exn offset (Int63.of_int length);
            Follow)
    in
    let* () =
      try
        Errs.catch (fun () ->
            iter_from_commit_key commit_key commit_store node_store ~f)
      with Abort_gc (#abort_error as err) -> Error err
    in

    (* Step 8. Close everything *)
    [%log.debug "GC: closing files"];
    let* () = Io.close dst_io in
    let* () = Fm.close fm in

    (* Step 9. Inform the caller of the new generation *)
    Ok generation
end
