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

exception Pack_error = Errors_base.Pack_error

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

  val run_and_output_result : generation:int -> string -> Args.key -> int63

  val transfer_exn :
    src:Args.Fm.Suffix.t ->
    dst:Args.Fm.Io.t ->
    off:int63 ->
    len:int63 ->
    bytes ->
    unit
end

module Make (Args : Args) : S with module Args := Args = struct
  open Args
  module Io = Fm.Io

  type action = Follow | No_follow

  let string_of_key = Irmin.Type.to_string key_t

  let transfer_exn ~src ~dst ~(off : int63) ~(len : int63) buffer =
    let buffer_size = Bytes.length buffer |> Int63.of_int in
    let rec aux off len_remaining =
      let open Int63.Syntax in
      let min a b = if a < b then a else b in
      let len = min buffer_size len_remaining in
      let len' = Int63.to_int len in
      Fm.Suffix.read_exn src ~off ~len:len' buffer;
      Io.write_exn dst ~off ~len:len' (Bytes.unsafe_to_string buffer);
      let len_remaining = len_remaining - len in
      if len_remaining > Int63.zero then aux (off + len) len_remaining
    in
    aux off len

  (** [iter_from_node_key node_key _ _ ~f] calls [f] with the key of the node
      and iterates over its children.

      [f k] returns [Follow] or [No_follow], indicating the iteration algorithm
      if the children of [k] should be traversed or skiped. *)
  let rec iter_from_node_key_exn node_key node_store ~f k =
    match Node_store.unsafe_find ~check_integrity:false node_store node_key with
    | None -> raise (Pack_error (`Dangling_key (string_of_key node_key)))
    | Some node ->
        iter_from_node_children_exn node_store ~f (Node_value.pred node) k

  and iter_from_node_children_exn node_store ~f children k =
    match children with
    | [] -> k ()
    | (_step, kinded_key) :: tl -> (
        let k () = iter_from_node_children_exn node_store ~f tl k in
        match kinded_key with
        | `Contents key ->
            let (_ : action) = f key in
            k ()
        | `Inode key | `Node key -> (
            match f key with
            | No_follow -> k ()
            | Follow -> iter_from_node_key_exn key node_store ~f k))

  let magic_gced = Pack_value.Kind.to_magic Pack_value.Kind.Gced

  (* Dangling_parent_commit are the parents of the gced commit. They are kept on
     disk in order to correctly deserialised the gced commit. *)
  let magic_parent =
    Pack_value.Kind.to_magic Pack_value.Kind.Dangling_parent_commit

  (* Transfer the commit with a different magic. Note that this is modifying
     existing written data. *)
  let transfer_parent_commit ~src ~dst key =
    let open Result_syntax in
    let* off, len =
      match Irmin_pack.Pack_key.inspect key with
      | Indexed _ -> Error (`Commit_parent_key_is_indexed (string_of_key key))
      | Direct { offset; length; _ } -> Ok (offset, length)
    in
    let* s = Fm.Suffix.read_to_string src ~off ~len in
    let s = Bytes.of_string s in
    Bytes.set s Hash.hash_size magic_parent;
    Io.write_string dst ~off (Bytes.unsafe_to_string s)

  let fill ~io ~count =
    let open Result_syntax in
    let buffer = String.make buffer_size magic_gced in
    let buffer_size = Int63.of_int buffer_size in
    let rec aux off count =
      let open Int63.Syntax in
      if count = Int63.zero then Ok ()
      else if count < buffer_size then
        let buffer = String.make (Int63.to_int count) magic_gced in
        Io.write_string io ~off buffer
      else
        let* () = Io.write_string io ~off buffer in
        let off = off + buffer_size in
        let count = count - buffer_size in
        aux off count
    in
    aux Int63.zero count

  let run ~generation root commit_key =
    let open Result_syntax in
    let config =
      Irmin_pack.Conf.init ~fresh:false ~readonly:true ~lru_size:0 root
    in

    (* Step 1. Open the files *)
    [%log.debug "GC: opening files in RO mode"];
    let* fm = Fm.open_ro config in
    Errors.finalise (fun _outcome ->
        Fm.close fm |> Errs.log_if_error "GC: Close File_manager")
    @@ fun () ->
    let* dict = Dict.v fm in
    let node_store = Node_store.v ~config ~fm ~dict in
    let commit_store = Commit_store.v ~config ~fm ~dict in

    (* Step 2. Make [commit_key] [Direct] *)
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

    (* Step 3. Create the new suffix and prepare 2 functions for read and write
       operations. *)
    let suffix_path = Irmin_pack.Layout.V3.suffix ~root ~generation in
    [%log.debug "GC: creating %S" suffix_path];
    let* dst_io = Io.create ~path:suffix_path ~overwrite:true in
    Errors.finalise (fun _outcome ->
        Io.close dst_io |> Errs.log_if_error "GC: Close suffix")
    @@ fun () ->
    let src_ao = Fm.suffix fm in
    let buffer = Bytes.create buffer_size in
    let hash_size = Int63.of_int Hash.hash_size in
    let already_copied_exn off =
      (* Read the [kind] byte, which lies just after the hash *)
      let open Int63.Syntax in
      Io.read_exn dst_io ~off:(off + hash_size) ~len:1 buffer;
      Bytes.get buffer 0 <> magic_gced
    in
    let transfer_exn = transfer_exn ~src:src_ao ~dst:dst_io buffer in

    (* Step 4. *)
    [%log.debug "GC: filling the left side"];
    let* () = fill ~io:dst_io ~count:commit_offset in

    (* Step 5. Transfer the parents of [commit_key].

       The parent(s) of [commit_key] must be included in the iteration because,
       when decoding the [Commit_value.t] at [commit_key], the parents will have
       to be read in order to produce a key for them.

       There is no need to transfer [commit_key] itself because it is in
       right. *)
    [%log.debug "GC: transfering commit parent(s) to the left side"];
    let* commit =
      match
        Commit_store.unsafe_find ~check_integrity:false commit_store commit_key
      with
      | None -> Error (`Dangling_key (string_of_key commit_key))
      | Some commit -> Ok commit
    in
    let* () =
      List.fold_left
        (fun result parent_commit_key ->
          let* () = result in
          transfer_parent_commit ~src:src_ao ~dst:dst_io parent_commit_key)
        (Ok ())
        (Commit_value.parents commit)
    in

    (* Step 6. Transfer the nodes and blobs. *)
    [%log.debug "GC: transfering nodes and contents to the left side"];
    let transfer_object_exn key =
      let offset, length =
        match Irmin_pack.Pack_key.inspect key with
        | Indexed _ ->
            raise
              (Pack_error (`Node_or_contents_key_is_indexed (string_of_key key)))
        | Direct { offset; length; _ } -> (offset, length)
      in
      if already_copied_exn offset then No_follow
      else (
        transfer_exn ~off:offset ~len:(Int63.of_int length);
        Follow)
    in
    let node_key = Commit_value.node commit in
    let* (_ : action) = Errs.catch (fun () -> transfer_object_exn node_key) in
    let* () =
      Errs.catch (fun () ->
          iter_from_node_key_exn node_key node_store ~f:transfer_object_exn
            (fun () -> ()))
    in

    (* Step 7. *)
    [%log.debug "GC: transfering to the right side"];
    let* () = Fm.reload fm in
    let pl : Payload.t = Fm.Control.payload (Fm.control fm) in
    let end_offset = pl.entry_offset_suffix_end in
    let right_size =
      let open Int63.Syntax in
      let x = end_offset - commit_offset in
      assert (x >= Int63.of_int commit_len);
      x
    in
    let* () =
      Errs.catch (fun () -> transfer_exn ~off:commit_offset ~len:right_size)
    in

    (* Step 9. Inform the caller of the end_offset copied. *)
    Ok end_offset

  (* No one catches errors when this function terminates. Write the result in a
     file and terminate the process with an exception, if needed. *)
  let run_and_output_result ~generation root commit_key =
    let result = run ~generation root commit_key in
    Fm.write_gc_output ~root ~generation result |> Errs.raise_if_error;
    result |> Errs.raise_if_error
end
