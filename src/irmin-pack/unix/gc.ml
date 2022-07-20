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

exception Pack_error = Errors.Pack_error

module type Args = sig
  (* The following [with module Io = Io.Unix] forces unix *)
  module Fm : File_manager.S with module Io = Io.Unix
  module Dict : Dict.S with module Fm = Fm
  module Errs : Io_errors.S with module Io = Fm.Io
  module Dispatcher : Dispatcher.S with module Fm = Fm

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

    val v :
      config:Irmin.Backend.Conf.t ->
      fm:Fm.t ->
      dict:Dict.t ->
      dispatcher:Dispatcher.t ->
      read t

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
       and type dispatcher = Dispatcher.t
       and type hash = hash
end

module type S = sig
  module Args : Args

  val run_and_output_result : generation:int -> string -> Args.key -> int63

  val transfer_append_exn :
    read_exn:(off:int63 -> len:int -> bytes -> unit) ->
    append_exn:(string -> unit) ->
    off:int63 ->
    len:int63 ->
    bytes ->
    unit
end

module Make (Args : Args) : S with module Args := Args = struct
  open Args
  module Io = Fm.Io
  module Mapping_file = Mapping_file.Make (Errs)
  module Ao = Append_only_file.Make (Io)

  module X = struct
    type t = int63 [@@deriving irmin]

    let equal = Irmin.Type.(unstage (equal t))
    let hash = Irmin.Type.(unstage (short_hash t))
    let hash (t : t) : int = hash t
  end

  module Table = Hashtbl.Make (X)

  let string_of_key = Irmin.Type.to_string key_t

  let transfer_append_exn ~read_exn ~append_exn ~(off : int63) ~(len : int63)
      buffer =
    let buffer_size = Bytes.length buffer |> Int63.of_int in
    let rec aux off len_remaining =
      let open Int63.Syntax in
      let min a b = if a < b then a else b in
      let len = min buffer_size len_remaining in
      let len' = Int63.to_int len in
      read_exn ~off ~len:len' buffer;
      let () =
        if len = buffer_size then append_exn (Bytes.to_string buffer)
        else append_exn (String.sub (Bytes.to_string buffer) 0 len')
      in
      let len_remaining = len_remaining - len in
      if len_remaining > Int63.zero then aux (off + len) len_remaining
    in
    aux off len

  (** [iter_from_node_key node_key _ _ ~f] calls [f] with the key of the node
      and iterates over its children.

      [f k] returns [Follow] or [No_follow], indicating the iteration algorithm
      if the children of [k] should be traversed or skiped. *)
  let iter node_key node_store ~f k =
    let marks = Table.create 1024 in
    let mark offset = Table.add marks offset () in
    let has_mark offset = Table.mem marks offset in
    let rec iter_from_node_key_exn node_key node_store ~f k =
      match
        Node_store.unsafe_find ~check_integrity:false node_store node_key
      with
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
              let (_ : int63) = f key in
              k ()
          | `Inode key | `Node key ->
              let offset = f key in
              if has_mark offset then k ()
              else (
                mark offset;
                iter_from_node_key_exn key node_store ~f k))
    in
    iter_from_node_key_exn node_key node_store ~f k

  (* Dangling_parent_commit are the parents of the gced commit. They are kept on
     disk in order to correctly deserialised the gced commit. *)
  let magic_parent =
    Pack_value.Kind.to_magic Pack_value.Kind.Dangling_parent_commit

  (* Transfer the commit with a different magic. Note that this is modifying
     existing written data. *)
  let transfer_parent_commit_exn ~read_exn ~write_exn ~mapping key =
    let off, len =
      match Irmin_pack.Pack_key.inspect key with
      | Indexed _ ->
          (* As this is the second time we are reading this key, this case is
             unreachable. *)
          assert false
      | Direct { offset; length; _ } -> (offset, length)
    in
    let buffer = Bytes.create len in
    read_exn ~off ~len buffer;
    let poff = Dispatcher.poff_of_entry_exn ~off ~len mapping in
    Bytes.set buffer Hash.hash_size magic_parent;
    (* Bytes.unsafe_to_string usage: We assume read_exn returns unique ownership of buffer
       to this function. Then at the call to Bytes.unsafe_to_string we give up unique
       ownership to buffer (we do not modify it thereafter) in return for ownership of the
       resulting string, which we pass to write_exn. This usage is safe. *)
    write_exn ~off:poff ~len (Bytes.unsafe_to_string buffer)

  let create_new_suffix ~root ~generation =
    let open Result_syntax in
    let path = Irmin_pack.Layout.V3.suffix ~root ~generation in
    let auto_flush_threshold = 1_000_000 in
    let suffix_ref = ref None in
    let auto_flush_callback () =
      match !suffix_ref with
      | None -> assert false
      | Some x -> Ao.flush x |> Errs.raise_if_error
    in
    let* suffix =
      Ao.create_rw ~path ~overwrite:true ~auto_flush_threshold
        ~auto_flush_callback
    in
    suffix_ref := Some suffix;
    Ok suffix

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
    let* dispatcher = Dispatcher.v ~root fm in
    let node_store = Node_store.v ~config ~fm ~dict ~dispatcher in
    let commit_store = Commit_store.v ~config ~fm ~dict ~dispatcher in

    (* Step 2. Load commit which will make [commit_key] [Direct] if it's not
       already the case. *)
    let* commit =
      match
        Commit_store.unsafe_find ~check_integrity:false commit_store commit_key
      with
      | None -> Error (`Commit_key_is_dangling (string_of_key commit_key))
      | Some commit -> Ok commit
    in
    let commit_offset, commit_len =
      let state : _ Irmin_pack.Pack_key.state =
        Irmin_pack.Pack_key.inspect commit_key
      in
      match state with
      | Indexed _ -> assert false
      | Direct x -> (x.offset, x.length)
    in

    (* Step 3. Create the new mapping. *)
    let* () =
      (* Step 3.1 Start [Mapping_file] routine which will create the
         reachable file. *)
      (fun f -> Mapping_file.create ~root ~generation ~register_entries:f)
      @@ fun ~register_entry ->
      (* Step 3.2 Put the commit parents in the reachable file.
         The parent(s) of [commit_key] must be included in the iteration
         because, when decoding the [Commit_value.t] at [commit_key], the
         parents will have to be read in order to produce a key for them. *)
      let register_object_exn key =
        match Irmin_pack.Pack_key.inspect key with
        | Indexed _ ->
            raise
              (Pack_error (`Commit_parent_key_is_indexed (string_of_key key)))
        | Direct { offset; length; _ } -> register_entry ~off:offset ~len:length
      in
      List.iter register_object_exn (Commit_value.parents commit);

      (* Step 3.3 Put the nodes and contents in the reachable file. *)
      let register_object_exn key =
        match Irmin_pack.Pack_key.inspect key with
        | Indexed _ ->
            raise
              (Pack_error (`Node_or_contents_key_is_indexed (string_of_key key)))
        | Direct { offset; length; _ } ->
            register_entry ~off:offset ~len:length;
            offset
      in
      let node_key = Commit_value.node commit in
      let (_ : int63) = register_object_exn node_key in
      iter node_key node_store ~f:register_object_exn (fun () -> ());

      (* Step 3.4 Return and let the [Mapping_file] routine create the mapping
         file. *)
      ()
    in

    let mapping_path = Irmin_pack.Layout.V3.mapping ~root ~generation in
    let* mapping = Mapping_file.load_mapping_as_mmap mapping_path in
    let* () =
      (* Step 4. Create the new prefix. *)
      let prefix_ref = ref None in
      let auto_flush_callback () =
        match !prefix_ref with
        | None -> assert false
        | Some x -> Ao.flush x |> Errs.raise_if_error
      in
      let* prefix =
        let path = Irmin_pack.Layout.V3.prefix ~root ~generation in
        Ao.create_rw ~path ~overwrite:true ~auto_flush_threshold:1_000_000
          ~auto_flush_callback
      in
      prefix_ref := Some prefix;
      let* () =
        Errors.finalise (fun _outcome ->
            Ao.close prefix |> Errs.log_if_error "GC: Close prefix")
        @@ fun () ->
        ();

        (* Step 5. Transfer to the new prefix, flush and close. *)
        [%log.debug "GC: transfering to the new prefix"];
        let buffer = Bytes.create buffer_size in
        (* Step 5.1. Transfer all. *)
        let read_exn = Dispatcher.read_in_prefix_and_suffix_exn dispatcher in
        let append_exn = Ao.append_exn prefix in
        let f ~off ~len =
          let len = Int63.of_int len in
          transfer_append_exn ~read_exn ~append_exn ~off ~len buffer
        in
        let* () = Mapping_file.iter_mmap mapping f in
        Ao.flush prefix
      in
      (* Step 5.2. Transfer again the parent commits but with a modified
         magic. Load the mapping in memory to do a safe localisation of the
         parent commits. Reopen the new prefix, this time _not_ in append-only
         as we have to modify data inside the file. *)
      let* in_memory_map = Dispatcher.load_mapping mapping_path in
      let read_exn = Dispatcher.read_exn dispatcher in
      let* prefix =
        let path = Irmin_pack.Layout.V3.prefix ~root ~generation in
        Io.open_ ~path ~readonly:false
      in
      let* () =
        Errors.finalise (fun _outcome ->
            Io.close prefix
            |> Errs.log_if_error "GC: Close prefix after parent rewrite")
        @@ fun () ->
        let write_exn = Io.write_exn prefix in
        List.iter
          (fun key ->
            transfer_parent_commit_exn ~read_exn ~write_exn
              ~mapping:in_memory_map key)
          (Commit_value.parents commit);
        Ok ()
      in
      Ok ()
    in

    (* Step 6. Create the new suffix and prepare 2 functions for read and write
       operations. *)
    let buffer = Bytes.create buffer_size in
    [%log.debug "GC: creating new suffix"];
    let* suffix = create_new_suffix ~root ~generation in
    Errors.finalise (fun _outcome ->
        Ao.close suffix |> Errs.log_if_error "GC: Close suffix")
    @@ fun () ->
    let read_exn = Dispatcher.read_exn dispatcher in
    let append_exn = Ao.append_exn suffix in
    let transfer_exn = transfer_append_exn ~read_exn ~append_exn buffer in

    (* Step 7. Transfer to the next suffix. *)
    [%log.debug "GC: transfering to the new suffix"];
    let* () = Fm.reload fm in
    let pl : Payload.t = Fm.Control.payload (Fm.control fm) in
    let end_offset =
      Dispatcher.offset_of_suffix_off dispatcher pl.entry_offset_suffix_end
    in
    let right_size =
      let open Int63.Syntax in
      let x = end_offset - commit_offset in
      assert (x >= Int63.of_int commit_len);
      x
    in
    let flush_and_raise () = Ao.flush suffix |> Errs.raise_if_error in
    let* () =
      Errs.catch (fun () ->
          transfer_exn ~off:commit_offset ~len:right_size;
          flush_and_raise ())
    in
    (* Step 8. Inform the caller of the end_offset copied. *)
    Ok end_offset

  (* No one catches errors when this function terminates. Write the result in a
     file and terminate the process with an exception, if needed. *)
  let run_and_output_result ~generation root commit_key =
    let result = run ~generation root commit_key in
    let write_result = Fm.write_gc_output ~root ~generation result in
    write_result |> Errs.raise_if_error;
    result |> Errs.raise_if_error
end
