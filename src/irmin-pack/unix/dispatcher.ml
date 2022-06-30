open Import
module Payload = Control_file.Latest_payload

module type S = sig
  module Fm : File_manager.S

  type t

  val v : root:string -> Fm.t -> (t, [> Fm.Errs.t ]) result

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** [read_exn] either reads in the prefix or the suffix file, depending on
      [off]. See [Io.read_exn] for the arguments. *)

  val read_at_most_exn : t -> off:int63 -> len:int -> bytes -> int

  val end_offset : t -> int63
  (** [end_offset] is the end offsets of the pack entries, counting that the
      prefix doesn't start at 0. It counts the entries not yet flushed from the
      prefix. *)
end

module Intmap = Map.Make (Int63)

(* The following [with module Io = Io.Unix] forces unix *)
module Make (Fm : File_manager.S with module Io = Io.Unix) :
  S with module Fm = Fm = struct
  module Fm = Fm
  module Io = Fm.Io
  module Suffix = Fm.Suffix
  module Mapping_file = Mapping_file.Make (Fm.Errs)
  module Errs = Fm.Errs
  module Control = Fm.Control

  type mapping_value = { poff : int63; len : int }
  (** [poff] is a prefix offset (i.e. an offset in the prefix file *)

  let a = ref 0
  let b = ref 0
  let c = ref 0

  let print w =
    Fmt.epr "\n%s: pid:%d %#d %#d %#d \n%!" w (Unix.getpid ()) !a !b !c

  let () = at_exit (fun () -> print "at_exit")

  type t = {
    fm : Fm.t;
    mutable mapping : mapping_value Intmap.t;
    root : string;
  }
  (** [mapping] is a map from global offset to (offset,len) pairs in the prefix
      file *)

  let reload t =
    print "reload";
    let open Result_syntax in
    let* mapping =
      match Fm.mapping t.fm with
      | None -> Ok Intmap.empty
      | Some io ->
          let mapping = ref Intmap.empty in
          let open Int63 in
          let open Int63.Syntax in
          let poff = ref zero in
          let f ~off ~len =
            (* Fmt.epr "chunk: %d %d\n%!" (to_int off) len; *)
            mapping := Intmap.add off { poff = !poff; len } !mapping;
            poff := !poff + of_int len
          in
          let* () = Mapping_file.iter io f in
          Ok !mapping
    in
    Fmt.epr "mapping cardinal %d\n%!" (Intmap.cardinal mapping);
    t.mapping <- mapping;
    Ok ()

  let v ~root fm =
    let open Result_syntax in
    let t = { fm; mapping = Intmap.empty; root } in
    Fm.register_mapping_consumer fm ~after_reload:(fun () -> reload t);
    let* () = reload t in
    Ok t

  let entry_offset_suffix_start t =
    let pl = Control.payload (Fm.control t.fm) in
    match pl.status with
    | Payload.From_v1_v2_post_upgrade _
    | From_v3_used_non_minimal_indexing_strategy | From_v3_no_gc_yet ->
        Int63.zero
    | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
    | T15 ->
        assert false
    | From_v3_gced { entry_offset_suffix_start; _ } -> entry_offset_suffix_start

  let end_offset t = Suffix.end_offset (Fm.suffix t.fm)
  (* TODO: Swap to right formula *)
  (* let open Int63.Syntax in *)
  (* Suffix.end_offset (Fm.suffix t.fm) + entry_offset_suffix_start t *)

  let chunk_of_off_exn { mapping; _ } off_start =
    let open Int63 in
    let open Int63.Syntax in
    match
      (* Looking for the last chunk which is before [off] (or at [off]). *)
      Intmap.find_last_opt
        (fun chunk_off_start -> chunk_off_start <= off_start)
        mapping
    with
    | None ->
        (* Case 1: The entry if before the very first chunk (or there are no
           chunks) *)
        assert false (* TODO *)
    | Some (chunk_off_start, chunk) ->
        assert (chunk_off_start <= off_start);
        let chunk_len = chunk.len in
        let chunk_off_end = chunk_off_start + of_int chunk_len in

        (* Case 2: The entry starts after the chunk *)
        if chunk_off_end <= off_start then assert false (* TODO *);

        let shift_in_chunk = off_start - chunk_off_start in
        let max_entry_len = of_int chunk_len - shift_in_chunk in

        (chunk, shift_in_chunk, max_entry_len)

  let poff_of_entry_exn t off len =
    let chunk, shift_in_chunk, max_entry_len = chunk_of_off_exn t off in

    (* Case 3: The entry ends after the chunk *)
    let open Int63 in
    let open Int63.Syntax in
    if of_int len > max_entry_len then assert false (* TODO *);

    (* Case 4: Success *)
    chunk.poff + shift_in_chunk

  let read_exn t ~off ~len buf =
    let open Int63.Syntax in
    let entry_offset_suffix_start = entry_offset_suffix_start t in
    if off >= entry_offset_suffix_start then (
      incr a;
      (* Fmt.epr "read suff\n%!"; *)
      try Suffix.read_exn (Fm.suffix t.fm) ~off ~len buf
      with e ->
        let to_int = Int63.to_int in
        Fmt.epr "\n%!";
        Fmt.epr "exception!\n%!";
        Fmt.epr "%#d %#d %#d %#d\n%!" (to_int off) len
          (to_int entry_offset_suffix_start)
          (to_int @@ end_offset t);
        Fmt.epr "\n%!";
        raise e)
    else (
      incr b;
      (* Fmt.epr "read off\n%!"; *)
      let poff = poff_of_entry_exn t off len in
      (* TODO: Change that optionget *)
      Io.read_exn (Fm.prefix t.fm |> Option.get) ~off:poff ~len buf;
      ())

  let read_at_most_from_suffix_exn t ~off ~len buf =
    let bytes_after_off = Int63.sub (end_offset t) off in
    let len =
      let open Int63.Syntax in
      if bytes_after_off < Int63.of_int len then Int63.to_int bytes_after_off
      else len
    in
    Suffix.read_exn (Fm.suffix t.fm) ~off ~len buf;
    len

  let read_at_most_from_prefix_exn t ~off ~len buf =
    let chunk, shift_in_chunk, max_entry_len = chunk_of_off_exn t off in
    let fm = t.fm in

    let open Int63 in
    let open Int63.Syntax in
    let min a b = if a < b then a else b in
    let len = min max_entry_len (of_int len) |> to_int in
    let poff = chunk.poff + shift_in_chunk in
    (* TODO: Change that optionget *)
    Io.read_exn (Fm.prefix fm |> Option.get) ~off:poff ~len buf;
    len

  let read_at_most_exn t ~off ~len buf =
    let open Int63.Syntax in
    let entry_offset_suffix_start = entry_offset_suffix_start t in
    if off >= entry_offset_suffix_start then
      read_at_most_from_suffix_exn t ~off ~len buf
    else read_at_most_from_prefix_exn t ~off ~len buf
end
