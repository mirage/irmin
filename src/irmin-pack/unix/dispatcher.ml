open Import

module type S = sig
  module Fm : File_manager.S

  type t

  val v : Fm.t -> (t, [> Fm.Errs.t ]) result

  val read_exn : t -> off:int63 -> len:int -> bytes -> unit
  (** [dispatched_read_exn] either reads in the prefix or the suffix file,
      depending on [off]. See [Io.read_exn] for the arguments. *)

  val end_offset : t -> int63
  (** [end_offset] is the end offsets of the pack entries, counting that the
      prefix doesn't start at 0. It counts the entries not yet flushed from the
      prefix. *)
end

module Make (Fm : File_manager.S) : S with module Fm = Fm = struct
  module Fm = Fm
  module Suffix = Fm.Suffix

  type t = { fm : Fm.t }

  let v fm = Ok { fm }

  (* let entry_offset_suffix_start t =
   *   let pl = Control.payload t.control in
   *   match pl.status with
   *   | Payload.From_v1_v2_post_upgrade _
   *   | From_v3_used_non_minimal_indexing_strategy | From_v3_no_gc_yet ->
   *       Int63.zero
   *   | T1 | T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | T11 | T12 | T13 | T14
   *   | T15 ->
   *       assert false
   *   | From_v3_gced { entry_offset_suffix_start; _ } -> entry_offset_suffix_start *)

  let entry_offset_suffix_start _ = Int63.zero

  let read_exn t ~off ~len buf =
    let open Int63.Syntax in
    let entry_offset_suffix_start = entry_offset_suffix_start t in
    if off >= entry_offset_suffix_start then
      Suffix.read_exn (Fm.suffix t.fm) ~off ~len buf
    else
      (* TODO: read in prefix *)
      Fmt.failwith "unexpected read %a" Int63.pp off

  let end_offset t =
    let open Int63.Syntax in
    Suffix.end_offset (Fm.suffix t.fm) + entry_offset_suffix_start t
end
