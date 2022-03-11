(** The control file records various metadata: generation, version, last flushed offset;
    in particular, the generation is used to determine the current sparse+suffix.

    The generation is an integer that is incremented every time we switch to a
    new sparse+suffix. Essentially it is used as a pointer to the current
    version of the sparse and suffix. For example, if generation is [1234], then
    we can find the current sparse under path [sparse.1234], and similarly for
    the suffix.

    The other metadata does not affect the functioning of layers - we just need
    to be able to store some metadata fields as part of the [IO] interface, and
    the control file is one place to put it. *)

open Util

module Private = struct
  (* At the moment, users in irmin-pack always set the version explicitly anyway, so this
     [default_version] is only used on initial creation, before it is explicitly set by
     irmin-pack. It is only exposed if there is a crash before it is updated by
     irmin-pack. In this case, this [default_version] should match the one in irmin-pack,
     but no explicit check for this is made. *)
  let default_version = 2

  type t = Int_mmap.t

  type field = int
  (** Metadata fields are identifed by their index in the array; use the
      predefined fields, don't create your own *)

  let generation_field : field = 0
  let version_field : field = 1
  let last_synced_offset_field : field = 2
  let max_field = last_synced_offset_field
  let set t index v = t.Int_mmap.arr.{index} <- v
  let get t index = t.Int_mmap.arr.{index}

  let create ~root ~name =
    let ok = not (Sys.file_exists Fn.(root / name)) in
    assert ok;
    let t = Int_mmap.create ~fn:Fn.(root / name) ~sz:(max_field + 1) in
    set t version_field default_version;
    t

  let open_ ~root ~name =
    let ok = Sys.file_exists Fn.(root / name) in
    assert ok;
    let t = Int_mmap.open_ ~fn:Fn.(root / name) ~sz:(max_field + 1) in
    t

  let close t = Int_mmap.close t

  (* NOTE it is believed that under Linux this implies that the mmap is flushed as if via
     msync FIXME perhaps use msync instead *)
  let fsync (t : t) = Unix.fsync t.fd
  let get_generation t = get t generation_field

  let to_string c =
    Printf.sprintf "(generation: %d; version: %d; last_synced_offset: %d)"
      (get c generation_field) (get c version_field)
      (get c last_synced_offset_field)
end

include (
  Private :
    sig
      type t
      type field

      val generation_field : field
      val version_field : field
      val last_synced_offset_field : field
      val set : t -> field -> int -> unit
      val get : t -> field -> int
      val create : root:string -> name:string -> t
      val open_ : root:string -> name:string -> t
      val fsync : t -> unit
      val close : t -> unit

      val default_version: int 
      (** Exposed so that we can check it agrees with the default version in irmin-pack *)

      val get_generation : t -> int
      (** Convenience; just [get t generation] *)

      val to_string : t -> string
      (** A human-readable representation of the control, for debugging. *)
    end)
