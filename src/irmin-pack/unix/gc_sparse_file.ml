(* NOTE if we want to use this with the new IO, we can eliminate the load/save mmap usage
   and just write ints to disk via new IO; we also need to adjust the v3 layout to include
   the sparse file map and data files; then presumably we need to add file read/write
   functions (in file manager?) which determines whether to read/write in the sparse or
   the suffix (based on the suffix offset) *)

(** A sparse file is "a file containing gaps"; the gaps do not take up space on
    disk.

    The implementation uses a normal "data" file for the non-gap data, and a
    "map" file, which is used to translate "addresses in the sparse file" (aka
    "virtual addresses") to "addresses in the data file".

    The prototype, which includes extensive documentation, can be found at
    {:https://github.com/tomjridge/sparse-file/}. *)

open Import
open Gc_util

include struct
  module Io = Io.Unix
  module Append_only_file = Append_only_file.Make (Io)
  module Aof = Append_only_file
end

(** Signature for sparse files; file-like with [append_region] and [pread] *)
module type S = sig
  type t
  (** The type of sparse files. *)

  val create : path:string -> t
  (** [create ~path] creates an empty sparse file; requires [path] is
      non-existent; creates directory [path] and files [path/data] to hold the
      data and [path/map] to hold the mapping from virtual offset to (real
      offset and length) *)

  val open_ro : dir:string -> t
  (** [open_ro ~dir] opens the sparse file located in [dir] *)

  val close : t -> unit
  (** [close t] saves the sparse map and closes the underlying file descriptor
      for the sparse data file *)

  val append_region :
    t -> src:Pread.t -> src_off:int -> len:int -> virt_off:int -> unit
  (** [append_region t ~src ~src_off ~len ~virt_off] appends len bytes from
      [src] at offset [src_off], to the end of sparse file [t].

      A new map entry [virt_off -> (real_off,len)] is added, where [real_off] is
      the offset of the end of the sparse data file before the append took
      place.

      NOTE [len] must not be 0; no other validity checks are made on [off] and
      [len]. It is possible to add the same region multiple times for example,
      or add overlapping regions, etc. {b These usages should be avoided.} *)

  val pread : t -> off:int ref -> len:int -> buf:bytes -> int
  (** [pread t ~off ~len ~buf] reads [len] bytes from virtual offset [off] into
      [buf], updating [off] and returning the number of bytes read.

      A read that extends beyond the end of a data region will be supplemented
      with dummy bytes (this scenario is allowed with irmin-pack because often
      the user does not know how many bytes to read, so they read more than is
      strictly necessary and then examine the bytes they have read to try to
      decode an object).

      A read that starts in an empty region will fill the buffer with dummy
      bytes (this scenario is almost certainly an error case, so we could
      instead throw an exception). *)
end

(** Private implementation of the "map" file *)
module Private_map : sig
  type map = (int * int) Int_map.t

  val load : string -> map
  val save : map -> string -> unit

  (* real errors that can arise from save *)
  type save_error = [ `Io_misc of Io.misc_error ]

  val save' : map -> string -> (unit, save_error) result

  (* FIXME or prefer this?
  val save' : map -> string -> (unit, [> save_error ]) result
  *)

  type load_error =
    [ `Double_close
    | `Invalid_argument
    | `Io_misc of Io.misc_error
    | `No_such_file_or_directory
    | `Not_a_file
    | `Read_on_closed
    | `Read_out_of_bounds ]

  val load' : string -> (map, load_error) result
end = struct
  type map = (int * int) Int_map.t

  open Int_map

  let load fn =
    let sz_bytes = File.size fn in
    let ok = sz_bytes mod (3 * 8) = 0 in
    let _check_ints_size =
      if not ok then
        failwith
          (Printf.sprintf "%s: file %s did not contain 3*n ints" __FILE__ fn)
    in
    assert ok;
    let sz = sz_bytes / 8 in
    let mmap = Int_mmap.open_ ~fn ~sz:(sz_bytes / 8) in
    let arr = mmap.arr in
    let m =
      (0, empty)
      |> iter_k (fun ~k (i, m) ->
             match i + 2 < sz with
             | false -> m
             | true ->
                 let voff, off, len = (arr.{i}, arr.{i + 1}, arr.{i + 2}) in
                 let m = add voff (off, len) m in
                 k (i + 3, m))
    in
    (* remember to close the mmap *)
    Int_mmap.close mmap;
    m

  let save t fn =
    let sz_keys = cardinal t in
    let dir = Filename.dirname fn in
    let tmp = Filename.temp_file ~temp_dir:dir "sparse." ".map" in
    let mmap = Int_mmap.open_ ~fn:tmp ~sz:(sz_keys * 3) in
    let arr = mmap.arr in
    let i = ref 0 in
    t
    |> iter (fun voff (roff, len) ->
           arr.{!i} <- voff;
           arr.{!i + 1} <- roff;
           arr.{!i + 2} <- len;
           i := !i + 3);
    Int_mmap.close mmap;
    Unix.rename tmp fn;
    ()

  type save_error' =
    [ `Double_close
    | `File_exists of string
    | `Io_misc of Io.misc_error
    | `Pending_flush
    | `Ro_not_allowed
    | `Write_on_closed ]

  (* real errors that can arise from save *)
  type save_error = [ `Io_misc of Io.misc_error ]

  let megabyte = 1024 * 1024

  (* following version uses the new IO *)
  let save' t fn : (unit, save_error') result =
    (* we used to write to a temp file and rename, but the new IO doesn't provide the
       temp_file function; so we just write the file directly *)
    let open Result_syntax in
    let* f =
      Aof.create_rw ~path:fn ~overwrite:true ~auto_flush_threshold:(4 * megabyte)
        ~auto_flush_callback:(fun () -> ())
    in
    let buf = Bytes.create 24 in
    t
    |> iter (fun voff (roff, len) ->
           (* since we don't use mmaps for the sparse file map, we can chose to use big
              endian, little endian or native endian; if we pick either big endian or little
              endian, the stores can be used across architectures; so we choose little endian
              (since that is default on x86) *)
           Bytes.set_int64_le buf 0 (Int64.of_int voff);
           Bytes.set_int64_le buf 8 (Int64.of_int roff);
           Bytes.set_int64_le buf 16 (Int64.of_int len);
           Aof.append_exn f (Bytes.to_string buf);
           ());
    let* () = Aof.flush f in
    let* () = Aof.close f in
    Ok ()

  (* be more precise about errors... *)
  let save' t fn : (_, save_error) result =
    save' t fn |> function
    | Ok () -> Ok ()
    | Error x -> (
        match x with
        | `Double_close | `File_exists _ | `Pending_flush | `Ro_not_allowed
        | `Write_on_closed ->
            (* looking at the code, these errors can't actually happen *)
            assert false
        | `Io_misc x -> Error (`Io_misc x))

  let _ = save'

  type load_error =
    [ `Double_close
    | `Invalid_argument
    | `Io_misc of Io.misc_error
    | `No_such_file_or_directory
    | `Not_a_file
    | `Read_on_closed
    | `Read_out_of_bounds ]

  let load' fn : (_, load_error) result =
    let open Result_syntax in
    let* io = Io.open_ ~path:fn ~readonly:true in
    let* sz_bytes = Io.read_size io in
    let sz_bytes = Int63.to_int sz_bytes in
    let ok = sz_bytes mod (3 * 8) = 0 in
    let _check_ints_size =
      if not ok then
        failwith
          (Printf.sprintf "%s: file %s did not contain 3*n ints" __FILE__ fn)
    in
    assert ok;
    (* NOTE we expect the sparse map file to be under 50MB; if it is over 100MB then we
       issue a warning *)
    let _warn_if_size_large =
      if sz_bytes > 100 * megabyte then
        [%log.warn
          "The size of the sparse map file is very large (%d bytes)" sz_bytes]
    in
    let sz = sz_bytes / 8 in
    (* NOTE sz is number of ints *)
    (* Since we are going to turn the entries into a map, we are not too worried about
       holding all the bytes in the file in memory *)
    let* contents = Io.read_to_string io ~off:Int63.zero ~len:sz in
    (* now we can iterate through the contents, filling the map *)
    let m =
      (0, empty)
      |> iter_k (fun ~k (i, m) ->
             match i + 2 < sz with
             | false -> m
             | true ->
                 let x, y, z =
                   ( String.get_int64_le contents (i * 8),
                     String.get_int64_le contents ((i + 1) * 8),
                     String.get_int64_le contents ((i + 2) * 8) )
                 in
                 let voff, off, len =
                   (Int64.to_int x, Int64.to_int y, Int64.to_int z)
                 in
                 let m = add voff (off, len) m in
                 k (i + 3, m))
    in
    (* remember to close the io *)
    let* () = Io.close io in
    Ok m

  let _ = load'
end

(** Private implementation of sparse files *)
module Private = struct
  open struct
    module Map_ = Private_map
  end

  let data_fn = "data"
  let map_fn = "map"

  type nonrec map = Map_.map

  type t = {
    dir : string;
    fd : Unix.file_descr;
    mutable map : map;
    readonly : bool;
  }

  (* Construction functions ---- *)

  let save_map t = Map_.save t.map Fn.(t.dir / map_fn)

  let create ~path =
    let ok = not (Sys.file_exists path) in
    assert ok;
    mkdir ~path;
    let fd =
      Unix.(openfile Fn.(path / data_fn) [ O_RDWR; O_CREAT; O_TRUNC ] 0o660)
    in
    let t = { dir = path; fd; map = Int_map.empty; readonly = false } in
    (* we also create an initially empty map file; we can't open in future if the map file
       doesn't exist *)
    let _ = save_map t in
    t

  let open_ro ~dir =
    assert (Sys.file_exists dir);
    assert (Sys.file_exists Fn.(dir / data_fn));
    assert (Sys.file_exists Fn.(dir / map_fn));
    let fd = Unix.(openfile Fn.(dir / data_fn) [ O_RDONLY ] 0) in
    let map = Map_.load Fn.(dir / map_fn) in
    { dir; fd; map; readonly = true }

  let flush t =
    if not t.readonly then save_map t;
    Unix.fsync t.fd

  let close t =
    flush t;
    Unix.close t.fd

  let map_add t ~virt_off ~real_off ~len =
    t.map <- Int_map.add virt_off (real_off, len) t.map

  let append_region t ~(src : Pread.t) ~src_off ~len ~virt_off =
    assert (len > 0);
    (* read in small chunks, in case the region is very large *)
    let buf_sz = 5 * 4096 in
    let buf = Bytes.create buf_sz in
    (* seek to end of sparse file *)
    let real_off = Unix.(lseek t.fd 0 SEEK_END) in
    let off = ref src_off in
    len
    |> iter_k (fun ~k len ->
           match len > 0 with
           | false -> ()
           | true ->
               src.pread ~off ~len:(min buf_sz len) ~buf |> fun n' ->
               (* we could add error messages for situations where asserts fail *)
               assert (n' > 0);
               assert (Unix.write t.fd buf 0 n' = n');
               k (len - n'));
    (* add map entry *)
    map_add t ~virt_off ~real_off ~len;
    ()

  (** When we try to find a (virt_off,len) region within the sparse file, there
      are various possibilities. We first find the largest voff'
      smaller-or-equal-to virt_off for which there exists entry
      [(voff',(roff',len'))] in the map. Then we have the following
      possibilities:

      - No entry found: The first data region starts beyond [virt_off]; this is
        "starts in gap"

      - Within: (line shows relative relationships between 4 positions)

      {v
----------------------------------------------
  ^voff'  ^virt_off  ^virt_off+len  ^voff'+len'
      v}

      In this case we can safely read the entirety of the data.

      - Starts in gap:

      {v
----------------------------------------
  ^voff' ^voff'+len' ^virt_off
      v}
      - Extends beyond:

      {v
----------------------------------------------
  ^voff'  ^virt_off  ^voff'+len'  ^virt_off+len 
      v}

      In this case, we can only read some of the data, and we pad the rest with
      dummy bytes. *)
  type real_region =
    | Within of { real_off : int }
    | Starts_in_gap
    | Extends_beyond of { real_off : int; real_len : int }

  let translate_vreg map ~virt_off ~len =
    Int_map.find_last_opt (fun voff' -> voff' <= virt_off) map |> function
    | None ->
        Log.err (fun m ->
            m "%s: No virtual offset found <= %d" __FILE__ virt_off);
        Starts_in_gap
    | Some (voff', (roff', len')) -> (
        match voff' + len' <= virt_off with
        | true -> Starts_in_gap
        | false -> (
            match virt_off + len <= voff' + len' with
            | true -> Within { real_off = roff' + (virt_off - voff') }
            | false ->
                Extends_beyond
                  {
                    real_off = roff' + (virt_off - voff');
                    real_len = voff' + len' - virt_off;
                  }))

  let pread t ~off ~len ~buf =
    (* translate the virtual offset to a real offset *)
    let real = translate_vreg t.map ~virt_off:!off ~len in
    match real with
    | Within { real_off } ->
        let n = File.pread t.fd ~off:(ref real_off) ~len ~buf in
        assert (n = len);
        off := !off + n;
        n
    | Starts_in_gap ->
        Log.err (fun m ->
            m "%s: attempt to read from gap; voff=%d, len=%d" __FILE__ !off len);
        Fmt.failwith "%s: attempt to read from gap; voff=%d, len=%d" __FILE__
          !off len
    | Extends_beyond { real_off; real_len } ->
        (* NOTE we need to allow this case, because a user may read more bytes than they
           need into a buffer, then attempt to decode what they have read; we want to allow
           this usage *)
        Printf.printf
          "%s: attempt to read beyond; off=%d, len=%d, real_off=%d real_len=%d\n\
           %!"
          __FILE__ !off len real_off real_len;
        assert (real_len < len);
        (* first fill buf with 0s (or something else) *)
        Bytes.fill buf real_len (len - real_len) '!';
        (* copy the data that we can *)
        let n = File.pread t.fd ~off:(ref real_off) ~len:real_len ~buf in
        assert (n = real_len);
        off := !off + len;
        len
end

include (Private : S)
