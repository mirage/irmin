(** A sparse file is "a file containing gaps"; the gaps do not take up space on disk.

The implementation uses a normal "data" file for the non-gap data, and a "map" file, which
is used to translate "addresses in the sparse file" (aka "virtual addresses") to
"addresses in the data file".

The prototype, which includes extensive documentation, can be found at {{: https://github.com/tomjridge/sparse-file/}}.

*)

open Util

(** Signature for sparse files; file-like with [append_region] and [pread] *)
module type S = sig

  (** The type of sparse files. *)
  type t

  val create: path:string -> t
  (** [create ~path] creates an empty sparse file; requires [path] is non-existent;
      creates directory [path] and files [path/data] to hold the data and [path/map] to
      hold the mapping from virtual offset to (real offset and length) *)

  (** [open_ro ~dir] opens the sparse file located in [dir] *)
  val open_ro : dir:string -> t

  (** [close t] saves the sparse map and closes the underlying file descriptor for the
      sparse data file *)
  val close: t -> unit

  (** [append_region t ~src ~src_off ~len ~virt_off] appends len bytes from [src] at
      offset [src_off], to the end of sparse file [t].

      A new map entry [virt_off -> (real_off,len)] is added, where [real_off] is the
      offset of the end of the sparse data file before the append took place.

      NOTE [len] must not be 0; no other validity checks are made on [off] and [len]. It
      is possible to add the same region multiple times for example, or add overlapping
      regions, etc. {b These usages should be avoided.}  *)
  val append_region: t -> src:Pread.t -> src_off:int -> len:int -> virt_off:int -> unit

  (** [pread t ~off ~len ~buf] reads [len] bytes from virtual offset [off] into [buf],
      updating [off] and returning the number of bytes read.

      A read that extends beyond the end of a data region will be supplemented with dummy
      bytes (this scenario is allowed with irmin-pack because often the user does not know
      how many bytes to read, so they read more than is strictly necessary and then
      examine the bytes they have read to try to decode an object).

      A read that starts in an empty region will fill the buffer with dummy bytes (this
      scenario is almost certainly an error case, so we could instead throw an
      exception).  *)
  val pread : t -> off:int ref -> len:int -> buf:bytes -> int

end

(** Private implementation of the "map" file *)
module Private_map = struct
  type map = (int * int) Int_map.t

  include Int_map
  let load fn = 
    let sz_bytes = File.size fn in
    let ok = sz_bytes mod (3*8) = 0 in
    let _check_ints_size = 
      if not ok then 
        failwith (P.s "%s: file %s did not contain 3*n ints" __FILE__ fn)
    in
    assert(ok);
    let sz = sz_bytes / 8 in
    let mmap = Int_mmap.open_ ~fn ~sz:(sz_bytes / 8) in
    let arr = mmap.arr in
    let m = 
      (0,empty) |> iter_k (fun ~k (i,m) ->         
          match i+2 < sz with 
          | false -> m
          | true ->
            let voff,off,len = arr.{i},arr.{i+1},arr.{i+2} in
            let m = add voff (off,len) m in
            k (i+3,m))
    in
    m

  let save t fn = 
    let sz_keys = cardinal t in
    let dir = Filename.dirname fn in
    let tmp = Filename.temp_file ~temp_dir:dir "sparse." ".map" in
    let mmap = Int_mmap.open_ ~fn:tmp ~sz:(sz_keys * 3) in
    let arr = mmap.arr in
    let i = ref 0 in
    t |> iter (fun voff (roff,len) -> 
        arr.{!i} <- voff;
        arr.{!i+1} <- roff;
        arr.{!i+2} <- len;
        i:=!i+3);
    Int_mmap.close mmap;
    Unix.rename tmp fn;
    ()

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
    dir         : string;
    fd          : Unix.file_descr; 
    mutable map : map; 
    readonly    : bool 
  }

  (* Construction functions ---- *)

  let save_map t = Map_.save t.map Fn.(t.dir / map_fn)

  let create ~path = 
    let ok = not (Sys.file_exists path) in
    assert(ok);
    mkdir ~path;
    let fd = Unix.(openfile Fn.(path / data_fn) [O_RDWR;O_CREAT;O_TRUNC] 0o660) in
    let t = { dir=path; fd; map=Map_.empty; readonly=false } in
    (* we also create an initially empty map file; we can't open in future if the map file
       doesn't exist *)
    let _ = save_map t in
    t

  let open_ro ~dir = 
    assert(Sys.file_exists dir);
    assert(Sys.file_exists Fn.(dir / data_fn));
    assert(Sys.file_exists Fn.(dir / map_fn));
    let fd = Unix.(openfile Fn.(dir / data_fn) [O_RDONLY] 0) in
    let map = Map_.load Fn.(dir / map_fn) in
    { dir; fd; map; readonly=true }

  let flush t = 
    (if not t.readonly then save_map t);
    Unix.fsync t.fd

  let close t = 
    flush t;
    Unix.close t.fd

  let map_add t ~virt_off ~real_off ~len = t.map <- Map_.add virt_off (real_off,len) t.map

  let append_region t ~(src:Pread.t) ~src_off ~len ~virt_off =
    assert(len > 0);
    (* read in small chunks, in case the region is very large *)
    let buf_sz = 5 * 4096 in
    let buf = Bytes.create buf_sz in 
    (* seek to end of sparse file *)
    let real_off = Unix.(lseek t.fd 0 SEEK_END) in
    let off = ref src_off in
    len |> iter_k (fun ~k len -> 
        match len > 0 with 
        | false -> ()
        | true -> 
          src.pread ~off ~len:(min buf_sz len) ~buf |> fun n' -> 
          (* we could add error messages for situations where asserts fail *)
          assert(n' > 0);
          assert(Unix.write t.fd buf 0 n' = n');
          k (len - n'));
    (* add map entry *)
    map_add t ~virt_off ~real_off ~len;
    ()

  (** 

When we try to find a (virt_off,len) region within the sparse file, there are various
possibilities. We first find the largest voff' smaller-or-equal-to virt_off for which
there exists entry [(voff',(roff',len'))] in the map. Then we have the following
possibilities:

- No entry found: The first data region starts beyond [virt_off]; this is "starts in gap"

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

In this case, we can only read some of the data, and we pad the rest with dummy bytes.


 *)
  type real_region = 
    | Within of { real_off:int }
    | Starts_in_gap
    | Extends_beyond of { real_off:int; real_len:int }

  let translate_vreg map ~virt_off ~len = 
    Map_.find_last_opt (fun voff' -> voff' <= virt_off) map |> function
    | None ->
      Log.err (fun m -> m "%s: No virtual offset found <= %d" __FILE__ virt_off);
      Starts_in_gap
    | Some (voff',(roff',len')) -> 
      match voff' + len' <= virt_off with
      | true -> Starts_in_gap
      | false -> 
        match virt_off + len <= voff' + len' with
        | true -> Within { real_off=roff'+(virt_off - voff') }
        | false -> 
          Extends_beyond { real_off=roff'+(virt_off - voff'); real_len=voff'+len'-virt_off }

  let pread t ~off ~len ~buf =
    (* translate the virtual offset to a real offset *)
    let real = translate_vreg t.map ~virt_off:!off ~len in
    match real with
    | Within { real_off } -> 
      let n = File.pread t.fd ~off:(ref real_off) ~len ~buf in
      assert(n=len);
      off:=!off+n;
      n
    | Starts_in_gap -> 
      Log.err (fun m -> m "%s: attempt to read from gap; voff=%d, len=%d" __FILE__ !off len);
      Fmt.failwith "%s: attempt to read from gap; voff=%d, len=%d" __FILE__ !off len
    | Extends_beyond { real_off; real_len } -> 
      (* NOTE we need to allow this case, because a user may read more bytes than they
         need into a buffer, then attempt to decode what they have read; we want to allow
         this usage *)
      Printf.printf "%s: attempt to read beyond; off=%d, len=%d, real_off=%d real_len=%d\n%!" __FILE__ (!off) len real_off real_len;
      assert(real_len < len);
      (* first fill buf with 0s (or something else) *)
      Bytes.fill buf real_len (len-real_len) '!';
      (* copy the data that we can *)
      let n = File.pread t.fd ~off:(ref real_off) ~len:real_len ~buf in
      assert(n=real_len);
      off:=!off+len;
      len
end

include (Private : S)

