(* NOTE (adating to new IO): some of this can be deleted (eg small_int_file_v1? ,
   add_load_save_funs, mkdir (new IO has this); binary_search (assuming we don't need it
   because we are happy with an in-memory map)). *)

(** Miscellaneous small utilities *)

(** {1 Logging} *)

module Log = Import.Log

(** Essentially the Y combinator; useful for anonymous recursive functions. The
    k argument is the recursive callExample:

    {[
      iter_k (fun ~k n -> if n = 0 then 1 else n * k (n - 1))
    ]} *)
let iter_k f (x : 'a) =
  let rec k x = f ~k x in
  k x

(** This module provides the infix [/] operator for string concat *)
module Fn = struct
  let ( / ) = Filename.concat
end

module BigArr1 = Bigarray.Array1
(** Abbrev *)

type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Int_mmap : sig
  type int_bigarray =
    (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = private {
    fn : string;
    fd : Unix.file_descr;
    mutable arr : int_bigarray;
  }

  val create : fn:string -> sz:int -> t

  val open_ : fn:string -> sz:int -> t
  (** NOTE [open_ ~fn ~sz] can use [sz=-1] to open with size based on the size
      of the underlying file *)

  val close : t -> unit
end = struct
  type int_bigarray =
    (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

  type t = { fn : string; fd : Unix.file_descr; mutable arr : int_bigarray }

  (* NOTE both following are shared *)
  let shared = true

  let create ~fn ~sz =
    assert (
      (not (Sys.file_exists fn))
      ||
      (Printf.printf "File exists: %s\n%!" fn;
       false));
    let fd =
      Unix.(openfile fn [ O_CREAT; O_RDWR; O_TRUNC; O_EXCL; O_CLOEXEC ] 0o660)
    in
    let arr =
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  (* NOTE sz=-1 is recognized by [map_file] as "derive from size of file"; if we want a
     different size (eg because we want the file to grow) we can provide it explicitly *)
  let open_ ~fn ~sz =
    assert (Sys.file_exists fn);
    let fd = Unix.(openfile fn [ O_RDWR ] 0o660) in
    let arr =
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  let close t =
    Unix.close t.fd;
    (* following tries to make the array unreachable, so GC'able; however, no guarantee
       that arr actually is unreachable *)
    t.arr <- Bigarray.(Array1.create Int c_layout 0);
    ()
end

module Out_channel_extra = struct
  let _ = assert(Sys.word_size = 64)

  (* FIXME check that mmap on big-endian archs uses big-endian format; perhaps have a
     test for this *)

  (** [output_int_ne oc i] writes [i] to [oc] using native endian format,
      suitable for reading by mmap; not thread safe - shared buffer *)
  let output_int_ne =
    let buf = Bytes.create 8 in
    fun oc i ->
      Bytes.set_int64_ne buf 0 (Int64.of_int i);
      (* NOTE Stdlib.output_binary_int always uses big-endian, but mmap is (presumably)
         arch dependent, hence we use set_int64_ne *)
      Stdlib.output_bytes oc buf;
      ()

  let input_int_ne =
    let buf = Bytes.create 8 in
    fun ic ->
      Stdlib.input ic buf 0 8 |> fun nread ->
      assert (nread = 8);
      Bytes.get_int64_ne buf 0 |> Int64.to_int
end

module Int_map = Map.Make (Int)

(** Common strings for filenames *)
let control_s = "control"
(** The control file is always called "control" *)

let mkdir ~path =
  let ok = not (Sys.file_exists path) in
  assert ok;
  Unix.mkdir path 0o770;
  ()

module Pwrite = struct
  type t = { pwrite : off:int ref -> bytes -> unit }
end

module Pread = struct
  type t = { pread : off:int ref -> len:int -> buf:bytes -> int }
end

module File = struct
  let create ~path =
    let ok = not (Sys.file_exists path) in
    assert ok;
    let fd =
      Unix.(openfile path [ O_CREAT; O_RDWR; O_EXCL; O_CLOEXEC ] 0o660)
    in
    fd

  let open_ ~path =
    let ok = Sys.file_exists path in
    assert ok;
    let fd = Unix.(openfile path [ O_RDWR; O_CLOEXEC ] 0o660) in
    fd

  let pread fd ~off ~len ~buf =
    assert (len <= Bytes.length buf);
    ignore (Unix.lseek fd !off SEEK_SET);
    let pos =
      0
      |> iter_k (fun ~k pos ->
             match pos = len with
             | true -> pos
             | false ->
                 let n = Unix.read fd buf pos (len - pos) in
                 if n = 0 then pos else k (pos + n))
    in
    off := !off + pos;
    pos

  let read_string ~fd ~off ~len =
    let buf = Bytes.create len in
    let n = pread fd ~off ~len ~buf in
    Bytes.unsafe_to_string (Bytes.sub buf 0 n)

  let pwrite fd ~off buf =
    ignore (Unix.lseek fd !off SEEK_SET);
    let len = Bytes.length buf in
    let n = Unix.write fd buf 0 len in
    assert (n = len);
    off := !off + n;
    ()

  let size fn = Unix.((stat fn).st_size)

  let copy ~(src : Pread.t) ~(dst : Pwrite.t) ~src_off ~len ~dst_off =
    match len = 0 with
    | true -> ()
    | false ->
        let Pread.{ pread }, Pwrite.{ pwrite } = (src, dst) in
        let src_off = ref src_off in
        let dst_off = ref dst_off in
        let buf_sz = 8192 in
        let buf = Bytes.create buf_sz in
        len
        |> iter_k (fun ~k len ->
               match len <= 0 with
               | true -> ()
               | false ->
                   let n = pread ~off:src_off ~len:(min buf_sz len) ~buf in
                   if n = 0 then
                     Log.warn (fun m ->
                         m "pread returned n=0 bytes, off=%d len=%d" !src_off
                           (min buf_sz len));
                   assert (n > 0);
                   pwrite ~off:dst_off (Bytes.sub buf 0 n);
                   k (len - n))
end

(** Currently used for logging where we get to in the code *)
let create_marks ~__FILE__ (xs : string list) =
  let arr = Array.of_list xs in
  let mark i = Printf.printf "%s: mark: %s\n%!" __FILE__ arr.(i) in
  (mark, List.mapi (fun i _ -> i) xs)

(** [rm_rf path] recursively deletes all files/dirs under path; behaviour with symlinks not checked *)
let rec rm_rf path =
  match Sys.is_directory path with
  | true ->
    Sys.readdir path |>
    Array.iter (fun name -> rm_rf (Filename.concat path name));
    Unix.rmdir path
  | false -> Sys.remove path
(* code taken from
     https://stackoverflow.com/questions/56327912/how-to-remove-a-non-empty-directory-with-ocaml
     ; NOTE may not work properly with symlinks etc.; assumes small directories *)
               
