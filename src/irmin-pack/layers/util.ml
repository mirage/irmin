(** Miscellaneous small utilities *)


(** {1 Logging} *)

let src = Logs.Src.create "irmin.layers.io" ~doc:"IO for irmin-layers"

module Log = (val Logs.src_log src : Logs.LOG)

include Log


(** Essentially the Y combinator; useful for anonymous recursive
    functions. The k argument is the recursive callExample:

    {[
      iter_k (fun ~k n -> 
          if n = 0 then 1 else n * k (n-1))

    ]}


*)
let iter_k f (x:'a) =
  let rec k x = f ~k x in
  k x  

(** This module provides the infix [/] operator for string concat *)
module Fn = struct
  let ( / ) = Filename.concat
end


(** Abbrev *)
module BA1 = Bigarray.Array1

(** Printf abbreviations *)
module P = struct
  include Printf
  let s = sprintf 
  let p = printf
end

module F = struct
  include Format
  let s = sprintf 
  let p = printf
end


(** For testing *)
let random_int_list ~size = 
  let ints = 
    (0,[]) |> iter_k (fun ~k (len,xs) -> 
        if len >=size then xs else
          k (len+1,(Random.int (1024 * 1024 * 1024 -1))::xs))
  in
  ints

type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t

module Int_mmap : 
sig 
  type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t = private { fn:string; fd:Unix.file_descr; mutable arr: int_bigarray }
  val create : fn:string -> sz:int -> t
    
  (** NOTE [open_ ~fn ~sz] can use [sz=-1] to open with size based on the size of the
      underlying file *)
  val open_  : fn:string -> sz:int -> t
  val close  : t -> unit
end      
= struct
  type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t = { fn:string; fd:Unix.file_descr; mutable arr: int_bigarray }

  (* NOTE both following are shared *)
  let shared = true

  let create ~fn ~sz =
    assert(not (Sys.file_exists fn) || begin
        Printf.printf "File exists: %s\n%!" fn; false end);
    let fd = Unix.(openfile fn [O_CREAT;O_RDWR;O_TRUNC;O_EXCL;O_CLOEXEC] 0o660) in
    let arr = 
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fn; fd; arr }

  (* NOTE sz=-1 is recognized by [map_file] as "derive from size of file"; if we want a
     different size (eg because we want the file to grow) we can provide it explicitly *)
  let open_ ~fn ~sz =
    assert(Sys.file_exists fn);
    let fd = Unix.(openfile fn [O_RDWR] 0o660) in
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


(** This test code checks that we can read back data written using an mmap; this seems
    fine, but we maybe have to account for whether the arch is big/little endian *)
module Int_mmap_test() = struct

  let fn = "tmp.mmap" 

  let t = 
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz:5

  let arr = t.arr

  let _ = 
    arr. { 0 } <- 0;
    arr. { 1 } <- 1;
    arr. { 2 } <- (-1);
    arr. { 3 } <- Int.min_int;
    arr. { 4 } <- Int.max_int

  let _ = Int_mmap.close t

  let fd = Unix.openfile fn [O_RDWR] 0o777

  let _ = print_endline "Reading back in"

  let _ =
    let len = 5 in
    let arr = Array.init len (fun _ -> Bytes.create 8) in
    for i = 0 to len-1 do
      Unix.read fd arr.(i) 0 8 |> fun nread -> 
      assert(nread=8);
      Printf.printf "Read bytes: %S\n" (Bytes.to_string arr.(i))
    done;
    for i = 0 to len-1 do
      Printf.printf "%d\n"
        (* x86 is little endian; presumably bigendian is used on some other archs; so use
           "native endian" *)
        (Bytes.get_int64_ne arr.(i) 0 |> Int64.to_int)
    done


end

module Out_channel_extra = struct

  let _ = Sys.word_size = 64

  (* FIXME check that mmap on big-endian archs uses big-endian format; perhaps have a
     test for this *)

  (** [output_int_ne oc i] writes [i] to [oc] using native endian format, suitable for
      reading by mmap; not thread safe - shared buffer *)
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
      assert(nread=8);
      Bytes.get_int64_ne buf 0 |> Int64.to_int

end

(* NOTE we can just use an output channel and write ints in big endian format
(** Growable int mmap

If [arr] is the mmap'ed bigarray, then rather than accessing [arr.{i}] directly, we check
if it is within the arr bounds; if not, we grow the mmap in [chunk_sz] amounts. When we
close, we truncate so that the file doesn't contain any extra zero entries in the last
chunk.
*)
module Growable_int_mmap : sig
  type t 
  val create : fn:string -> sz:int -> t
  val open_ : fn:string -> sz:int -> t
  val grow : t -> int -> unit
  val set : t -> int -> int -> unit
  val close : t -> unit
end = struct

type t = { mutable mmap: Int_mmap.t; mutable real_sz:int } 
(* real size of written portion of underlying file in ints *)

let create ~fn ~sz = Int_mmap.create ~fn ~sz |> fun mmap -> {mmap;real_sz=0}

let open_ ~fn ~sz = Int_mmap.open_ ~fn ~sz |> fun mmap -> {mmap;real_sz=BA1.dim mmap.arr}

(* Amount (in ints) we grow the mmap each time *)
let chunk_sz = 10 * (4096 (* blk sz *) / 8 (* bytes per int *))

(** Grow the underlying mmap so that we can write at the index position [i] *)
let grow t i = 
  let sz = Bigarray.Array1.dim t.mmap.arr in
  match i < sz with
  | true -> ()
  | false -> 
    let n_chunks = 1+(sz / chunk_sz) in
    Int_mmap.close t.mmap;
    t.mmap <- Int_mmap.open_ ~fn:t.mmap.fn ~sz:(n_chunks * chunk_sz);
    ()

let set t i v = 
  grow t i;
  t.real_sz <- max i t.real_sz;
  t.mmap.arr.{ i } <- v

let close t =
  Unix.ftruncate t.mmap.fd (t.real_sz * 8 (* bytes per int *));
  Int_mmap.close t.mmap;
  ()

end
*)


(** A small (easily held in memory) file containing just ints; loaded
   via mmap; created via mmap (so, you need to know the number of ints
   upfront).  *)
module Small_int_file_v1 = struct 
  let load fn : int list = 
    (* check that the size is a multiple of 8 bytes, then mmap and
       read from array *)
    let fsz = Unix.(stat fn |> fun st -> st.st_size) in      
    ignore(fsz mod 8 = 0 || failwith "File size is not a multiple of 8");
    let sz = fsz / 8 in (* size of resulting int list *)
    let fd = Unix.(openfile fn [O_RDWR] 0o660(*dummy*)) in
    (* NOTE O_RDWR seems to be required even though we only open in
       readonly mode; FIXME after bug fix, maybe we can now use O_RDONLY *)
    let shared = false in 
    (* if another mmap is open on the same region, with shared=true,
       then we can't open with shared=false? *)
    (* log (P.s "Small_int_file.load: calling Unix.map_file fn=%s" fn); *)
    let mmap = Bigarray.(Unix.map_file fd Int c_layout shared [| sz |]) 
               |> Bigarray.array1_of_genarray
    in
    (* log "Small_int_file.load: finished calling Unix.map_file"; *)
    (* now iterate through, constructing the map *)
    let ints = List.init sz (fun i -> mmap.{i}) in
    Unix.close fd;
    Gc.full_major (); (* reclaim mmap; finalizes mmap *)
    ints

  let save (ints: int list) fn : unit =
    let fd = Unix.(openfile fn [O_CREAT;O_RDWR;O_TRUNC] 0o660) in
    let shared = true in
    let sz = List.length ints in
    (* log (P.s "Small_int_file.save: calling Unix.map_file fn=%s" fn); *)
    let mmap = Bigarray.(Unix.map_file fd Int c_layout shared [| sz |]) 
               |> Bigarray.array1_of_genarray
    in
    (* log "Small_int_file.save: finished calling Unix.map_file"; *)
    let i = ref 0 in
    (* NOTE that Map.iter operates in key order, lowest first *)
    ints|> List.iter (fun k -> 
        mmap.{ !i } <- k;
        i:=!i + 1;
        ());
    Unix.fsync fd;
    Unix.close fd;
    Gc.full_major (); (* reclaim mmap; finalizes mmap *)
    ()

  module Test() = struct
    let _ = 
      let size = 1_000_000 in
      let ints = random_int_list ~size in
      assert(List.length ints = size);
      let fn = Filename.temp_file "test" ".tmp" in
      save ints fn;
      let ints' = load fn in
      Unix.unlink fn;
      assert(ints = ints');
      ()
  end
end

module Int_map = Map.Make(Int)

module Add_load_save_funs(S:sig type t[@@deriving sexp] end) = struct
  open S

  let save t fn = Sexplib.Sexp.save_hum fn (t |> sexp_of_t)

  let load fn = Sexplib.Sexp.load_sexp fn |> t_of_sexp

  let to_string t = Sexplib.Sexp.to_string_hum (t |> sexp_of_t)
  
  (* this loads from the file name s! *)
  let _of_string s = Sexplib.Sexp.load_sexp s |> t_of_sexp

  let of_string s = Sexplib.Sexp.of_string s |> t_of_sexp
      
  let to_bytes t = t |> to_string |> Bytes.unsafe_of_string

  let of_bytes bs = bs |> Bytes.unsafe_to_string |> of_string

end

(** Common strings for filenames *)
let control_s = "control" (** The control file is always called "control" *)




let mkdir ~path = 
  let ok = not (Sys.file_exists path) in
  assert(ok);  
  Unix.mkdir path 0o770;
  ()

module Pwrite = struct
  type t = {
    pwrite: off:int ref -> bytes -> unit;
  }
end

module Pread = struct
  type t = {
    pread : off:int ref -> len:int -> buf:bytes -> int; 
  }
end

module File = struct
  let create ~path =
    let ok = not (Sys.file_exists path) in
    assert(ok);
    let fd = Unix.(openfile path [O_CREAT;O_RDWR;O_EXCL;O_CLOEXEC] 0o660) in
    fd

  let open_ ~path =
    let ok = Sys.file_exists path in
    assert(ok);
    let fd = Unix.(openfile path [O_RDWR;O_CLOEXEC] 0o660) in
    fd

  let pread fd ~off ~len ~buf =
    assert(len <= Bytes.length buf);
    ignore(Unix.lseek fd !off SEEK_SET);    
    let pos =
      0 |> iter_k (fun ~k pos -> 
          match pos=len with 
          | true -> pos
          | false -> 
            let n = Unix.read fd buf pos (len - pos) in
            if n = 0 then pos else k (pos+n))
    in
    off := !off + pos;
    pos   

  let read_string ~fd ~off ~len = 
    let buf = Bytes.create len in
    let n = pread fd ~off ~len ~buf in
    Bytes.unsafe_to_string (Bytes.sub buf 0 n)

  let pwrite fd ~off buf =
    ignore(Unix.lseek fd !off SEEK_SET);    
    let len = Bytes.length buf in
    let n = Unix.write fd buf 0 len in
    assert(n=len);
    off := !off + n;
    ()    

  let size fn = Unix.((stat fn).st_size)

  let copy ~(src:Pread.t) ~(dst:Pwrite.t) ~src_off ~len ~dst_off = 
    match len = 0 with
    | true -> ()
    | false -> 
      let Pread.{pread},Pwrite.{pwrite} = src,dst in
      let src_off = ref src_off in
      let dst_off = ref dst_off in
      let buf_sz = 8192 in
      let buf = Bytes.create buf_sz in
      len |> iter_k (fun ~k len -> 
          match len <=0 with
          | true -> ()
          | false -> 
            let n = pread ~off:src_off ~len:(min buf_sz len) ~buf in
            (if n=0 then Log.warn (fun m -> m "pread returned n=0 bytes, off=%d len=%d" !src_off (min buf_sz len)));
            assert(n>0);
            pwrite ~off:dst_off (Bytes.sub buf 0 n);
            k (len - n))
end






module Binary_search = struct

  (* Based on https://en.wikipedia.org/wiki/Binary_search_algorithm, the first pseudocode;
     could replace arr,get with just get *)
  let binary_search ~arr ~get ~lo ~hi ~key =
    (lo,hi) |> iter_k (fun ~k:kont (lo,hi) -> 
        match lo <= hi with
        | false -> None
        | true -> 
          let mid = (lo+hi)/2 in
          let arr_mid = get arr mid in
          match Stdlib.compare arr_mid key with
          | x when x < 0 -> kont (mid+1,hi)
          | x when x > 0 -> kont (lo,mid-1)
          | _ (* when x=0 *) -> Some mid (* NOTE returns the index, not the value *))    
    
  let _ : arr:'a -> get:('a -> int -> 'b) -> lo:int -> hi:int -> key:'b -> int option = binary_search

  let nearest_leq ~arr ~get ~lo ~hi ~key = 
    match get arr lo <= key with
    | false -> `All_gt_key
    | true -> 
      match get arr hi <= key with
      | true -> `Some hi
      | false -> 
        (lo,hi) |> iter_k (fun ~k:kont (lo,hi) -> 
            assert(get arr lo <= key && key < get arr hi);
            assert(lo < hi);
            match lo+1 = hi with 
            | true -> `Some lo
            | false -> 
              let mid = (lo+hi)/2 in
              let arr_mid = get arr mid in
              match arr_mid <= key with 
              | true -> kont (mid,hi)
              | false -> kont (lo,mid))

  let test_2 () = 
    let arr = Array.of_list [1;3;5;7] in
    let get arr i = arr.(i) in
    let lo,hi = 0,Array.length arr -1 in
    let nearest_leq_ key = nearest_leq ~arr ~get ~lo ~hi ~key in
    assert(nearest_leq_ 0 = `All_gt_key); 
    assert(nearest_leq_ 1 = `Some 0);
    assert(nearest_leq_ 2 = `Some 0);
    assert(nearest_leq_ 3 = `Some 1);
    assert(nearest_leq_ 3 = `Some 1);
    assert(nearest_leq_ 4 = `Some 1);
    assert(nearest_leq_ 5 = `Some 2);
    assert(nearest_leq_ 6 = `Some 2);
    assert(nearest_leq_ 7 = `Some 3);
    assert(nearest_leq_ 8 = `Some 3);
    assert(nearest_leq_ 100 = `Some 3);
    ()
    
    
end

(** Currently used for logging where we get to in the code *)
let create_marks ~__FILE__ (xs:string list) = 
  let arr = Array.of_list xs in
  let mark i = Printf.printf "%s: mark: %s\n%!" __FILE__ arr.(i) in
  mark,(List.mapi (fun i _ -> i) xs)
