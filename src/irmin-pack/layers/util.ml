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


(** Printf abbreviations *)
module P = struct
  include Printf
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
  type t = private { fd:Unix.file_descr; mutable arr: int_bigarray }
  val create : fn:string -> sz:int -> t
  val open_  : fn:string -> sz:int -> t
  val close  : t -> unit
end      
= struct
  type int_bigarray = (int, Bigarray.int_elt, Bigarray.c_layout) Bigarray.Array1.t
  type t = { fd:Unix.file_descr; mutable arr: int_bigarray }

  (* NOTE both following are shared *)
  let shared = true

  let create ~fn ~sz =
    assert(not (Sys.file_exists fn));
    let fd = Unix.(openfile fn [O_CREAT;O_RDWR;O_TRUNC;O_EXCL;O_CLOEXEC] 0o660) in
    let arr = 
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fd; arr }

  let open_ ~fn ~sz =
    assert(Sys.file_exists fn);
    let fd = Unix.(openfile fn [O_RDWR] 0o660) in
    let arr = 
      let open Bigarray in
      Unix.map_file fd Int c_layout shared [| sz |] |> array1_of_genarray
    in
    { fd; arr }

  let close t = 
    Unix.close t.fd;
    (* following tries to make the array unreachable, so GC'able; however, no guarantee
       that arr actually is unreachable *)
    t.arr <- Bigarray.(Array1.create Int c_layout 0);
    ()
end


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
    
end


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
