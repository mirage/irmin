(* commented test code, moved from gc_ files *)

(*


(** This test code checks that we can read back data written using an mmap; this
    seems fine, but we maybe have to account for whether the arch is big/little
    endian *)
module Int_mmap_test () = struct
  let fn = "tmp.mmap"

  let t =
    (try Unix.unlink fn with _ -> ());
    Int_mmap.create ~fn ~sz:5

  let arr = t.arr

  let _ =
    arr.{0} <- 0;
    arr.{1} <- 1;
    arr.{2} <- -1;
    arr.{3} <- Int.min_int;
    arr.{4} <- Int.max_int

  let _ = Int_mmap.close t
  let fd = Unix.openfile fn [ O_RDWR ] 0o777
  let _ = print_endline "Reading back in"

  let _ =
    let len = 5 in
    let arr = Array.init len (fun _ -> Bytes.create 8) in
    for i = 0 to len - 1 do
      Unix.read fd arr.(i) 0 8 |> fun nread ->
      assert (nread = 8);
      Printf.printf "Read bytes: %S\n" (Bytes.to_string arr.(i))
    done;
    for i = 0 to len - 1 do
      Printf.printf "%d\n"
        (* x86 is little endian; presumably bigendian is used on some other archs; so use
           "native endian" *)
        (Bytes.get_int64_ne arr.(i) 0 |> Int64.to_int)
    done
end


*)
