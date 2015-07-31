(*
(*
Simple example showing how to create and use a Git store.
$ make # Compile
$ ./git_store # Run
$ cd /tmp/irmin/test && git log # Show the Git history
 *)
open Lwt
open Irmin_unix
open Nocrypto.Cipher_block


(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
      | "" -> ()
      | _ ->
	 Log.color_on ();
	 Log.set_log_level Log.DEBUG
  with Not_found -> ()

		      
(* AO and RW PERSISTANT BACKENDS *)
module AO = Irmin_fs.AO (* Irmin_mem.AO (*Irmin_git.AO (Git_unix.FS)*) *)
module RW = Irmin_fs.RW

	      
(* KRYPTO BACKEND *)
module KRYPTO_KM = Irmin_krypto.Make_km
module AES_CTR = AES.CTR (Counters.Inc_LE)
module KRYPTO_AES = Irmin_krypto.Make_cipher (KRYPTO_KM) (AES_CTR)
module KRYPTO = Irmin_krypto.KRYPTO_AO (KRYPTO_AES) (AO)

				       
(* CHUNCK BACKEND *)
module CHUNCK = Irmin_chunck.CHUNCK_AO (KRYPTO)

					     
(* INDEX FOR KEY CONVERGENCE *)
module INDEX = Irmin_index.HT

		 
(* STORE WITH THE APPLICATION OF FUNCTOR *)
module MY_STORE = Irmin_index.Make (INDEX) (CHUNCK) (RW)

let store = Irmin.basic (module MY_STORE) (module Irmin.Contents.String)

let main () =
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true () in
  Irmin.create store config task >>= fun t ->
  let content1 = "Hello world ! 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" in
  Irmin.update (t "t: Update 1.txt") ["root";"misc";"1.txt"] content1 >>= fun () ->
  Irmin.read_exn (t "t: Read 1.txt") ["root";"misc";"1.txt"] >>= fun file1 ->
  Printf.printf "Plain text: 1:%s \n%!" file1;
  Irmin.update (t "t: Update 2.txt") ["root";"misc";"2.txt"] "Hi! 222222222222222222222222222222222222222222222222222222222222222222222222222222 " >>= fun () ->
  Irmin.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "How are you ? 333333333333333333333333333333333333333333333333333333333333333333333333333 " >>= fun () ->
  Irmin.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"] >>= fun file ->
  Printf.printf "I've just read: %s\n%!" file;
  Irmin.clone_force task (t "x: Cloning 't'") "test" >>= fun x ->
  Irmin.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "Hohoho eljrzfnekjfneklfnekljfnelzf" >>= fun () ->
  Irmin.update (x "x: Update 2.txt") ["root";"misc";"2.txt"] "HELP! lemrkzfnlerfnleknrfklenflekrnflkezr" >>= fun () ->
  Irmin.merge_exn "t: Merge with 'x'" x ~into:t >>= fun () ->
  Irmin.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"] >>= fun file2 ->
  Irmin.read_exn (t "t: Read 3.txt") ["root";"misc";"3.txt"] >>= fun file3 ->
  Printf.printf "I've just read: 2:%s 3:%s\n%!" file2 file3;
  return_unit
    
let () =
  Lwt_unix.run (main ())

	       
 *)
