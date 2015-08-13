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
module AO = Irmin_fs.AO
module RW = Irmin_fs.RW

(* KRYPTO *)			
module KRYPTO_KM = Irmin_krypto.Make_km
module KRYPTO_AES = Irmin_krypto.Make_cipher (KRYPTO_KM) (AES.CTR)
module KRYPTO = Irmin_krypto.KRYPTO_AO (KRYPTO_AES) (AO)
	       
(* CHUNCK BACKEND *)
module LCK = Irmin_chunck.CHUNCK_AO (KRYPTO) 
					     
(* INDEX FOR KEY CONVERGENCE *)
module LINK = Irmin_link.FS
		 
(* STORE WITH THE APPLICATION OF FUNCTOR *)
module MY_STORE = Irmin_link.Make (LINK) (LCK) (RW)

let store = Irmin.basic (module MY_STORE) (module Irmin.Contents.String)
			
let main () =
  
  let config = Irmin_chunck.config ~size:4096 () in
  let config = Irmin_fs.config ~conf:config ~root:"/tmp/irmin"() in
      
  Irmin.create store config task >>= fun t ->

  let content1 = " Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus lectus odio, tincidunt eget accumsan a, tempor quis risus. In hac habitasse platea dictumst. Ut varius urna sed mollis ultricies. Aliquam bibendum erat sit amet eros tincidunt efficitur. Vivamus ullamcorper tincidunt mauris id maximus. Integer sapien pretium, viverra orci in, commodo elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur a lacus feugiat, consectetur mauris ut, malesuada erat. Fusce egestas, ante et gravida porttitor, turpis leo pretium tellus, ut lobortis diam turpis ac mauris. Sed sit amet neque dui. Etiam justo diam, gravida eu diam at, elementum rutrum est. Donec efficitur mattis quam, vitae venenatis massa pretium nec. Mauris rutrum id tellus sit amet aliquet. Praesent suscipit elementum lectus, eu malesuada lorem venenatis mollis. Vestibulum vitae malesuada turpis. Sed eleifend, erat iaculis lobortis tincidunt, massa eros ullamcorper risus, eget laoreet enim velit at orci. Nulla erat lacus, luctus eu augue sit amet, euismod lobortis ante. Aliquam volutpat placerat tristique. Pellentesque varius elit non risus molestie dapibus. Duis sed justo eget quam rutrum pharetra a nec mi. Nam sit amet libero dapibus odio hendrerit egestas. Aliquam erat volutpat. Suspendisse vitae dapibus lacus. Nam cursus in ante a porta. Curabitur lobortis, ex vitae posuere semper, dolor nisl vulputate eros, nec lacinia enim tortor vitae est. Maecenas tincidunt magna id est viverra pharetra. Ut id tellus scelerisque, tincidunt velit at, fringilla mi. Nam id elit viverra, rutrum orci eget, venenatis sapien. Curabitur blandit aliquet condimentum. Nunc eu metus nec tellus sollicitudin interdum. Fusce Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus lectus odio, tincidunt eget accumsan a, tempor quis risus. In hac habitasse platea dictumst. Ut varius urna sed mollis ultricies. Aliquam bibendum erat sit amet eros tincidunt efficitur. Vivamus ullamcorper tincidunt mauris id maximus. Integer a sapien pretium, viverra orci in, commodo elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur a lacus feugiat, consectetur mauris ut, malesuada erat. Fusce egestas, ante et gravida porttitor, turpis leo pretium tellus, ut lobortis diam turpis ac mauris. Sed sit amet neque dui. Etiam justo diam, gravida eu diam at, elementum rutrum est. Donec efficitur mattis quam, vitae venenatis massa pretium nec. Mauris rutrum id tellus sit amet aliquet. Praesent suscipit elementum lectus, eu malesuada lorem venenatis mollis. Vestibulum vitae malesuada turpis. Sed eleifend, erat iaculis lobortis tincidunt, massa eros ullamcorper risus, eget laoreet enim velit at orci. Nulla erat lacus, luctus eu augue sit amet, euismod lobortis ante. Aliquam volutpat placerat tristique. Pellentesque varius elit non risus molestie dapibus. Duis sed justo eget quam rutrum pharetra a nec mi. Nam sit amet libero dapibus odio hendrerit egestas. Aliquam erat volutpat. Suspendisse vitae dapibus lacus. Nam cursus in ante a porta. Curabitur lobortis, ex vitae posuere semper, dolor nisl vulputate eros, nec lacinia enim tortor vitae est. Maecenas tincidunt magna id est viverra pharetra. Ut id tellus scelerisque, tincidunt velit at, fringilla mi. Nam id elit viverra, rutrum orci eget, venenatis sapien. Curabitur blandit aliquet condimentum. Nunc eu metus nec tellus sollicitudin interdum. Fusce Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus lectus odio, tincidunt eget accumsan a, tempor quis risus. In hac habitasse platea dictumst. Ut varius urna sed mollis ultricies. Aliquam bibendum erat sit amet eros tincidunt efficitur. Vivamus ullamcorper tincidunt mauris id maximus. Integer a sapien pretium, viverra orci in, commodo elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur a lacus feugiat, consectetur mauris ut, malesuada erat. Fusce egestas, ante et gravida porttitor, turpis leo pretium tellus, ut lobortis diam turpis ac mauris. Sed sit amet neque dui. Etiam justo diam, gravida eu diam at, elementum rutrum est. Donec efficitur mattis quam, vitae venenatis massa pretium nec. Mauris rutrum id tellus sit amet aliquet. Praesent suscipit elementum lectus, eu malesuada lorem venenatis mollis. Vestibulum vitae malesuada turpis. Sed eleifend, erat iaculis lobortis tincidunt, massa eros ullamcorper risus, eget laoreet enim velit at orci. Nulla erat lacus, luctus eu augue sit amet, euismod lobortis ante. Aliquam volutpat placerat tristique. Pellentesque varius elit non risus molestie dapibus. Duis sed justo eget quam rutrum pharetra a nec mi. Nam sit amet libero dapibus odio hendrerit egestas. Aliquam erat volutpat. Suspendisse vitae dapibus lacus. Nam cursus in ante a porta. Curabitur lobortis, ex vitae posuere semper, dolor nisl vulputate eros, nec lacinia enim tortor vitae est. Maecenas tincidunt magna id est viverra pharetra. Ut id tellus scelerisque, tincidunt velit at, fringilla mi. Nam id elit viverra, rutrum orci eget, venenatis sapien. Curabitur blandit aliquet condimentum. Nunc eu metus nec tellus sollicitudin interdum. Fusce Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus lectus odio, tincidunt eget accumsan a, tempor quis risus. In hac habitasse platea dictumst. Ut varius urna sed mollis ultricies. Aliquam bibendum erat sit amet eros tincidunt efficitur. Vivamus ullamcorper tincidunt mauris id maximus. Integer a sapien pretium, viverra orci in, commodo elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur a lacus feugiat, consectetur mauris ut, malesuada erat. Fusce egestas, ante et gravida porttitor, turpis leo pretium tellus, ut lobortis diam turpis ac mauris. Sed sit amet neque dui. Etiam justo diam, gravida eu diam at, elementum rutrum est. Donec efficitur mattis quam, vitae venenatis massa pretium nec. Mauris rutrum id tellus sit amet aliquet. Praesent suscipit elementum lectus, eu malesuada lorem venenatis mollis. Vestibulum vitae malesuada turpis. Sed eleifend, erat iaculis lobortis tincidunt, massa eros ullamcorper risus, eget laoreet enim velit at orci. Nulla erat lacus, luctus eu augue sit amet, euismod lobortis ante. Aliquam volutpat placerat tristique. Pellentesque varius elit non risus molestie dapibus. Duis sed justo eget quam rutrum pharetra a nec mi. Nam sit amet libero dapibus odio hendrerit egestas. Aliquam erat volutpat. Suspendisse vitae dapibus lacus. Nam cursus in ante a porta. Curabitur lobortis, ex vitae posuere semper, dolor nisl vulputate eros, nec lacinia enim tortor vitae est. Maecenas tincidunt magna id est viverra pharetra. Ut id tellus scelerisque, tincidunt velit at, fringilla mi. Nam id elit viverra, rutrum orci eget, venenatis sapien. Curabitur blandit aliquet condimentum. Nunc eu metus nec tellus sollicitudin interdum. Fusce Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus lectus odio, tincidunt eget accumsan a, tempor quis risus. In hac habitasse platea dictumst. Ut varius urna sed mollis ultricies. Aliquam bibendum erat sit amet eros tincidunt efficitur. Vivamus ullamcorper tincidunt mauris id maximus. Integer a sapien pretium, viverra orci in, commodo elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur a lacus feugiat, consectetur mauris ut, malesuada erat. Fusce egestas, ante et gravida porttitor, turpis leo pretium tellus, ut lobortis diam turpis ac mauris. Sed sit amet neque dui. Etiam justo diam, gravida eu diam at, elementum rutrum est. Donec efficitur mattis quam, vitae venenatis massa pretium nec. Mauris rutrum id tellus sit amet aliquet. Praesent suscipit elementum lectus, eu malesuada lorem venenatis mollis. Vestibulum vitae malesuada turpis. Sed eleifend, erat iaculis lobortis tincidunt, massa eros ullamcorper risus, eget laoreet enim velit at orci. Nulla erat lacus, luctus eu augue sit amet, euismod lobortis ante. Aliquam volutpat placerat tristique. Pellentesque varius elit non risus molestie dapibus. Duis sed justo eget quam rutrum pharetra a nec mi. Nam sit amet libero dapibus odio hendrerit egestas. Aliquam erat volutpat. Suspendisse vitae dapibus lacus. Nam cursus in ante a porta. Curabitur lobortis, ex vitae posuere semper, dolor nisl vulputate eros, nec lacinia enim tortor vitae est. Maecenas tincidunt magna id est viverra pharetra. Ut id tellus scelerisque, tincidunt velit at, fringilla mi. Nam id elit viverra, rutrum orci eget, venenatis sapien. Curabitur blandit aliquet condimentum. Nunc eu metus nec tellus sollicitudin interdum. Fusce Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus lectus odio, tincidunt eget accumsan a, tempor quis risus. In hac habitasse platea dictumst. Ut varius urna sed mollis ultricies. Aliquam bibendum erat sit amet eros tincidunt efficitur. Vivamus ullamcorper tincidunt mauris id maximus. Integer a sapien pretium, viverra orci in, commodo elit. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur a lacus feugiat, consectetur mauris ut, malesuada erat. Fusce egestas, ante et gravida porttitor, turpis leo pretium tellus, ut lobortis diam turpis ac mauris. Sed sit amet neque dui. Etiam justo diam, gravida eu diam at, elementum rutrum est. Donec efficitur mattis quam, vitae venenatis massa pretium nec. Mauris rutrum id tellus sit amet aliquet. Praesent suscipit elementum lectus, eu malesuada lorem venenatis mollis. Vestibulum vitae malesuada turpis. Sed eleifend, erat iaculis lobortis tincidunt, massa eros ullamcorper risus, eget laoreet enim velit at orci. Nulla erat lacus, luctus eu augue sit amet, euismod lobortis ante. Aliquam volutpat placerat tristique. Pellentesque varius elit non risus molestie dapibus. Duis sed justo eget quam rutrum pharetra a nec mi. Nam sit amet libero dapibus odio hendrerit egestas. Aliquam erat volutpat. Suspendisse vitae dapibus lacus. Nam cursus in ante a porta. Curabitur lobortis, ex vitae posuere semper, dolor nisl vulputate eros, nec lacinia enim tortor vitae est. Maecenas tincidunt magna id est viverra pharetra. Ut id tellus scelerisque, tincidunt velit at, fringilla mi. Nam id elit viverra, rutrum orci eget, venenatis sapien. Curabitur blandit aliquet condimentum. Nunc eu metus nec tellus sollicitudin interdum. Fusce "

  in

  Irmin.update (t "t: Update 1.txt") ["root";"misc";"1.txt"] content1 >>= fun () ->
  Irmin.read_exn (t "t: Read 1.txt") ["root";"misc";"1.txt"] >>= fun file1 ->
  Printf.printf "Plain text: 1:%s \n%!" file1;
  Irmin.update (t "t: Update 2.txt") ["root";"misc";"2.txt"] "Hi! " >>= fun () ->
  Irmin.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "How are you ? " >>= fun () ->
  Irmin.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"] >>= fun file ->
  Printf.printf "I've just read: %s\n%!" file;
  Irmin.clone_force task (t "x: Cloning 't'") "test" >>= fun x ->
  Irmin.update (t "t: Update 3.txt") ["root";"misc";"3.txt"] "Hohoho" >>= fun () ->
  Irmin.update (x "x: Update 2.txt") ["root";"misc";"2.txt"] "HELP!" >>= fun () ->
  Irmin.merge_exn "t: Merge with 'x'" x ~into:t >>= fun () ->
  Irmin.read_exn (t "t: Read 2.txt") ["root";"misc";"2.txt"] >>= fun file2 ->
  Irmin.read_exn (t "t: Read 3.txt") ["root";"misc";"3.txt"] >>= fun file3 ->
  Printf.printf "I've just read: 2:%s 3:%s\n%!" file2 file3;
  return_unit

let () =
  Lwt_unix.run (main ())

	       
