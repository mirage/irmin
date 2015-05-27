

(*
  Security implementation for Irmin
  Encryption of blobs content
 *)



(* Crypo Library *)
open Nocrypto
open Nocrypto.Uncommon

(* Lwt library *)
open Lwt

(* Log *)
module Log = Log.Make(struct let section = "SECURITY" end)


module Security = struct

    (* Exceptions *)
    exception Algorithms_Not_Found

    (* Recovery method *)
    type key_method =
      | File
      | Static
      | OS
      | Void

    (* Add others algorithms if we need them *)
    type algorithms =
      | AES_128
      | AES_192
      | AES_256
      | Void

    (* Return the size of key *)
    let get_key_size = function
      | AES_128 -> 128
      | AES_192 -> 192
      | AES_256 -> 256
      | _ -> raise Algorithms_Not_Found



    let init ~crypto_algo:algorithms ~way:key_method =



    module Recuperation_Key = struct

        let get algo path =
          try
            lwt in_channel = open_file ?buffer_size: (key_size algo) ?flags:Unix.O_RDONLY ~mode:input path in
            lwt key = read_line in
            lwt () = close in_channel in
            (Some key)
            with
            | Algorithms_Not_Found ->
               Log.error "Algorithms not found";
               None
            | Unix.Unix_error ->
               Log.error "File containing the Master Key not found";
               None
      end;;


    module Cipher = struct

        include Cipher_block

        (* !!!! The master key; must be hidden !!!!!! *)
        let mutable security_key = None ;;

        (* Master key initialisation *)
        let init_key ?(path_key = "master_key") key =
          match key with
          | Some x ->
             security_key <- (Some x)
          | None ->
             let k = Recuperation_Key.get path_key in
             match k with
             | Some x -> security_key <- (Some x)
             | None -> Log.error "Initialisation failed";

        (* Encrypt blob *)
        let encrypt_blob blob_key blob_content =
          match security_key with
          | Some k ->
          | None ->


    (* Decrypt blob *)
      end;;


  end;;
