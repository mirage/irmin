(** A single binary to calculate reachability from a particular commit. This is a hack to
    work around various problems in the irmin-pack code, such as the caching of the block
    IOs. 

Call as: IRMIN_PACK_LOG_READS=<output_file> create_reachability.exe <context_path> <commit_hash>
*)

let context_path,commit_hash_s = Sys.argv |> Array.to_list |> List.tl |> function
  | [context_path;commit_hash] -> context_path,commit_hash
  | _ -> failwith "Usage: create_reachability.exe <context_path> <commit_hash>"

let _ = assert(Sys.file_exists context_path)

open Lwt.Infix

module S = Irmin_tezos.Store

let fn = context_path

let config = Irmin_pack.config ~readonly:true fn

let repo = 
  Printf.printf "Opening repo %s\n%!" fn;
  S.Repo.v config

let Ok hash = Irmin.Type.of_string S.hash_t commit_hash_s[@@warning "-8"]

let commit = 
  repo >>= fun repo -> 
  Printf.printf "Found repository at %s\n" fn;
  S.Commit.of_hash repo hash >>= function
  | Some c -> 
    Printf.printf "Found commit %s\n" commit_hash_s;
    Lwt.return c[@@warning "-8"]

let iter = 
  repo >>= fun repo -> 
  commit >>= fun commit -> 
  Printf.printf "Calling Repo.iter\n%!";
  S.Commit.key commit |> fun commit_key ->
  S.Repo.iter 
    ~min:[`Commit commit_key] ~max:[`Commit commit_key] 
    repo

let _ = Lwt_main.run iter
