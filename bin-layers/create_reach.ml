(** A single binary to calculate reachability from a particular commit. This is a hack to
    work around various problems in the irmin-pack code, such as the caching of the block
    IOs. 

Call as: create_reach.exe <context path> <commit hash> <output file>

Invariant INV_CREATE_REACH_1: the first Pack_store_IO instance opened by this executable
is the one that needs to have reads logged in order to create the reachability data.
*)

let context_path,commit_hash_s,output_path = Sys.argv |> Array.to_list |> List.tl |> function
  | [context_path;commit_hash;output_path] -> context_path,commit_hash,output_path
  | _ -> failwith "Usage: create_reachability.exe <context path> <commit hash> <output path>"

let _ = assert(Sys.file_exists context_path)

(* Before doing anything else, we set a global variable which controls various other
   aspects of the code (see doc for [running_create_reach_exe]) *)
let _ = Irmin_pack_layers.running_create_reach_exe := Some output_path

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

let _ = S.Repo.iter
(*
?cache_size:int ->
min:S.Repo.elt list ->
max:S.Repo.elt list ->
?edge:(S.Repo.elt -> S.Repo.elt -> unit Lwt.t) ->
?branch:(string -> unit Lwt.t) ->
?commit:(S.commit_key -> unit Lwt.t) ->
?node:(S.node_key -> unit Lwt.t) ->
?contents:(S.contents_key -> unit Lwt.t) ->
?skip_branch:(string -> bool Lwt.t) ->
?skip_commit:(S.commit_key -> bool Lwt.t) ->
?skip_node:(S.node_key -> bool Lwt.t) ->
?skip_contents:(S.contents_key -> bool Lwt.t) ->
?pred_branch:(S.repo -> string -> S.Repo.elt list Lwt.t) ->
?pred_commit:(S.repo -> S.commit_key -> S.Repo.elt list Lwt.t) ->
?pred_node:(S.repo -> S.node_key -> S.Repo.elt list Lwt.t) ->
?pred_contents:(S.repo -> S.contents_key -> S.Repo.elt list Lwt.t) ->
?rev:bool -> S.repo -> unit Lwt.t
*)


(* We want to force Repo.iter to visit every node; we worry that there is more than one
   node with the same hash, so Repo.iter visits the first, then for the second it revisits
   the first *)
let flush_caches = fun _ -> 
  Irmin_pack.Pack_store.clear_all_caches();
  Lwt.return ()

(* NOTE cb abbrev. callback *)

(* let finish_cb () = flush_caches () *)

let finish_cb () = Lwt.return ()

let iter = 
  repo >>= fun repo -> 
  commit >>= fun commit -> 
  Printf.printf "Calling Repo.iter\n%!";
  S.Commit.key commit |> fun commit_key ->
  (* Repo.iter takes callbacks for each particular type of object; the callbacks typically
     take a key; we want to ensure that each particular object is at least read; so for
     each callback we use the key to pull the full object *)
  let commit_cb = fun ck -> 
    S.Commit.of_key repo ck >>= function
    | None -> failwith ""
    | Some _commit -> finish_cb ()
  in
  let contents_cb = fun ck -> 
    S.Contents.of_key repo ck >>= function
    | None -> failwith ""
    | Some _contents -> finish_cb ()
  in
  let node_cb = fun nk -> 
    S.Tree.of_key repo (`Node nk) >>= function
    | None -> failwith ""
    | Some _tree -> finish_cb()
  in
  S.Repo.iter
    ~cache_size:0
    ~min:[`Commit commit_key] ~max:[`Commit commit_key] 
    ~edge:(fun _e1 _e2 -> Lwt.return ())
    ~branch:(fun s -> ignore s; Lwt.return ())
    ~commit:commit_cb
    ~node:node_cb
    ~contents:contents_cb
    repo

let _ = Lwt_main.run iter

(* pack store reads are logged to the output file; we don't have a handle on the
   [out_channel], so we can't directly close the channel; likely the channel is flushed
   and closed on termination anway, but just to make sure, we flush all out channels at
   this point, just before termination *)
let _ = Stdlib.flush_all ()
