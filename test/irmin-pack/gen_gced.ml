(* Generator for version_3_minimal_gced test data *)
(*
   To regenerate the test data:
   1. dune exec test/irmin-pack/gen_gced.exe
   2. rm -rf test/irmin-pack/data/version_3_minimal_gced
   3. mv version_3_minimal_gced_new test/irmin-pack/data/version_3_minimal_gced
*)

let rm_dir data_dir =
  if Sys.file_exists data_dir then
    let cmd = Printf.sprintf "rm -rf %s" data_dir in
    let _ = Sys.command cmd in
    ()

(* Use the same Conf and Schema as the tests in Common.ml *)
module Conf = Irmin_tezos.Conf

module Schema = struct
  open Irmin
  module Metadata = Metadata.None
  module Contents = Contents.String_v2
  module Path = Path.String_list
  module Branch = Branch.String
  module Hash = Hash.SHA1
  module Node = Node.Generic_key.Make_v2 (Hash) (Path) (Metadata)
  module Commit = Commit.Generic_key.Make_v2 (Hash)
  module Info = Info.Default
end

module Store = struct
  module Maker = Irmin_pack_unix.Maker (Conf)
  include Maker.Make (Schema)
end

let config ~sw ~fs root =
  Irmin_pack.config ~sw ~fs
    ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal ~readonly:false
    ~fresh:true root

let info = Store.Info.empty

let generate ~domain_mgr ~sw ~fs =
  let path = "version_3_minimal_gced_new" in
  rm_dir path;
  let rw = Store.Repo.v (config ~sw ~fs Eio.Path.(fs / path)) in

  (* Create tree matching the original structure:
     borphan | b01 <- n01 <- n0 <- c0
     Where step-n01 and step-b01 are the path steps

     borphan is orphan content that is added to the content store but
     NOT part of the tree. The tree only has step-n01/step-b01 -> "b01" *)
  let k_c0 =
    Store.Backend.Repo.batch rw (fun bstore nstore cstore ->
        (* First, add the orphan content directly to the content store *)
        let _ = Store.Backend.Contents.add bstore "borphan" in

        (* Add b01 content *)
        let k_b01 = Store.Backend.Contents.add bstore "b01" in

        (* Create n01 node with step-b01 -> b01 *)
        let n01 =
          Store.Backend.Node.Val.of_list [ ("step-b01", `Contents (k_b01, ())) ]
        in
        let k_n01 = Store.Backend.Node.add nstore n01 in

        (* Create n0 (root) node with step-n01 -> n01 *)
        let n0 = Store.Backend.Node.Val.of_list [ ("step-n01", `Node k_n01) ] in
        let k_n0 = Store.Backend.Node.add nstore n0 in

        (* Create commit *)
        let c0 = Store.Backend.Commit.Val.v ~parents:[] ~info ~node:k_n0 in
        let k_c0 = Store.Backend.Commit.add cstore c0 in

        Printf.printf "Commit key type: %s\n"
          (Irmin.Type.to_string Store.Backend.Commit.Key.t k_c0);
        let hash = Store.Backend.Commit.Key.to_hash k_c0 in
        Printf.printf "Commit hash (type): %s\n"
          (Irmin.Type.to_string Store.Hash.t hash);
        let hash_bytes =
          Irmin.Type.(unstage (to_bin_string Store.Hash.t)) hash
        in
        let hex =
          String.to_seq hash_bytes
          |> Seq.map (fun c -> Printf.sprintf "%02x" (Char.code c))
          |> List.of_seq
          |> String.concat ""
        in
        Printf.printf "Commit hash (hex): %s\n" hex;
        k_c0)
  in

  (* Run GC with c0 as target to remove borphan *)
  Printf.printf "Running GC with c0 as target...\n";
  let launched = Store.Gc.start_exn ~domain_mgr ~unlink:true rw k_c0 in
  assert launched;
  let result = Store.Gc.finalise_exn ~wait:true rw in
  (match result with
  | `Idle | `Running -> failwith "expected finalised gc"
  | `Finalised _ -> Printf.printf "GC completed\n");

  Store.Repo.close rw;
  Printf.printf "Store created at: %s\n" path

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.cwd env in
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  generate ~domain_mgr ~sw ~fs
