open Irmin_pack_layers

open Util

module Type = Irmin.Type

open Lwt.Infix

let _ = Util.Binary_search.test_2()


module Calculate_reads_from_hangzu_context() = struct

  (** {1 Irmin-pack} *)

  (* We now try the same experiments, using irmin-pack *)

  (* First, manually copy a store test/irmin-pack/data/version_1 to /tmp *)

  let context_path = "/tmp/l/github/scratch/hangzu_context/context"

  let _ = assert(Sys.file_exists context_path)

  (* see test_pack_version_bump.ml *)


  module S = Irmin_tezos.Store



  let fn = context_path

  let config = Irmin_pack.config ~readonly:true fn

  let repo = S.Repo.v config

  (* NOTE we redefine, because the Repo.t type is different *)
  let print_repo repo : unit Lwt.t = 
    repo >>= fun repo -> 
    S.Repo.heads repo >>= fun cs -> 
    P.p "Repo heads: ";
    begin
      List.iter (fun c -> 
          let pp = S.Commit.pp_hash in
          F.p "(Commit hash: %a) %!" pp c) cs 
    end;
    P.p "\n";
    Lwt.return ()

  let go = print_repo repo

(*
Repo heads: (Commit hash: CoV8SQumiVU9saiu3FVNeDNewJaJH8yWdsGF3WLdsRr2P9S7MzCj) 
*)

  (* NOTE Tezos does not use branches; fortunately, for hangzu context, nico has provided
     hash commit info *)

  let recent_commit_s = "CoWMUSFj7gp4LngpAhaZa62xPYZcKWMyr4Wnh14CcyyQWsPrghLx"

  let Ok hash = Type.of_string S.hash_t recent_commit_s[@@warning "-8"]

  let commit = 
    repo >>= fun repo -> 
    S.Commit.of_hash repo hash >>= function
    | Some c -> Lwt.return c[@@warning "-8"]

  let _ = 
    repo >>= fun repo ->
    commit >>= fun c ->
    F.p "Got commit: %a\n%!" (Type.pp_dump (S.commit_t repo)) c;
    Lwt.return ()
(*
Got head commit: { key =
                    {"Direct":["CoWMUSFj7gp4LngpAhaZa62xPYZcKWMyr4Wnh14CcyyQWsPrghLx",54882865952,110]};
                   value =
                    {"node":{"Direct":["CoW8djDQn8FT59NzALwenpWpvftxbPtntd78KBLs7x7u11GjRPrk",54882865806,146]},"parents":[{"Direct":["CoV99SpD6zCGzWJa99ZZDdNnWM3uYZgk3fNpJuRJFex7XVH12YEt",54882459494,110]}],"info":{"date":1642970640,"author":"Tezos","message":"lvl 2056193, fit 1:1400833, prio 0, 93 ops"}} }
*)


  module Pk = Irmin_pack.Pack_key

  let _ = 
    P.p "Using Repo.iter\n%!";
    repo >>= fun repo -> 
    commit >>= fun commit -> 
    S.Commit.key commit |> fun commit_key ->
    S.Repo.iter 
      ~min:[`Commit commit_key] ~max:[`Commit commit_key] 
      repo


  let _ = Lwt_main.run go
end

let hangzu_sorted_s = "hangzu.sorted"

let working_dir = "/tmp/l/github/scratch/"    

module Do_sort() = struct
  (** Process the hangzu (off,len) data *)

  let path = "/tmp/l/github/scratch/hangzu_reads.log"

  let _ = assert(Sys.file_exists path)

  let src = Int_mmap.open_ ~fn:path ~sz:(-1)

  let dst = Int_mmap.create ~fn:Fn.(working_dir / hangzu_sorted_s) ~sz:(BA1.dim src.arr)

  let _ = 
    Printf.printf "Sorting...\n%!";
    let chunk_sz = 1_000_000 / 16 in
    External_sort.sort ~chunk_sz ~src:src.arr ~dst:dst.arr;
    Int_mmap.close src; 
    Int_mmap.close dst;
    ()

end

(** Calc extents *)

let hangzu_extents_s = "hangzu.extents"

module Calc_extents() = struct
  let src = Int_mmap.open_ ~fn:Fn.(working_dir / hangzu_sorted_s) ~sz:(-1)

  let dst = open_out_bin Fn.(working_dir / hangzu_extents_s)

  let _ = 
    Printf.printf "Calculating extents...\n%!";
    External_sort.calculate_extents_oc ~src_is_sorted:() ~gap_tolerance:0 ~src:src.arr ~dst;
    Int_mmap.close src;
    close_out_noerr dst;
    ()
end

module Calc_extents_with_tolerance(S:sig val gap_tolerance:int end) = struct
  open S

  let src = Int_mmap.open_ ~fn:Fn.(working_dir / hangzu_sorted_s) ~sz:(-1)

  let name = hangzu_extents_s ^ "."^(string_of_int gap_tolerance)

  let dst = open_out_bin Fn.(working_dir / name)

  let _ = 
    Printf.printf "Calculating extents...\n%!";
    External_sort.calculate_extents_oc ~src_is_sorted:() ~gap_tolerance ~src:src.arr ~dst;
    Int_mmap.close src;
    close_out_noerr dst;
    ()

  let size_of_extents_file = Unix.(stat Fn.(working_dir/name) |> fun x -> x.st_size)

  let src = Int_mmap.open_ ~fn:Fn.(working_dir / name) ~sz:(-1)

  (* calculate total size of live data *)
  let _ = 
    let tot = 
      (0,0) |> iter_k (fun ~k (i,tot) -> 
          match i< BA1.dim src.arr with
          | true -> 
            let _off,len = src.arr.{ i },src.arr.{ i+1 } in
            k (i+2,tot+len)
          | false -> tot)
    in
    Printf.printf "Gap tolerance: %d; Size of extents file: %d; Total bytes: %d\n%!" gap_tolerance size_of_extents_file tot

(*
  Gap tolerance: 0; Size of extents file: 172229600; Total bytes: 2095322806
 Gap tolerance: 500; Size of extents file: 47881840; Total bytes: 2688118086
Gap tolerance: 1000; Size of extents file: 39372048; Total bytes: 3072243543
Gap tolerance: 2000; Size of extents file: 30281104; Total bytes: 3831181176
*)

end


(* module _ = Calc_extents_with_tolerance(struct let gap_tolerance=0 end) *)


(** Calculate some statistics *)

module Do_stats() = struct
  (* Useful stats: length distribution; distribution of gaps between regions *)

  let src = Int_mmap.open_ ~fn:Fn.(working_dir / hangzu_extents_s ^ ".2") ~sz:(-1)

  (* gaps between regions *)

  let hangzu_gaps_s = "hangzu.gaps"
(*
let _ = 
  let tbl = Hashtbl.create 1000 in
  0 |> iter_k (fun ~k i -> 
      match i+2 >= BA1.dim src.arr with
      | true -> ()
      | false -> 
        let off,len = src.arr.{ i },src.arr.{ i+1 } in
        let gap = src.arr.{i+2} - (off+len) in
        (Hashtbl.find_opt tbl gap |> function
          | None -> Hashtbl.replace tbl gap 1
          | Some n -> Hashtbl.replace tbl gap (n+1));
        k (i+2));
  tbl |> Hashtbl.to_seq |> List.of_seq |> List.sort Stdlib.compare |> fun xs -> 
  let out = open_out_bin Fn.(working_dir / hangzu_gaps_s) in
  xs |> List.iter (fun (gap,n) -> Printf.fprintf out "%d,%d\n%!" gap n);
  close_out_noerr out;
  ()
*)

(*
(* another version, where we use a coarser resolution - divide gap by 1000 *)
let _ = 
  let tbl = Hashtbl.create 1000 in
  0 |> iter_k (fun ~k i -> 
      match i+2 >= BA1.dim src.arr with
      | true -> ()
      | false -> 
        let off,len = src.arr.{ i },src.arr.{ i+1 } in
        let gap = (src.arr.{i+2} - (off+len)) / 1000 in
        (Hashtbl.find_opt tbl gap |> function
          | None -> Hashtbl.replace tbl gap 1
          | Some n -> Hashtbl.replace tbl gap (n+1));
        k (i+2));
  tbl |> Hashtbl.to_seq |> List.of_seq |> List.sort Stdlib.compare |> fun xs -> 
  let out = open_out_bin Fn.(working_dir / hangzu_gaps_s ^".2") in
  xs |> List.iter (fun (gap,n) -> Printf.fprintf out "%d,%d\n%!" gap n);
  close_out_noerr out;
  ()

(* first few entries: 

0	8303214
1	568366
2	309916
3	193756
4	143104
5	121298
6	93624
7	78714
8	67299
9	58082

ie the bin with the most entries is for 0-999 byte gaps
*)
*)

(*
  (* another version, where we use a coarser resolution - divide gap by 100 *)
  let _ = 
    let tbl = Hashtbl.create 1000 in
    0 |> iter_k (fun ~k i -> 
        match i+2 >= BA1.dim src.arr with
        | true -> ()
        | false -> 
          let off,len = src.arr.{ i },src.arr.{ i+1 } in
          let gap = (src.arr.{i+2} - (off+len)) / 100 in
          (Hashtbl.find_opt tbl gap |> function
            | None -> Hashtbl.replace tbl gap 1
            | Some n -> Hashtbl.replace tbl gap (n+1));
          k (i+2));
    tbl |> Hashtbl.to_seq |> List.of_seq |> List.sort Stdlib.compare |> fun xs -> 
    let out = open_out_bin Fn.(working_dir / hangzu_gaps_s ^".3") in
    xs |> List.iter (fun (gap,n) -> Printf.fprintf out "%d,%d\n%!" gap n);
    close_out_noerr out;
    ()
(*
First few entries:
00,6292371
01,872058
02,230580
03,277760
04,98692
05,76297
06,124681
07,206451
08,79800
09,44524
10,168635
11,102774
12,63123
13,37471
14,36132
15,40277
16,30442
17,28751
18,28081
19,32680
20,29937

ie 6M regions have a gap to next region of <100 

*)
*)



  (* calculate total size of live data *)
  let _ = 
    let tot = 
      (0,0) |> iter_k (fun ~k (i,tot) -> 
          match i< BA1.dim src.arr with
          | true -> 
            let _off,len = src.arr.{ i },src.arr.{ i+1 } in
            k (i+2,tot+len)
          | false -> tot)
    in
    Printf.printf "Total bytes: %d\n%!" tot

  (* Total bytes: 2095322806 (ie 2GB) 

  *)

end


