(*
 * Copyright (c) 2022-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
open Common

let ( / ) = Filename.concat
let archive_v2_minimal = "test" / "irmin-pack" / "data" / "version_2_minimal"
let archive_v2_always = "test" / "irmin-pack" / "data" / "version_2_always"
let archive_v3_minimal = "test" / "irmin-pack" / "data" / "version_3_minimal"
let archive_v3_always = "test" / "irmin-pack" / "data" / "version_3_always"

let archive_v3_minimal_gced =
  "test" / "irmin-pack" / "data" / "version_3_minimal_gced"

let root_local_build = "_build" / "test-upgrade"

type pack_entry = {
  h : Schema.Hash.t;
  o : Int63.t;
  l : int;
  k : [ `b | `n | `c ];
}
(** [pack_entry]: hash / offset / length / kind *)

let e h o l k =
  let h =
    match Irmin.Type.(of_string Schema.Hash.t) h with
    | Error (`Msg s) -> Alcotest.failf "failed hash_of_string %s" s
    | Ok h -> h
  in
  let o = Int63.of_int o in
  { h; o; l; k }

(* Objects inserted during preload

   borphan | b01 <- n01 <- n0 <- c0 *)
let borphan = e "c9bfadf2d211aa6da8e2d00732628a0880b7ee98" 0 29 `b
let b01 = e "5368d2c2f4fc5521fe8e8acd17cdd7349aa8f753" 29 25 `b
let n01 = e "9b120e5019dcc6cd90b4d9c9826c9ebbebdc0023" 54 34 `n
let n0 = e "fe0084f902d55464e9e6dbd82fb60fcf058bb6b1" 88 34 `n
let c0 = e "22e159de13b427226e5901defd17f0c14e744205" 122 42 `c

(* Objects inserted during write1

   to n01 <-    to c0 <-
            \           \
        b1 <- n1 <------- c1 | borphan' *)
let b1 = e "7e83ca2a65d6f90a809c8570c6c905a941b87732" 164 24 `b
let n1 = e "2cc1191a4cfbf869c62da4649961455df6e6b424" 188 44 `n
let c1 = e "09468f13334d3120d8798e27a28d23baba628710" 232 51 `c
let borphan' = e "945bcf284cb6f4735eb8eb74553637b43fde996b" 283 30 `b

(* Objects inserted during write2

     to c1 <-
             \
   b2 <- n2 <- c2 *)
let b2 = e "32f28ea03b1b20126629d2ca63fc6665b0bbb604" 313 24 `b
let n2 = e "bbca871beaebb1b556e498a8e1ccae7817f5f4ff" 337 34 `n
let c2 = e "6d6c9fcf882f1473f5e2bd0cd4b475611c3a5b60" 371 51 `c

let pack_entries =
  [ n0; b1; borphan; c2; c1; b01; borphan'; n1; n2; n01; c0; b2 ]

let dict_entries =
  [ ("step-n01", 1); ("step-b01", 0); ("step-b1", 2); ("step-b2", 3) ]

let dict_entries = List.to_seq dict_entries |> Hashtbl.of_seq

let index_entries =
  List.map (fun e -> (e.h, e.o)) pack_entries |> List.to_seq |> Hashtbl.of_seq

let key_of_entry x =
  Irmin_pack_unix.Pack_key.v_direct ~offset:x.o ~length:x.l x.h

type start_mode = From_v2 | From_v3 | From_scratch | From_v3_c0_gced
[@@deriving irmin]

type setup = {
  indexing_strategy : [ `always | `minimal ];
  start_mode : start_mode;
  lru_size : int;
}
[@@deriving irmin ~pp]

type phase =
  | S1_before_start
  | S2_before_write
  | S3_before_gc
  | S4_before_write
  | S5_before_close
[@@deriving irmin ~pp]

(** A model is updated in conjunction with a store. Both should always reference
    the same entries *)
module Model = struct
  type t = {
    setup : setup;
    dict : (string, unit) Hashtbl.t;
    suffix : (Int63.t, unit) Hashtbl.t;
    index : (Schema.Hash.t, unit) Hashtbl.t;
  }

  let v setup =
    let dict = Hashtbl.create 5 in
    let suffix = Hashtbl.create 5 in
    let index = Hashtbl.create 5 in
    { setup; dict; suffix; index }

  let preload_dict t =
    Hashtbl.replace t.dict "step-b01" ();
    Hashtbl.replace t.dict "step-n01" ()

  let preload_suffix t =
    if t.setup.start_mode <> From_v3_c0_gced then
      Hashtbl.replace t.suffix borphan.o ();
    Hashtbl.replace t.suffix b01.o ();
    Hashtbl.replace t.suffix n01.o ();
    Hashtbl.replace t.suffix n0.o ();
    Hashtbl.replace t.suffix c0.o ()

  let preload_index t =
    if t.setup.indexing_strategy = `always then (
      Hashtbl.replace t.index borphan.h ();
      Hashtbl.replace t.index b01.h ();
      Hashtbl.replace t.index n01.h ();
      Hashtbl.replace t.index n0.h ());
    Hashtbl.replace t.index c0.h ()

  let preload t =
    preload_suffix t;
    preload_index t;
    preload_dict t

  let write1_dict t = Hashtbl.replace t.dict "step-b1" ()

  let write1_suffix t =
    Hashtbl.replace t.suffix b1.o ();
    Hashtbl.replace t.suffix n1.o ();
    Hashtbl.replace t.suffix c1.o ();
    Hashtbl.replace t.suffix borphan'.o ()

  let write1_index t =
    if t.setup.indexing_strategy = `always then (
      Hashtbl.replace t.index b1.h ();
      Hashtbl.replace t.index n1.h ();
      Hashtbl.replace t.index borphan'.h ());
    Hashtbl.replace t.index c1.h ()

  let write1 t =
    write1_suffix t;
    write1_index t;
    write1_dict t

  let gc t =
    Hashtbl.remove t.suffix borphan.o;
    Hashtbl.remove t.suffix n0.o;
    Hashtbl.remove t.suffix c0.o

  let write2_dict t = Hashtbl.replace t.dict "step-b2" ()

  let write2_suffix t =
    Hashtbl.replace t.suffix b2.o ();
    Hashtbl.replace t.suffix n2.o ();
    Hashtbl.replace t.suffix c2.o ()

  let write2_index t =
    if t.setup.indexing_strategy = `always then (
      Hashtbl.replace t.index b2.h ();
      Hashtbl.replace t.index n2.h ());
    Hashtbl.replace t.index c2.h ()

  let write2 t =
    write2_suffix t;
    write2_index t;
    write2_dict t

  (** The 5 different states in which a model may be *)
  include struct
    let create_empty setup = v setup

    let create_after_preload setup =
      let m = v setup in
      preload m;
      m

    let create_after_write1 setup =
      let m = v setup in
      preload m;
      write1 m;
      m

    let create_after_gc setup =
      let m = v setup in
      preload m;
      write1 m;
      gc m;
      m

    let create_after_write2 setup =
      let m = v setup in
      preload m;
      write1 m;
      gc m;
      write2 m;
      m
  end
end

(** A store is updated in conjunction with a model. Both should always reference
    the same entries *)
module Store = struct
  module S = struct
    module Maker = Irmin_pack_unix.Maker (Conf)
    include Maker.Make (Schema)
  end

  type repo = S.repo

  let config setup ?(readonly = false) ?(fresh = true) root =
    let module Index = Irmin_pack.Indexing_strategy in
    let indexing_strategy =
      if setup.indexing_strategy = `always then Index.always else Index.minimal
    in
    let lru_size = setup.lru_size in
    Irmin_pack.config ~readonly ~indexing_strategy ~lru_size ~fresh root

  let v setup ~readonly ~fresh root =
    S.Repo.v (config setup ~readonly ~fresh root)

  let close = S.Repo.close
  let reload = S.reload

  let gc repo =
    let k = key_of_entry c1 in
    let launched = S.Gc.start_exn ~unlink:true repo k in
    assert launched;
    let result = S.Gc.finalise_exn ~wait:true repo in
    match result with
    | `Idle | `Running -> Alcotest.fail "expected finalised gc"
    | `Finalised _ -> ()

  let dict_find_opt (repo : S.repo) step =
    S.Internal.(Dict.find (dict repo) step)

  let index_find_opt (repo : S.repo) hash =
    S.Internal.(Index.find (File_manager.index (file_manager repo)) hash)

  let suffix_mem (repo : S.repo) e =
    let k = key_of_entry e in
    try
      match e.k with
      | `c -> S.Backend.Commit.mem (S.Backend.Repo.commit_t repo) k
      | `n -> S.Backend.Node.mem (S.Backend.Repo.node_t repo) k
      | `b -> S.Backend.Contents.mem (S.Backend.Repo.contents_t repo) k
    with Irmin_pack_unix.Pack_store.Invalid_read _ ->
      (* In RW mode, [mem] will raise an exception if the offset of the key is
         out of the bounds of the pack file *)
      false

  let put_borphan bstore =
    let k = S.Backend.Contents.add bstore "borphan" in
    assert (k = key_of_entry borphan);
    k

  let put_b01 bstore =
    let k = S.Backend.Contents.add bstore "b01" in
    assert (k = key_of_entry b01);
    k

  let put_n01 bstore nstore =
    let k_b01 = put_b01 bstore in
    let step = "step-b01" in
    let childs = [ (step, `Contents (k_b01, ())) ] in
    let n = S.Backend.Node.Val.of_list childs in
    let k = S.Backend.Node.add nstore n in
    assert (k = key_of_entry n01);
    k

  let put_n0 bstore nstore =
    let k_n01 = put_n01 bstore nstore in
    let step = "step-n01" in
    let childs = [ (step, `Node k_n01) ] in
    let n = S.Backend.Node.Val.of_list childs in
    let k = S.Backend.Node.add nstore n in
    assert (k = key_of_entry n0);
    k

  let put_c0 bstore nstore cstore =
    let k_n0 = put_n0 bstore nstore in
    let c = S.Backend.Commit.Val.v ~info:S.Info.empty ~node:k_n0 ~parents:[] in
    let k = S.Backend.Commit.add cstore c in
    assert (k = key_of_entry c0);
    k

  let put_b1 bstore =
    let k = S.Backend.Contents.add bstore "b1" in
    k

  let put_n1 bstore nstore =
    let k_b1 = put_b1 bstore in
    let k_n01 = key_of_entry n01 in
    let step = "step-b1" in
    let step' = "step-b01" in
    let childs = [ (step, `Contents (k_b1, ())); (step', `Node k_n01) ] in
    let n = S.Backend.Node.Val.of_list childs in
    let k = S.Backend.Node.add nstore n in
    assert (k = key_of_entry n1);
    k

  let put_c1 bstore nstore cstore =
    let k_n1 = put_n1 bstore nstore in
    let k_c0 = key_of_entry c0 in
    let c =
      S.Backend.Commit.Val.v ~info:S.Info.empty ~node:k_n1 ~parents:[ k_c0 ]
    in
    let k = S.Backend.Commit.add cstore c in
    assert (k = key_of_entry c1);
    k

  let put_borphan' bstore =
    let k = S.Backend.Contents.add bstore "borphan'" in
    assert (k = key_of_entry borphan');
    k

  let put_b2 bstore =
    let k = S.Backend.Contents.add bstore "b2" in
    assert (k = key_of_entry b2);
    k

  let put_n2 bstore nstore =
    let k_b2 = put_b2 bstore in
    let step = "step-b2" in
    let childs = [ (step, `Contents (k_b2, ())) ] in
    let n = S.Backend.Node.Val.of_list childs in
    let k = S.Backend.Node.add nstore n in
    assert (k = key_of_entry n2);
    k

  let put_c2 bstore nstore cstore =
    let k_n2 = put_n2 bstore nstore in
    let k_c1 = key_of_entry c1 in
    let c =
      S.Backend.Commit.Val.v ~info:S.Info.empty ~node:k_n2 ~parents:[ k_c1 ]
    in
    let k = S.Backend.Commit.add cstore c in
    assert (k = key_of_entry c2);
    k

  let preload repo =
    S.Backend.Repo.batch repo (fun bstore nstore cstore ->
        let _ = put_borphan bstore in
        let _ = put_c0 bstore nstore cstore in
        Lwt.return_unit)

  let write1 repo =
    S.Backend.Repo.batch repo (fun bstore nstore cstore ->
        let _ = put_c1 bstore nstore cstore in
        let _ = put_borphan' bstore in
        Lwt.return_unit)

  let write2 repo =
    S.Backend.Repo.batch repo (fun bstore nstore cstore ->
        let _ = put_c2 bstore nstore cstore in
        Lwt.return_unit)
end

exception Skip_the_rest_of_that_test

type hash = Store.S.hash [@@deriving irmin ~pp]

type t = {
  setup : setup;
  mutable ro : (Model.t * Store.repo) option;
  mutable rw : (Model.t * Store.repo) option;
}

let check_dict repo model =
  Hashtbl.iter
    (fun step idx ->
      let got = Store.dict_find_opt repo idx in
      let exp = Hashtbl.mem model.Model.dict step in
      match (got, exp) with
      | None, false -> ()
      | Some step', true ->
          let msg = Fmt.str "Dict entry with id:%d" idx in
          Alcotest.(check string) msg step step'
      | Some step', false ->
          Alcotest.failf
            "Dict entry with id:%d step:%s shouldn't be there (it's under step \
             %s)"
            idx step step'
      | None, true ->
          Alcotest.failf "Dict entry with id:%d step:%s missing" idx step)
    dict_entries

let check_index repo model =
  Hashtbl.iter
    (fun hash off ->
      let got = Store.index_find_opt repo hash in
      let exp = Hashtbl.mem model.Model.index hash in
      match (got, exp) with
      | None, false -> ()
      | Some (off', _, _), true ->
          let msg = Fmt.str "Index entry with hash:%a" pp_hash hash in
          Alcotest.(check int) msg (Int63.to_int off) (Int63.to_int off')
      | Some (off', _, _), false ->
          Alcotest.failf
            "Index entry with hash:%a offset:%d shouldn't be there (it \
             contains offset %d)"
            pp_hash hash (Int63.to_int off) (Int63.to_int off')
      | None, true ->
          Alcotest.failf "Index entry with hash:%a off:%d is missing" pp_hash
            hash (Int63.to_int off))
    index_entries

let check_suffix repo model =
  List.iter
    (fun e ->
      let got = Store.suffix_mem repo e in
      let exp = Hashtbl.mem model.Model.suffix e.o in
      match (got, exp) with
      | false, false -> ()
      | true, true -> ()
      | true, false ->
          Alcotest.failf "Pack entry with hash:%a off:%d shouldn't be there"
            pp_hash e.h (Int63.to_int e.o)
      | false, true ->
          Alcotest.failf "Pack entry with hash:%a off:%d is missing" pp_hash e.h
            (Int63.to_int e.o))
    pack_entries

let check t =
  List.iter
    (fun (model, repo) ->
      check_dict repo model;
      check_index repo model;
      check_suffix repo model)
    (Option.to_list t.ro @ Option.to_list t.rw)

let create_test_env setup =
  rm_dir root_local_build;
  let () =
    match setup.start_mode with
    | From_scratch -> ()
    | From_v2 ->
        let root_archive =
          if setup.indexing_strategy = `always then archive_v2_always
          else archive_v2_minimal
        in
        setup_test_env ~root_archive ~root_local_build
    | From_v3 ->
        let root_archive =
          if setup.indexing_strategy = `always then archive_v3_always
          else archive_v3_minimal
        in
        setup_test_env ~root_archive ~root_local_build
    | From_v3_c0_gced ->
        let root_archive =
          if setup.indexing_strategy = `minimal then archive_v3_minimal_gced
          else assert false
        in
        setup_test_env ~root_archive ~root_local_build
  in

  { setup; rw = None; ro = None }

(** One of the 4 rw mutations *)
let start_rw ~sw t =
  [%logs.app "*** start_rw %a" pp_setup t.setup];
  let rw =
    match t.rw with
    | Some _ -> assert false
    | None ->
        let model =
          match t.setup.start_mode with
          | From_v2 | From_v3 | From_v3_c0_gced ->
              (* Model with pre-loaded data. *)
              let m = Model.v t.setup in
              Model.preload m;
              m
          | From_scratch -> Model.v t.setup
        in
        let repo =
          Store.v ~sw t.setup ~readonly:false ~fresh:false root_local_build
        in
        (model, repo)
  in
  t.rw <- Some rw

(** One of the 4 rw mutations *)
let write1_rw t =
  [%logs.app "*** write1_rw %a" pp_setup t.setup];
  match t.rw with
  | None -> assert false
  | Some (_, repo) ->
      t.rw <- Some (Model.create_after_write1 t.setup, repo);
      let () =
        (* If the preload commit is not yet in the store, add it. Note that
           adding the same commit twice is not idempotent in indexing strategy
           minimal, therefore we need to make this distinction. *)
        if t.setup.start_mode = From_scratch then
          let _ = Store.preload repo in
          ()
        else ()
      in
      let _ = Store.write1 repo in
      ()

(** One of the 4 rw mutations *)
let gc_rw t =
  [%logs.app "*** gc_rw %a" pp_setup t.setup];
  match t.rw with
  | None -> assert false
  | Some (_, repo) ->
      t.rw <- Some (Model.create_after_gc t.setup, repo);
      let () =
        match (t.setup.start_mode, t.setup.indexing_strategy) with
        | From_v2, _ | _, `always ->
            let () =
              Alcotest.check_raises "GC on V2/always"
                (Irmin_pack_unix.Errors.Pack_error
                   (`Gc_disallowed "Store does not support GC"))
                (fun () -> Store.gc repo)
            in
            raise Skip_the_rest_of_that_test
        | (From_v3 | From_scratch | From_v3_c0_gced), `minimal -> Store.gc repo
      in
      ()

(** One of the 4 rw mutations *)
let write2_rw t =
  [%logs.app "*** write2_rw %a" pp_setup t.setup];
  match t.rw with
  | None -> assert false
  | Some (_, repo) ->
      t.rw <- Some (Model.create_after_write2 t.setup, repo);
      let _ = Store.write2 repo in
      ()

(** One of the 2 ro mutations *)
let open_ro ~sw t current_phase =
  [%logs.app "*** open_ro %a, %a" pp_setup t.setup pp_phase current_phase];
  let ro =
    match t.ro with
    | Some _ -> assert false
    | None ->
        let model =
          match (t.setup.start_mode, current_phase) with
          | From_scratch, (S1_before_start | S2_before_write) ->
              Model.create_empty t.setup
          | ( (From_v2 | From_v3 | From_v3_c0_gced),
              (S1_before_start | S2_before_write) ) ->
              Model.create_after_preload t.setup
          | (From_v2 | From_v3 | From_v3_c0_gced | From_scratch), S3_before_gc
            ->
              Model.create_after_write1 t.setup
          | ( (From_v2 | From_v3 | From_v3_c0_gced | From_scratch),
              S4_before_write ) ->
              Model.create_after_gc t.setup
          | ( (From_v2 | From_v3 | From_v3_c0_gced | From_scratch),
              S5_before_close ) ->
              Model.create_after_write2 t.setup
        in
        let fail_and_skip error =
          let () =
            Alcotest.check_raises "open empty/V2 store in RO"
              (Irmin_pack_unix.Errors.Pack_error error) (fun () ->
                let repo =
                  Store.v ~sw t.setup ~readonly:true ~fresh:false
                    root_local_build
                in
                Store.close repo)
          in
          raise Skip_the_rest_of_that_test
        in
        let repo =
          match (t.setup.start_mode, current_phase) with
          | From_scratch, S1_before_start ->
              let missing_path =
                Irmin_pack.Layout.V1_and_v2.pack ~root:root_local_build
              in
              fail_and_skip (`No_such_file_or_directory missing_path)
          | From_v2, S1_before_start -> fail_and_skip `Migration_needed
          | (From_v2 | From_v3 | From_v3_c0_gced | From_scratch), _ ->
              Store.v ~sw t.setup ~readonly:true ~fresh:false root_local_build
        in
        (model, repo)
  in
  t.ro <- Some ro

(** One of the 2 ro mutations *)
let sync_ro t current_phase =
  [%logs.app "*** sync_ro %a, %a" pp_setup t.setup pp_phase current_phase];
  match t.ro with
  | None -> assert false
  | Some (_, repo) ->
      let () =
        match current_phase with
        | S1_before_start | S2_before_write -> ()
        | S3_before_gc -> t.ro <- Some (Model.create_after_write1 t.setup, repo)
        | S4_before_write -> t.ro <- Some (Model.create_after_gc t.setup, repo)
        | S5_before_close ->
            t.ro <- Some (Model.create_after_write2 t.setup, repo)
      in
      Store.reload repo

let close_everything t =
  List.iter
    (fun (_, repo) -> Store.close repo)
    (Option.to_list t.ro @ Option.to_list t.rw)

let test_one t ~ro_open_at ~ro_sync_at =
  Eio.Switch.run @@ fun sw ->
  let aux phase =
    let () = check t in
    let () = if ro_open_at = phase then open_ro ~sw t phase else () in
    let () = check t in
    if ro_sync_at = phase then sync_ro t phase;
    check t
  in

  let () = aux S1_before_start in
  let () = start_rw ~sw t in
  let () = aux S2_before_write in
  let () = write1_rw t in
  let () = aux S3_before_gc in
  let () = gc_rw t in
  let () = aux S4_before_write in
  let () = write2_rw t in
  aux S5_before_close

let test_one_guarded setup ~ro_open_at ~ro_sync_at =
  let t = create_test_env setup in
  try
    let () = test_one t ~ro_open_at ~ro_sync_at in
    close_everything t
  with
  | Skip_the_rest_of_that_test ->
      [%logs.app "*** skip rest of %a" pp_setup setup];
      close_everything t
  | exn -> raise exn

(** All possible interleaving of the ro calls (open and sync) with the rw calls
    (open, write1, gc and write2). *)
let test start_mode indexing_strategy lru_size =
  let setup = { start_mode; indexing_strategy; lru_size } in
  let t = test_one_guarded setup in

  let () = t ~ro_open_at:S1_before_start ~ro_sync_at:S1_before_start in
  let () = t ~ro_open_at:S1_before_start ~ro_sync_at:S2_before_write in
  let () = t ~ro_open_at:S1_before_start ~ro_sync_at:S3_before_gc in
  let () = t ~ro_open_at:S1_before_start ~ro_sync_at:S4_before_write in
  let () = t ~ro_open_at:S1_before_start ~ro_sync_at:S5_before_close in

  let () = t ~ro_open_at:S2_before_write ~ro_sync_at:S2_before_write in
  let () = t ~ro_open_at:S2_before_write ~ro_sync_at:S3_before_gc in
  let () = t ~ro_open_at:S2_before_write ~ro_sync_at:S4_before_write in
  let () = t ~ro_open_at:S2_before_write ~ro_sync_at:S5_before_close in

  let () = t ~ro_open_at:S3_before_gc ~ro_sync_at:S3_before_gc in
  let () = t ~ro_open_at:S3_before_gc ~ro_sync_at:S4_before_write in
  let () = t ~ro_open_at:S3_before_gc ~ro_sync_at:S5_before_close in

  let () = t ~ro_open_at:S4_before_write ~ro_sync_at:S4_before_write in
  let () = t ~ro_open_at:S4_before_write ~ro_sync_at:S5_before_close in

  let () = t ~ro_open_at:S5_before_close ~ro_sync_at:S5_before_close in
  ()

(** Product on lru_size *)
let test start_mode indexing_strategy =
  test start_mode indexing_strategy 0;
  test start_mode indexing_strategy 100

let test_gced_store () = test From_v3_c0_gced `minimal

(** Product on indexing_strategy *)
let test start_mode () =
  test start_mode `minimal;
  test start_mode `always

(** Product on start_mode *)
let tests =
  [
    Alcotest.test_case "upgrade From_v3" `Quick (test From_v3);
    Alcotest.test_case "upgrade From_v2" `Quick (test From_v2);
    Alcotest.test_case "upgrade From_scratch" `Quick (test From_scratch);
    Alcotest.test_case "upgrade From_v3 after Gc" `Quick test_gced_store;
  ]
