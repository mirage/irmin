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
let test_dir = "_build" / "test-upgrade"
let root_v2_minimal = "test" / "irmin-pack" / "data" / "version_2_minimal"
let root_v2_always = "test" / "irmin-pack" / "data" / "version_2_always"
let store_v2 = "_build" / "test-upgrade-v2"
let store_v3 = "_build" / "test-upgrade-v3"

let hash_of_string hash =
  match Irmin.Type.(of_string Schema.Hash.t) hash with
  | Error (`Msg s) -> Alcotest.failf "failed hash_of_string %s" s
  | Ok h -> h

module Test (I : sig
  val indexing_strategy : string
end) =
struct
  module Model = struct
    type offset = Int63.t
    type len = int

    type t = {
      dict : (string, int) Hashtbl.t;
      suffix : (offset, Schema.Hash.t * len) Hashtbl.t;
      index :
        (Schema.Hash.t, offset * len * Irmin_pack.Pack_value.Kind.t) Hashtbl.t;
    }

    let v () =
      let dict = Hashtbl.create 5 in
      let suffix = Hashtbl.create 5 in
      let index = Hashtbl.create 5 in
      { dict; suffix; index }

    let add_entry_suffix t off (hash, len) =
      let hash = hash_of_string hash in
      Hashtbl.replace t.suffix (Int63.of_int off) (hash, len)

    let preload_suffix t =
      add_entry_suffix t 0 ("11f6ad8ec52a2984abaafd7c3b516503785c2072", 23);
      add_entry_suffix t 23 ("d8fe4f428588313ef0a1629bb61b4ab4c27bd518", 34);
      add_entry_suffix t 57 ("a89e8c7a52eaf185b88acc0435b75d69f510c9c8", 34);
      add_entry_suffix t 91 ("94919d4ecc64db71f19ea134e0eedf675512453c", 42)

    let add_entry_index t hash (off, len, kind) =
      let hash = hash_of_string hash in
      let kind = Irmin_pack.Pack_value.Kind.of_magic_exn kind in
      Hashtbl.replace t.index hash (Int63.of_int off, len, kind)

    let preload_index t =
      if I.indexing_strategy = "always" then (
        add_entry_index t "11f6ad8ec52a2984abaafd7c3b516503785c2072" (0, 23, 'B');
        add_entry_index t "d8fe4f428588313ef0a1629bb61b4ab4c27bd518"
          (23, 34, 'R');
        add_entry_index t "a89e8c7a52eaf185b88acc0435b75d69f510c9c8"
          (57, 34, 'R'))
      else ();
      add_entry_index t "94919d4ecc64db71f19ea134e0eedf675512453c" (91, 42, 'D')

    let preload_dict t =
      Hashtbl.replace t.dict "abab" 0;
      Hashtbl.replace t.dict "abba" 1

    let preload t =
      preload_suffix t;
      preload_index t;
      preload_dict t

    let add_suffix t =
      add_entry_suffix t 133 ("95cb0bfd2977c761298d9624e4b4d4c72a39974a", 23);
      add_entry_suffix t 156 ("d9962241e991b771359d8278e79004a334088fde", 44);
      add_entry_suffix t 200 ("8aa02b8a95077bffe8672222ef69c8967e23ed49", 42)

    let add_index t =
      if I.indexing_strategy = "always" then (
        add_entry_index t "95cb0bfd2977c761298d9624e4b4d4c72a39974a"
          (133, 23, 'B');
        add_entry_index t "d9962241e991b771359d8278e79004a334088fde"
          (156, 44, 'R'))
      else ();
      add_entry_index t "8aa02b8a95077bffe8672222ef69c8967e23ed49" (200, 42, 'D')

    let add_dict t = Hashtbl.add t.dict "baba" 2

    let add t =
      add_suffix t;
      add_index t;
      add_dict t
  end

  module Store = struct
    module S = struct
      module Maker = Irmin_pack_unix.Maker (Conf)
      include Maker.Make (Schema)
    end

    type repo = S.repo

    let config ?(readonly = false) ?(fresh = true) root =
      let module Index = Irmin_pack.Indexing_strategy in
      let indexing_strategy =
        if I.indexing_strategy = "always" then Index.always else Index.minimal
      in
      Irmin_pack.config ~readonly ~indexing_strategy ~lru_size:0 ~fresh root

    let v ~readonly ~fresh root = S.Repo.v (config ~readonly ~fresh root)
    let close = S.Repo.close
    let reload = S.reload

    let preload_commit repo =
      let tree = S.Tree.empty () in
      let* tree = S.Tree.add tree [ "abba"; "abab" ] "x" in
      let* commit = S.Commit.v repo ~info:S.Info.empty ~parents:[] tree in
      let commit_key = S.Commit.key commit in
      Lwt.return commit_key

    let commit repo =
      let* o =
        "94919d4ecc64db71f19ea134e0eedf675512453c"
        |> hash_of_string
        |> S.Commit.of_hash repo
      in
      match o with
      | None -> assert false
      | Some commit ->
          let tree = S.Commit.tree commit in
          let* tree = S.Tree.add tree [ "baba" ] "y" in
          S.Commit.v repo ~info:S.Info.empty ~parents:[] tree

    let pp_hash = Irmin.Type.pp Schema.Hash.t

    let decode_entry_prefix ~off fm =
      let module CA = S.X.Contents.CA in
      let entry = CA.read_and_decode_entry_prefix ~off fm in
      let len = CA.Entry_prefix.total_entry_length entry |> Option.get in
      (entry.hash, len)

    let dict_find (repo : S.repo) step =
      let dict = repo.dict in
      S.Dict.index dict step |> Option.get

    let index_find (repo : S.repo) hash =
      let index = S.File_manager.index repo.fm in
      S.Index.find index hash |> Option.get

    let suffix_find (repo : S.repo) off = decode_entry_prefix ~off repo.fm

    let index_iter (repo : S.repo) f =
      let index = S.File_manager.index repo.fm in
      S.Index.iter f index
  end

  exception Skip_the_rest_of_that_test

  let fail_and_skip root error =
    let* () =
      Alcotest.check_raises_lwt "open empty/V2 store in RO"
        (Irmin_pack_unix.Errors.Pack_error error) (fun () ->
          let* repo = Store.v ~readonly:true ~fresh:false root in
          Store.close repo)
    in
    raise Skip_the_rest_of_that_test

  (* Only checking injection and not bijection. *)
  let test_dict repo model =
    Hashtbl.iter
      (fun step expected ->
        let got = Store.dict_find repo step in
        let msg = "---- dict " ^ step in
        Alcotest.(check int) msg expected got)
      model.Model.dict

  let test_index repo model =
    Hashtbl.iter
      (fun hash expected ->
        let got = Store.index_find repo hash in
        let msg = Fmt.str "---- M index %a" Store.pp_hash hash in
        Alcotest.(check (triple int63 int kind)) msg expected got)
      model.Model.index;
    Store.index_iter repo (fun hash expected ->
        let got = Hashtbl.find model.Model.index hash in
        let msg = Fmt.str "---- S index %a" Store.pp_hash hash in
        Alcotest.(check (triple int63 int kind)) msg expected got)

  (* Only checking injection and not bijection. *)
  let test_suffix repo model =
    Hashtbl.iter
      (fun off expected ->
        let got = Store.suffix_find repo off in
        let msg = Fmt.str "---- suffix %a" Int63.pp off in
        Alcotest.(check (pair hash int)) msg expected got)
      model.Model.suffix

  type mode = From_v2 | From_v3 | From_scratch [@@deriving irmin ~pp]

  type phase = Before_rw_open | Between_rw_open_and_write | After_rw_write
  [@@deriving irmin ~pp]

  type t = {
    start_mode : mode;
    mutable ro : (Model.t * Store.repo) option;
    mutable rw : (Model.t * Store.repo) option;
  }

  let setup_test_env root_v2 =
    setup_test_env ~root_archive:root_v2 ~root_local_build:store_v2

  let create_test_env start_mode =
    rm_dir test_dir;
    rm_dir store_v3;
    match start_mode with
    | From_scratch -> Lwt.return { start_mode; rw = None; ro = None }
    | From_v3 ->
        let* repo = Store.v ~readonly:false ~fresh:true store_v3 in
        let* _ = Store.preload_commit repo in
        let* () = Store.close repo in
        Lwt.return { start_mode; rw = None; ro = None }
    | From_v2 ->
        let root_v2 =
          if I.indexing_strategy = "always" then root_v2_always
          else root_v2_minimal
        in
        setup_test_env root_v2;
        Lwt.return { start_mode; rw = None; ro = None }

  let start_rw t =
    [%logs.debug "*** start_rw start_mode = %a" pp_mode t.start_mode];
    let+ rw =
      match t.rw with
      | Some _ -> assert false
      | None ->
          let model =
            match t.start_mode with
            | From_v2 | From_v3 ->
                (* Model with pre-loaded data. *)
                let m = Model.v () in
                Model.preload m;
                m
            | From_scratch -> Model.v ()
          in
          let+ repo =
            match t.start_mode with
            | From_v2 -> Store.v ~readonly:false ~fresh:false store_v2
            | From_v3 -> Store.v ~readonly:false ~fresh:false store_v3
            | From_scratch -> Store.v ~readonly:false ~fresh:true test_dir
          in
          (model, repo)
    in
    t.rw <- Some rw

  let write_rw t =
    [%logs.debug "*** write_rw start_mode = %a" pp_mode t.start_mode];
    match t.rw with
    | None -> assert false
    | Some (model, repo) ->
        (* Put exactly the same thing in the repo and in the model. *)
        Model.preload model;
        Model.add model;
        let* () =
          (* If the preload commit is not yet in the store, add it. Note that
             adding the same commit twice is not idempotent in indexing strategy
             minimal, therefore we need to make this distinction. *)
          if t.start_mode = From_scratch then
            let* _ = Store.preload_commit repo in
            Lwt.return_unit
          else Lwt.return_unit
        in
        let* _ = Store.commit repo in
        Lwt.return_unit

  let open_ro t current_phase =
    [%logs.debug
      "*** open_ro current_phase = %a, start_mode = %a" pp_phase current_phase
        pp_mode t.start_mode];
    let+ ro =
      match t.ro with
      | Some _ -> assert false
      | None ->
          let model =
            match (current_phase, t.start_mode) with
            | Before_rw_open, From_scratch
            | Between_rw_open_and_write, From_scratch ->
                Model.v ()
            | Before_rw_open, _ | Between_rw_open_and_write, _ ->
                let m = Model.v () in
                Model.preload m;
                m
            | After_rw_write, _ ->
                let m = Model.v () in
                Model.preload m;
                Model.add m;
                m
          in
          let+ repo =
            match (current_phase, t.start_mode) with
            | Before_rw_open, From_scratch ->
                fail_and_skip test_dir `No_such_file_or_directory
            | Before_rw_open, From_v2 ->
                fail_and_skip store_v2 `Migration_needed
            | _, From_v2 -> Store.v ~readonly:true ~fresh:false store_v2
            | _, From_v3 -> Store.v ~readonly:true ~fresh:false store_v3
            | _, From_scratch -> Store.v ~readonly:true ~fresh:false test_dir
          in
          (model, repo)
    in
    t.ro <- Some ro

  let sync_ro t current_phase =
    [%logs.debug
      "*** sync_ro current_phase = %a, start_mode = %a" pp_phase current_phase
        pp_mode t.start_mode];
    match t.ro with
    | None -> assert false
    | Some (model, repo) ->
        let () =
          match current_phase with
          | Before_rw_open | Between_rw_open_and_write -> ()
          | After_rw_write ->
              Model.preload model;
              Model.add model
        in
        Store.reload repo

  let check t =
    let test_all (model, repo) =
      test_dict repo model;
      test_index repo model;
      test_suffix repo model
    in
    Option.iter test_all t.ro;
    Option.iter test_all t.rw

  let close_everything t =
    match (t.rw, t.ro) with
    | Some (_, repo), Some (_, repo') ->
        let* () = Store.close repo in
        Store.close repo'
    | _ -> assert false

  let test_one start_mode ~ro_open_at ~ro_sync_at =
    let* t = create_test_env start_mode in

    let* () =
      if ro_open_at = Before_rw_open then open_ro t Before_rw_open
      else Lwt.return_unit
    in
    check t;
    if ro_sync_at = Before_rw_open then sync_ro t Before_rw_open;
    check t;

    let* () = start_rw t in
    check t;
    let* () =
      if ro_open_at = Between_rw_open_and_write then
        open_ro t Between_rw_open_and_write
      else Lwt.return_unit
    in
    check t;
    if ro_sync_at = Between_rw_open_and_write then
      sync_ro t Between_rw_open_and_write;
    check t;

    let* () = write_rw t in
    let* () =
      if ro_open_at = After_rw_write then open_ro t After_rw_write
      else Lwt.return_unit
    in
    check t;
    if ro_sync_at = After_rw_write then sync_ro t After_rw_write;
    check t;

    close_everything t

  let test_one_guarded start_mode ~ro_open_at ~ro_sync_at =
    Lwt.catch
      (fun () -> test_one start_mode ~ro_open_at ~ro_sync_at)
      (function
        | Skip_the_rest_of_that_test ->
            [%logs.debug "*** skip rest of the test"];
            Lwt.return_unit
        | exn -> Lwt.fail exn)

  (* All possible interleaving of the ro calls (open and sync) with the rw calls
     (open and write). Test them on three types of store.
     Test with two different indexing strategies. *)
  let test () =
    [ From_v2; From_v3; From_scratch ]
    |> Lwt_list.iter_s (fun start_mode ->
           let t = test_one_guarded start_mode in
           let* () = t ~ro_open_at:Before_rw_open ~ro_sync_at:Before_rw_open in
           let* () =
             t ~ro_open_at:Before_rw_open ~ro_sync_at:Between_rw_open_and_write
           in
           let* () = t ~ro_open_at:Before_rw_open ~ro_sync_at:After_rw_write in
           let* () =
             t ~ro_open_at:Between_rw_open_and_write
               ~ro_sync_at:Between_rw_open_and_write
           in
           let* () =
             t ~ro_open_at:Between_rw_open_and_write ~ro_sync_at:After_rw_write
           in
           let* () = t ~ro_open_at:After_rw_write ~ro_sync_at:After_rw_write in
           Lwt.return_unit)
end

module Always = Test (struct
  let indexing_strategy = "always"
end)

module Minimal = Test (struct
  let indexing_strategy = "minimal"
end)

let tests =
  [
    Alcotest.test_case "upgrade indexing_strategy=always" `Quick (fun () ->
        Lwt_main.run (Always.test ()));
    Alcotest.test_case "upgrade indexing_strategy=minimal" `Quick (fun () ->
        Lwt_main.run (Minimal.test ()));
  ]
