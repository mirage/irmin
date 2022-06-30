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
open Test_upgrade

let suffix_mem repo e =
  let k = key_of_entry e in
  try
    match e.k with
    | `c ->
        Store.S.X.Commit.CA.unsafe_find ~check_integrity:false
          (snd (Store.S.X.Repo.commit_t repo))
          k
        |> Option.is_some
    | `n ->
        Store.S.X.Node.CA.unsafe_find ~check_integrity:false
          (snd (Store.S.X.Repo.node_t repo))
          k
        |> Option.is_some
    | `b ->
        Store.S.X.Contents.CA.unsafe_find ~check_integrity:false
          (Store.S.X.Repo.contents_t repo)
          k
        |> Option.is_some
  with Irmin_pack_unix.Pack_store.Invalid_read _ ->
    (* In RW mode, [mem] will raise an exception if the offset of the key is
       out of the bounds of the pack file *)
    false

let check_suffix repo model =
  List.iter
    (fun e ->
      let got = suffix_mem repo e in
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

let check_ro t =
  match t.ro with
  | None -> assert false
  | Some (model, repo) ->
      check_dict repo model;
      check_index repo model;
      check_suffix repo model

type phase_flush =
  | S1_before_flush
  | S2_after_flush_dict
  | S3_after_flush_suffix
  | S4_after_flush
[@@deriving irmin ~pp]

let write1_dict model =
  Model.preload_dict model;
  Model.write1_dict model

let write1_suffix model =
  write1_dict model;
  Model.preload_suffix model;
  Model.write1_suffix model

let write1_index model =
  write1_dict model;
  write1_suffix model;
  Model.preload_index model;
  Model.write1_index model

let reload_ro t current_phase =
  [%logs.app
    "*** reload_ro %a, %a" pp_setup t.setup pp_phase_flush current_phase];
  match t.ro with
  | None -> assert false
  | Some (model, repo) ->
      let () =
        match current_phase with
        | S1_before_flush -> ()
        | S2_after_flush_dict -> write1_dict model
        | S3_after_flush_suffix -> write1_suffix model
        | S4_after_flush -> write1_index model
      in
      Store.reload repo

let write1_no_flush bstore nstore cstore =
  let* _ = Store.put_borphan bstore in
  let* _ = Store.put_c0 bstore nstore cstore in
  let* _ = Store.put_c1 bstore nstore cstore in
  let* _ = Store.put_borphan' bstore in
  Lwt.return_unit

(* Open both stores. RW writes but does not flush - we do this by running the
   rest of the test inside the [batch]. Then reload the RO at different phases
   during the flush. *)
let test_one t ~(ro_reload_at : phase_flush) =
  let aux phase =
    let () = check_ro t in
    if ro_reload_at = phase then reload_ro t phase;
    check_ro t
  in
  let* () = start_rw t in
  (* Open RO in a stage without any data in the model. *)
  let* () = open_ro t S2_before_write in
  let rw_repo = Option.get t.rw |> snd in
  let* () =
    Store.S.Backend.Repo.batch rw_repo (fun bstore nstore cstore ->
        let* () = write1_no_flush bstore nstore cstore in
        let () = aux S1_before_flush in
        let hook = function
          | `After_dict -> aux S2_after_flush_dict
          | `After_suffix -> aux S3_after_flush_suffix
        in
        let () = Store.S.X.Repo.flush_with_hook ~hook rw_repo in
        let () = aux S4_after_flush in
        Lwt.return_unit)
  in
  Lwt.return_unit

let test_one_guarded setup ~ro_reload_at =
  let t = create_test_env setup in
  let* () = test_one t ~ro_reload_at in
  close_everything t

let test () =
  let setup =
    (* We are using indexing strategy always here to have more entries in index
       for the flush tests. *)
    { start_mode = From_scratch; indexing_strategy = `always; lru_size = 0 }
  in
  let t = test_one_guarded setup in
  let* () = t ~ro_reload_at:S1_before_flush in
  let* () = t ~ro_reload_at:S2_after_flush_dict in
  let* () = t ~ro_reload_at:S3_after_flush_suffix in
  let* () = t ~ro_reload_at:S4_after_flush in
  Lwt.return_unit

let tests =
  [
    Alcotest.test_case "Reload during flush stages" `Quick (fun () ->
        Lwt_main.run (test ()));
  ]
