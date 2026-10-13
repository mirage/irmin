(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
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

(** Benchmark for comparing performance with and without inline contents.

    This benchmark measures the time for:
    - Writing trees with small contents (that would be inlined when enabled)
    - Reading back those contents
    - Storage size comparison *)

module Store = Irmin_tezos.Store

let info = Store.Info.empty

(* Timing utilities *)
let time_it name f =
  Gc.full_major ();
  let t0 = Mtime_clock.counter () in
  let result = f () in
  let elapsed = Mtime_clock.count t0 in
  let ms = Mtime.Span.to_float_ns elapsed *. 1e-6 in
  Fmt.pr "%s: %.2f ms@." name ms;
  (ms, result)

(* Content generators *)
let small_content i = Bytes.of_string (Printf.sprintf "v%d" (i mod 1000))

(* 2-4 bytes, will be inlined *)
let medium_content i = Bytes.of_string (Printf.sprintf "value_%06d" i)
(* 12 bytes, will be inlined *)

let large_content i =
  Bytes.of_string (Printf.sprintf "large_content_value_%010d" i)
(* 30 bytes, NOT inlined *)

type content_size = Small | Medium | Large

let content_of_size size i =
  match size with
  | Small -> small_content i
  | Medium -> medium_content i
  | Large -> large_content i

let string_of_size = function
  | Small -> "small"
  | Medium -> "medium"
  | Large -> "large"

(* Benchmark configuration *)
type config = {
  num_entries : int;
  num_commits : int;
  content_size : content_size;
}

let _default_config =
  { num_entries = 1000; num_commits = 10; content_size = Small }

(* Run a single benchmark *)
let run_benchmark ~sw ~fs ~inline_contents ~config root =
  Eio.Path.rmtree ~missing_ok:true root;
  let store_config =
    Irmin_pack.config ~sw ~fs ~fresh:true
      ~indexing_strategy:Irmin_pack.Indexing_strategy.minimal ~inline_contents
      root
  in
  let repo = Store.Repo.v store_config in

  (* Benchmark: Write phase *)
  let write_time, commits =
    time_it
      (Printf.sprintf "Write (%d commits x %d entries)" config.num_commits
         config.num_entries) (fun () ->
        let commits = ref [] in
        for commit_idx = 0 to config.num_commits - 1 do
          let tree = Store.Tree.empty () in
          let tree =
            let rec add_entries tree i =
              if i >= config.num_entries then tree
              else
                let key =
                  [
                    Printf.sprintf "dir%d" (i / 100); Printf.sprintf "file%d" i;
                  ]
                in
                let value =
                  content_of_size config.content_size
                    ((commit_idx * config.num_entries) + i)
                in
                let tree = Store.Tree.add tree key value in
                add_entries tree (i + 1)
            in
            add_entries tree 0
          in
          let commit = Store.Commit.v repo ~parents:[] ~info tree in
          commits := commit :: !commits
        done;
        !commits)
  in

  (* Benchmark: Read phase - read all contents from last commit *)
  let read_time, () =
    time_it (Printf.sprintf "Read (%d entries)" config.num_entries) (fun () ->
        match commits with
        | [] -> ()
        | commit :: _ ->
            let tree = Store.Commit.tree commit in
            for i = 0 to config.num_entries - 1 do
              let key =
                [ Printf.sprintf "dir%d" (i / 100); Printf.sprintf "file%d" i ]
              in
              let _ = Store.Tree.find tree key in
              ()
            done)
  in

  (* Get storage size *)
  let size = Bench_common.FSHelper.get_size root in

  Store.Repo.close repo;
  (write_time, read_time, size)

(* Main benchmark runner *)
let run ~sw ~fs ~config =
  let root_no_inline = Eio.Path.(fs / "_bench" / "inline-no") in
  let root_with_inline = Eio.Path.(fs / "_bench" / "inline-yes") in

  Fmt.pr "@.=== Benchmark: %s contents, %d commits x %d entries ===@.@."
    (string_of_size config.content_size)
    config.num_commits config.num_entries;

  Fmt.pr "--- Without inlining ---@.";
  let no_inline_write, no_inline_read, no_inline_size =
    run_benchmark ~sw ~fs ~inline_contents:false ~config root_no_inline
  in

  Fmt.pr "@.--- With inlining ---@.";
  let with_inline_write, with_inline_read, with_inline_size =
    run_benchmark ~sw ~fs ~inline_contents:true ~config root_with_inline
  in

  (* Print comparison *)
  Fmt.pr "@.=== Comparison ===@.";
  Fmt.pr "Write time: %.2f ms (no inline) vs %.2f ms (inline) [%.1f%%]@."
    no_inline_write with_inline_write
    ((with_inline_write -. no_inline_write) /. no_inline_write *. 100.0);
  Fmt.pr "Read time:  %.2f ms (no inline) vs %.2f ms (inline) [%.1f%%]@."
    no_inline_read with_inline_read
    ((with_inline_read -. no_inline_read) /. no_inline_read *. 100.0);
  Fmt.pr "Store size: %d MB (no inline) vs %d MB (inline) [%.1f%%]@."
    no_inline_size with_inline_size
    (Float.of_int (with_inline_size - no_inline_size)
    /. Float.of_int (max 1 no_inline_size)
    *. 100.0);
  ()

(* Command line interface *)
open Cmdliner

let num_entries =
  let doc = "Number of entries per commit" in
  Arg.(value & opt int 1000 & info [ "n"; "num-entries" ] ~doc)

let num_commits =
  let doc = "Number of commits" in
  Arg.(value & opt int 10 & info [ "c"; "num-commits" ] ~doc)

let content_size =
  let doc =
    "Content size: small (2-4 bytes), medium (12 bytes), or large (30 bytes)"
  in
  let sizes = [ ("small", Small); ("medium", Medium); ("large", Large) ] in
  Arg.(value & opt (enum sizes) Small & info [ "s"; "size" ] ~doc)

let main () num_entries num_commits content_size =
  Eio_main.run @@ fun env ->
  let _fs = Eio.Stdenv.fs env in
  let cwd = Eio.Stdenv.cwd env in
  Eio.Switch.run @@ fun sw ->
  let config = { num_entries; num_commits; content_size } in
  (* Create benchmark directory *)
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(cwd / "_bench");
  run ~sw ~fs:cwd ~config

let cmd =
  let doc = "Benchmark inline contents performance" in
  let info = Cmd.info "bench-inlined-contents" ~doc in
  Cmd.v info
    Term.(const main $ const () $ num_entries $ num_commits $ content_size)

let () = exit (Cmd.eval cmd)
