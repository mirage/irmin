(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

open Lwt.Syntax

module Schema = Irmin.Schema.KV (Irmin.Contents.String)
(** Make schema *)

(** Define configuration parameters for pack store *)
module Store_conf : Irmin_pack.Conf.S = struct
  (** The branching factor of the inode tree. 32 performs well *)
  let entries = 32

  (** Set this to 0 to only use inode hashing (this parameter exists for
      particular backwards compatibility applications with Irmin 1.0 data) *)
  let stable_hash = 0

  (** GC only supports minimal indexing strategy so [contents_length_header]
      must be [Some `Varint]. [None] requires an always indexing strategy *)
  let contents_length_header = Some `Varint

  (** This is the recommended sorting algorithm for new stores *)
  let inode_child_order = `Hash_bits

  (** Throw an error (or not) if an empty tree is persisted *)
  let forbid_empty_dir_persistence = true
end

(** Make pack store *)

module Maker = Irmin_pack_unix.Maker (Store_conf)
module Store = Maker.Make (Schema)

(** Define configuration for repo *)
module Repo_config = struct
  (** The number of entries to cache in the index log (an in-memory and on-disk
      data store). Default is [2_500_000] *)
  let index_log_size = 2_500_000

  (** Choose what to do when the index log is full and a merge is in-progress.

      - [Block_writes] (the default) will block writing until the merge is
        complete
      - [Overcommit_memory] will increase the in-memory cache indefinitely *)
  let merge_throttle = `Block_writes

  (** Must use minimal indexing strategy to use GC *)
  let indexing_strategy = Irmin_pack.Indexing_strategy.minimal

  (** Buffer size that triggers auto flushing to disk *)
  let dict_auto_flush_threshold = 1_000_000

  (** Buffer size that triggers auto flushing to disk *)
  let suffix_auto_flush_threshold = 1_000_000

  (** Location on disk to save the repository

      Note: irmin-pack will not create the entire path, only the final directory *)
  let root = "./irmin-pack-example"

  (** See {!Irmin_pack.Conf} for more keys that can be used when initialising
      the repository config *)

  (** Create new repository every time. Use [false] (the defalut) to use
      existing repository. *)
  let fresh = true

  (** Create config for our repository *)
  let config =
    Irmin_pack.config ~fresh ~index_log_size ~merge_throttle ~indexing_strategy
      ~dict_auto_flush_threshold ~suffix_auto_flush_threshold root
end

(** Utility for creating commit info *)
let info fmt = Irmin_unix.info ~author:"pack example" fmt

(** Utility for computing the size of a directory *)
let rec megabytes_of_path path =
  if Sys.is_directory path then
    Array.fold_left
      (fun acc p -> megabytes_of_path (Filename.concat path p) +. acc)
      0. (Sys.readdir path)
  else float_of_int Unix.((stat path).st_size) /. 1e6

(** Demonstrate running GC on all commits before the head *)
let gc_all_but_head repo branch =
  let* head = Store.Head.get branch in
  let head_key = Store.Commit.key head in
  let finished = function
    | Ok stats ->
        let duration = Irmin_pack_unix.Stats.Latest_gc.total_duration stats in
        let finalise_duration =
          Irmin_pack_unix.Stats.Latest_gc.finalise_duration stats
        in
        Printf.printf
          "GC finished in %.4fs. Finalise took %.4fs. Size of repo: %.2fMB.\n"
          duration finalise_duration
          (megabytes_of_path Repo_config.root)
        |> Lwt.return
    | Error (`Msg err) -> print_endline err |> Lwt.return
  in
  let+ launched = Store.Gc.run ~finished repo head_key in
  match launched with
  | Ok false ->
      Printf.printf "GC did not launch. Already running? %B\n"
        (Store.Gc.is_finished repo = false)
  | Ok true ->
      Printf.printf "GC started. Size of repo: %.2fMB\n"
        (megabytes_of_path Repo_config.root)
  | Error (`Msg err) -> print_endline err

let main () =
  let run_gc = true in
  let num_of_commits = 200_000 in
  let gc_every = 5_000 in
  let* repo = Store.Repo.v Repo_config.config in
  let* main_branch = Store.main repo in
  (* Create commits *)
  let* _ =
    let rec loop i n =
      let key = Printf.sprintf "hello%d" i in
      let value = Printf.sprintf "packfile%d" i in
      let* _ =
        Store.set ~info:(info "add %s = %s" key value) main_branch [ key ] value
      in
      let* _ =
        if run_gc && i mod gc_every = 0 then gc_all_but_head repo main_branch
        else Lwt.return_unit
      in
      if i >= n then Lwt.return_unit else loop (i + 1) n
    in
    loop 0 num_of_commits
  in
  (* A GC may still be running. Wait for GC to finish before ending the process *)
  let* _ = Store.Gc.wait repo in
  Lwt.return_unit

let () = Lwt_main.run @@ main ()
