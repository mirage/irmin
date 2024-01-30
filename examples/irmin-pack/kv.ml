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

(* A minimal example of instantiating a `irmin-pack.unix` key-value store. *)

let src =
  Logs.Src.create "irmin-pack.unix/examples/kv"
    ~doc:"irmin-pack.unix/examples/kv"

module Log = (val Logs.src_log src : Logs.LOG)

(* Compile-time configurations passed to a functor. *)
module Conf = struct
  let entries = 32
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = true
end

(* Run-time configurations passed when instantiating a repository. *)
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
      root
end

module StoreMaker = Irmin_pack_unix.KV (Conf)
module Store = StoreMaker.Make (Irmin.Contents.String)

let main () =
  (* Create a switch *)
  Eio.Switch.run @@ fun sw ->
  (* Instantiate a repository *)
  let repo = Store.Repo.v ~sw Repo_config.config in

  (* Get the store from the main branch. *)
  let store = Store.main repo in

  (* Set a value. *)
  let () =
    Store.set_exn
      ~info:(fun () -> Store.Info.empty)
      store [ "hello" ] "irmin-pack.unix!"
  in

  (* Get the value *)
  let content = Store.get store [ "hello" ] in

  Log.app (fun m -> m "hello: %s" content)

let setup_logs () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.(set_level @@ Some Debug)

let () =
  Eio_main.run @@ fun env ->
  Irmin_pack_unix.Io.set_env (Eio.Stdenv.fs env);
  setup_logs ();
  main ()
