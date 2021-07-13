(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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
module IO = IO.Unix

let latest_version = `V2

(** Migrate data from the IO [src] (with [name] in path [root_old]) into the
    temporary dir [root_tmp], then swap in the replaced version. *)
let migrate_io_to_v2 ~progress src =
  IO.migrate ~progress src `V2 |> function
  | Ok () -> IO.close src
  | Error (`Msg s) -> invalid_arg s

let root c = Conf.get c Conf.root_key

let run config =
  if Conf.readonly config then raise S.RO_not_allowed;
  Log.debug (fun l -> l "[%s] migrate" (root config));
  Layout.stores ~root:(root config)
  |> List.map (fun store ->
         let io = IO.v ~version:None ~fresh:false ~readonly:true store in
         let version = IO.version io in
         (store, io, version))
  |> List.partition (fun (_, _, v) -> v = latest_version)
  |> function
  | migrated, [] ->
      Log.info (fun l ->
          l "Store at %s is already in current version (%a)" (root config)
            Version.pp latest_version);
      List.iter (fun (_, io, _) -> IO.close io) migrated
  | migrated, to_migrate ->
      List.iter (fun (_, io, _) -> IO.close io) migrated;
      (match migrated with
      | [] -> ()
      | _ :: _ ->
          let pp_ios = Fmt.(Dump.list (using (fun (n, _, _) -> n) string)) in
          Log.warn (fun l ->
              l
                "Store is in an inconsistent state: files %a have already been \
                 upgraded, but %a have not. Upgrading the remaining files now."
                pp_ios migrated pp_ios to_migrate));
      let total =
        to_migrate
        |> List.map (fun (_, io, _) -> IO.offset io)
        |> List.fold_left Int63.add Int63.zero
      in
      let bar, progress =
        Utils.Progress.counter ~total ~sampling_interval:100
          ~message:"Migrating store" ~pp_count:Utils.pp_bytes ()
      in
      List.iter (fun (_, io, _) -> migrate_io_to_v2 ~progress io) to_migrate;
      Utils.Progress.finalise bar
