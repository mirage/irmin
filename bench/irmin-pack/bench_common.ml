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

open Lwt.Syntax

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Unix.gettimeofday () in
      Fmt.kpf k ppf
        ("%s%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        prefix dt Logs_fmt.pp_header (level, h)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  { Logs.report }

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.set_reporter (reporter ());
  ()

let reset_stats () =
  Index.Stats.reset_stats ();
  Irmin_pack.Stats.reset_stats ()

let random_char () = char_of_int (Random.int 256)
let random_string n = String.init n (fun _i -> random_char ())
let random_blob () = random_string 10 |> Bytes.of_string
let random_key () = random_string 5

let default_artefacts_dir =
  let ( / ) = Filename.concat in
  Unix.getcwd () / "_artefacts" / Uuidm.to_string (Uuidm.v `V4)

let prepare_artefacts_dir path =
  let rec mkdir_p path =
    if Sys.file_exists path then ()
    else
      let path' = Filename.dirname path in
      if path' = path then failwith "Failed to prepare result dir";
      mkdir_p path';
      Unix.mkdir path 0o755
  in
  mkdir_p path

let with_timer f =
  let t0 = Sys.time () in
  let+ a = f () in
  let t1 = Sys.time () -. t0 in
  (t1, a)

let with_progress_bar ~message ~n ~unit =
  let bar =
    let w =
      if n = 0 then 1
      else float_of_int n |> log10 |> floor |> int_of_float |> succ
    in
    let pp fmt i = Format.fprintf fmt "%*Ld/%*d %s" w i w n unit in
    let pp f = f ~width:(w + 1 + w + 1 + String.length unit) pp in
    Progress_unix.counter ~mode:`ASCII ~width:79 ~total:(Int64.of_int n)
      ~message ~pp ()
  in
  Progress_unix.with_reporters bar

module Conf = struct
  let entries = 32
  let stable_hash = 256
end

let info () =
  let date = Int64.of_float (Unix.gettimeofday ()) in
  let author = Printf.sprintf "TESTS" in
  Irmin.Info.v ~date ~author "commit "

module FSHelper = struct
  let file f =
    try (Unix.stat f).st_size with Unix.Unix_error (Unix.ENOENT, _, _) -> 0

  let dict root = file (Irmin_pack.Layout.dict ~root) / 1024 / 1024
  let pack root = file (Irmin_pack.Layout.pack ~root) / 1024 / 1024

  let index root =
    let index_dir = Filename.concat root "index" in
    let a = file (Filename.concat index_dir "data") in
    let b = file (Filename.concat index_dir "log") in
    let c = file (Filename.concat index_dir "log_async") in
    (a + b + c) / 1024 / 1024

  let size root = dict root + pack root + index root
  let get_size root = size root

  let print_size_layers root =
    let dt = Unix.gettimeofday () in
    let upper1 = Filename.concat root "upper1" in
    let upper0 = Filename.concat root "upper0" in
    let lower = Filename.concat root "lower" in
    Logs.app (fun l ->
        l "%+04.0fus: upper1 = %d M, upper0 = %d M, lower = %d M\n%!" dt
          (size upper1) (size upper0) (size lower))

  let rm_dir root =
    if Sys.file_exists root then (
      let cmd = Printf.sprintf "rm -rf %s" root in
      Logs.info (fun l -> l "exec: %s" cmd);
      let _ = Sys.command cmd in
      ())
end

module Generate_trees
    (Store : Irmin.S with type contents = bytes and type key = string list) =
struct
  let key depth =
    let rec aux i acc =
      if i >= depth then acc
      else
        let k = random_key () in
        aux (i + 1) (k :: acc)
    in
    aux 0 []

  let chain_tree tree depth path =
    let k = path @ key depth in
    Store.Tree.add tree k (random_blob ())

  let add_chain_trees depth nb tree =
    let path = key 2 in
    let rec aux i tree =
      if i >= nb then Lwt.return tree
      else
        let* tree = chain_tree tree depth path in
        aux (i + 1) tree
    in
    aux 0 tree

  let large_tree path tree width =
    let rec aux i tree =
      if i >= width then Lwt.return tree
      else
        let k = path @ [ random_key () ] in
        let* tree = Store.Tree.add tree k (random_blob ()) in
        aux (i + 1) tree
    in
    aux 0 tree

  let add_large_trees width nb tree =
    let path = key 1 in
    let rec aux i tree =
      if i >= nb then Lwt.return tree
      else
        let path = path @ [ random_key () ] in
        let* tree = large_tree path tree width in
        aux (i + 1) tree
    in
    aux 0 tree
end
