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

module Mtime = Import.Mtime

let c0 = Mtime_clock.counter ()
let now_us () = Mtime.span_to_us (Mtime_clock.count c0)
let last = ref (now_us ())

let dt_us () =
  let l = now_us () in
  let d = l -. !last in
  last := l;
  d

let reporter ?(prefix = "") () =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = dt_us () in
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

let default_artefacts_dir cwd =
  Eio.Path.(cwd / "_artefacts" / Uuidm.to_string (Uuidm.v `V4))

let prepare_artefacts_dir path =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 path

let with_timer f =
  let t0 = Sys.time () in
  let a = f () in
  let t1 = Sys.time () -. t0 in
  (t1, a)

let with_progress_bar ~message ~n ~unit =
  let open Progress in
  let config =
    Config.v ~max_width:(Some 79) ~min_interval:(Some Duration.(of_sec 0.5)) ()
  in
  let bar =
    Line.(
      list
        [
          const message;
          count_to n;
          const unit;
          elapsed ();
          parens (const "ETA: " ++ eta n);
          bar n;
          percentage_of n;
        ])
  in
  with_reporter ~config bar

module Conf = Irmin_tezos.Conf

module Schema = struct
  open Irmin
  module Metadata = Metadata.None
  module Contents = Contents.String
  module Path = Path.String_list
  module Branch = Branch.String
  module Hash = Hash.SHA1
  module Node = Node.Make (Hash) (Path) (Metadata)
  module Commit = Commit.Make (Hash)
  module Info = Info.Default
end

module Info (I : Irmin.Info.S) = struct
  let f () = I.v ~author:"tests" ~message:"commit" 0L
end

module FSHelper = struct
  let file f =
    (* in MiB *)
    try
      Eio.Switch.run @@ fun sw ->
      let f = Eio.Path.open_in ~sw f in
      Optint.Int63.to_int (Eio.File.size f)
    with Eio.Exn.Io (Eio.Fs.E (Not_found _), _) -> 0

  let dict root = file (Irmin_pack.Layout.V1_and_v2.dict ~root) / 1024 / 1024
  let pack root = file (Irmin_pack.Layout.V1_and_v2.pack ~root) / 1024 / 1024

  let index root =
    let index_dir = Eio.Path.(root / "index") in
    let a = file Eio.Path.(index_dir / "data") in
    let b = file Eio.Path.(index_dir / "log") in
    let c = file Eio.Path.(index_dir / "log_async") in
    (a + b + c) / 1024 / 1024

  let size root = dict root + pack root + index root
  let get_size root = size root
  let rm_dir root = Eio.Path.rmtree ~missing_ok:true root
end

module Generate_trees
    (Store : Irmin.Generic_key.KV with type Schema.Contents.t = bytes) =
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
      if i >= nb then tree
      else
        let tree = chain_tree tree depth path in
        aux (i + 1) tree
    in
    aux 0 tree

  let large_tree path tree width =
    let rec aux i tree =
      if i >= width then tree
      else
        let k = path @ [ random_key () ] in
        let tree = Store.Tree.add tree k (random_blob ()) in
        aux (i + 1) tree
    in
    aux 0 tree

  let add_large_trees width nb tree =
    let path = key 1 in
    let rec aux i tree =
      if i >= nb then tree
      else
        let path = path @ [ random_key () ] in
        let tree = large_tree path tree width in
        aux (i + 1) tree
    in
    aux 0 tree
end
