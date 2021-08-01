open Irmin.Export_for_backends

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

let reset_stats () =
  Index.Stats.reset_stats ();
  Irmin_pack.Stats.reset_stats ()

let random_char () = char_of_int (Random.int 256)
let random_string n = String.init n (fun _i -> random_char ())
let random_blob () = random_string 10
let random_key () = random_string 5

let default_results_dir =
  let ( / ) = Filename.concat in
  Unix.getcwd () / "_results" / Uuidm.to_string (Uuidm.v `V4)

let prepare_results_dir path =
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
  let open Progress in
  let config =
    Config.v ~max_width:(Some 79) ~min_interval:(Some Duration.(of_sec 0.5)) ()
  in
  let bar =
    Line.(
      const message
      ++ const " "
      ++ count_to n
      ++ const " "
      ++ const unit
      ++ const " "
      ++ elapsed ()
      ++ const " (ETA: "
      ++ eta n
      ++ const ") "
      ++ bar n
      ++ const " "
      ++ percentage_of n)
  in
  with_reporter ~config bar

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
    Fmt.epr "%+04.0fus: upper1 = %d M, upper0 = %d M, lower = %d M\n%!" dt
      (size upper1) (size upper0) (size lower)

  let rm_dir root =
    if Sys.file_exists root then (
      let cmd = Printf.sprintf "rm -rf %s" root in
      Logs.info (fun l -> l "exec: %s\n%!" cmd);
      let _ = Sys.command cmd in
      ())
end

module Generate_trees
    (Store : Irmin.S with type contents = string and type key = string list) =
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
