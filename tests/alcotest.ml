(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

(* Types *)
type speed_level = [`Quick | `Slow]

type test_case = string * speed_level * (unit -> unit)

type test = string * test_case list

(* global state *)

let errors = ref []
let docs = Hashtbl.create 16
let speeds = Hashtbl.create 16
let tests = ref []
let global_name = ref (Filename.basename Sys.argv.(0))
let max_label = ref 0
let max_doc = ref 0
let verbose = ref false
let speed_level = ref `Slow

let compare_speed_level s1 s2 =
  match s1, s2 with
  | `Quick, `Quick
  | `Slow , `Slow  -> 0
  | `Quick, _      -> 1
  | _     , `Quick -> -1


(* Printers *)

let red fmt = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt = Printf.sprintf ("\027[36m"^^fmt^^"\027[m")

let red_s = red "%s"
let green_s = green "%s"
let yellow_s = yellow "%s"
let blue_s = blue "%s"

let with_process_in cmd f =
  let ic = Unix.open_process_in cmd in
  try
    let r = f ic in
    ignore (Unix.close_process_in ic) ; r
  with exn ->
    ignore (Unix.close_process_in ic) ; raise exn

let dup oc =
  Unix.out_channel_of_descr (Unix.dup (Unix.descr_of_out_channel oc))

let mystdout = dup stdout
let mystderr = dup stderr

let terminal_columns =
  let split s c =
    Re_str.split (Re_str.regexp (Printf.sprintf "[%c]" c)) s in
  try           (* terminfo *)
    with_process_in "tput cols"
      (fun ic -> int_of_string (input_line ic))
  with _ -> try (* GNU stty *)
      with_process_in "stty size"
        (fun ic ->
           match split (input_line ic) ' ' with
           | [_ ; v] -> int_of_string v
           | _ -> failwith "stty")
    with _ -> try (* shell envvar *)
        int_of_string (Sys.getenv "COLUMNS")
      with _ ->
        80

let line oc ?color c =
  let line = match color with
    | Some `Blue   -> blue_s (String.make terminal_columns c)
    | Some `Yellow -> yellow_s (String.make terminal_columns c)
    | None         -> String.make terminal_columns c in
  Printf.fprintf oc "%s\n%!" line

let indent_left s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    s ^ String.make nb ' '

let indent_right s nb =
  let nb = nb - String.length s in
  if nb <= 0 then
    s
  else
    String.make nb ' ' ^ s

let left_column () =
  !max_label + !max_doc + 16

let right_column () =
  terminal_columns
  - left_column ()
  + 15

let printf fmt = Printf.fprintf mystdout fmt

let right s =
  printf "%s\n%!" (indent_right s (right_column ()))

let left s =
  printf "%s%!" (indent_left s (left_column ()))

let string_of_channel ic =
  let n = 32768 in
  let s = String.create n in
  let b = Buffer.create 1024 in
  let rec iter ic b s =
    let nread =
      try input ic s 0 n
      with End_of_file -> 0 in
    if nread > 0 then (
      Buffer.add_substring b s 0 nread;
      iter ic b s
    ) in
  iter ic b s;
  Buffer.contents b

let log_dir = ref (Sys.getcwd ())

let file_of_path path ext =
  let path = List.tl (List.rev path) in
  Printf.sprintf "%s.%s" (String.concat "-" (List.map OUnit.string_of_node path)) ext

let output_file path =
  Filename.concat !log_dir (file_of_path path "output")

let string_of_node = function
  | OUnit.ListItem i -> Printf.sprintf "%3d" i
  | OUnit.Label l    -> indent_left (Printf.sprintf "%s" (blue_s l)) (!max_label+8)

let string_of_path path =
  let rec aux = function
    | []   -> "--"
    | OUnit.ListItem _ :: t -> aux t
    | h::t -> string_of_node h ^ String.concat " " (List.map string_of_node t) in
  aux (List.rev path)

let doc_of_path path =
  let path = List.rev (List.tl (List.rev path)) in
  try Hashtbl.find docs path
  with Not_found -> ""

let speed_of_path path =
  let path = List.rev (List.tl (List.rev path)) in
  try Hashtbl.find speeds path
  with Not_found -> `Slow

let short_string_of_path path =
  let path = List.rev (List.tl (List.rev path)) in
  OUnit.string_of_path path

let error path fmt =
  let filename = output_file path in
  let file = open_in filename in
  let output = string_of_channel file in
  close_in file;
  right (red "[ERROR]");
  Printf.kprintf (fun str ->
      let error =
        Printf.sprintf "%s\n%s\n%s:\n%s\n%s\n"
          (red "-- %s Failed --" (short_string_of_path path))
          (doc_of_path path)
          filename output str in
      Printf.eprintf "%s\n%!" str;
      errors := error :: !errors
    ) fmt

let print_result = function
  | OUnit.RSuccess p     -> right (green "[OK]")
  | OUnit.RFailure (p,s) -> error p "Failure: %s" s
  | OUnit.RError (p, s)  -> error p "%s" s
  | OUnit.RSkip _        -> right (yellow "[SKIP]")
  | OUnit.RTodo _        -> right (yellow "[TODO]")

let print_event = function
  | OUnit.EStart p  ->
    left (Printf.sprintf "%s   %s" (string_of_path p) (doc_of_path p))
  | OUnit.EResult r -> print_result r
  | OUnit.EEnd p    -> ()

let failure = function
  | OUnit.RSuccess _
  | OUnit.RSkip _  -> false
  | OUnit.RError _
  | OUnit.RFailure _
  | OUnit.RTodo _ -> true

let has_run = function
  | OUnit.RSuccess _
  | OUnit.RError _
  | OUnit.RFailure _ -> true
  | OUnit.RSkip _
  | OUnit.RTodo _    -> false

let redirect oc file =
  let oc = Unix.descr_of_out_channel oc in
  let fd = Unix.(openfile file [O_WRONLY; O_TRUNC; O_CREAT] 0o666) in
  Unix.dup2 fd oc;
  Unix.close fd

let map_test fn test =
  let rec aux path = function
    | OUnit.TestCase tf      -> OUnit.TestCase (fn path tf)
    | OUnit.TestList tl      ->
      let tl = List.mapi (fun i t -> aux (OUnit.ListItem i :: path) t) tl in
      OUnit.TestList tl
    | OUnit.TestLabel (l ,t) ->
      let t = aux (OUnit.Label l :: path) t in
      OUnit.TestLabel (l, t) in
  aux [] test

let same_label x y =
  (String.lowercase x) = (String.lowercase y)

let skip_fun () =
  OUnit.skip_if true "Not selected"

let skip =
  OUnit.TestCase skip_fun

let skip_label l =
  OUnit.TestLabel (l, skip)

let filter_test ~subst labels test =
  let rec aux path suffix test =
    match suffix, test with
    | []       , _
    | _        , OUnit.TestCase _       -> Some test
    | h::suffix, OUnit.TestLabel (l ,t) ->
      if same_label h l then
        match aux (OUnit.Label h :: path) suffix t with
        | None  -> if subst then Some (OUnit.TestLabel (l, skip)) else None
        | Some t ->Some (OUnit.TestLabel (l, t))
      else None
    | h::suffix, OUnit.TestList tl ->
      let tl, _ = List.fold_left (fun (tl, i) t ->
          if same_label (string_of_int i) h then
            match aux (OUnit.ListItem i :: path) suffix t with
            | None   -> (if subst then skip :: tl else tl), i+1
            | Some t -> (t :: tl), i+1
          else          (if subst then skip :: tl else tl), i+1
        ) ([], 0) tl in
      match List.rev tl with
      | [] -> None
      | tl -> Some (OUnit.TestList tl) in
  match test with
  | OUnit.TestCase _
  | OUnit.TestLabel _ -> aux [] labels test
  | OUnit.TestList tl ->
    let tl = List.fold_left (fun acc test ->
        match aux [] labels test with
        | None   -> if subst then skip :: acc else acc
        | Some r -> r :: acc
      ) [] tl in
    if tl = [] then None else Some (OUnit.TestList tl)

let filter_tests ~subst labels tests =
  let tests = List.fold_left (fun acc test ->
      match test with
      | OUnit.TestCase _
      | OUnit.TestList _ -> assert false
      | OUnit.TestLabel (l, _) ->
        match filter_test ~subst labels test with
        | None   -> if subst then skip_label l :: acc else acc
        | Some r -> r :: acc
    ) [] tests in
  List.rev tests

let redirect_test_output labels test_fun =
  fun () ->
    let output_file = output_file labels in
    if not (Sys.file_exists !log_dir) then Unix.mkdir !log_dir 0o755;
    redirect stdout output_file;
    redirect stderr output_file;
    test_fun ()

let select_speed labels test_fun =
  if compare_speed_level (speed_of_path labels) !speed_level >= 0 then test_fun
  else skip_fun

let run test =
  IrminLog.set_debug_mode true;
  let start_time = Sys.time () in
  let test = map_test redirect_test_output test in
  let test = map_test select_speed test in
  let results = OUnit.perform_test print_event test in
  let total_time = Sys.time () -. start_time in
  let runs = List.length (List.filter has_run results) in
  let s = if runs = 1 then "" else "s" in
  match List.filter failure results with
  | [] ->
    printf "%s in %.3fs. %d test%s ran.\n%!"
      (green "Test Successfull") total_time runs s
  | l  ->
    if !verbose then
      List.iter (fun error -> printf "%s\n" error) (List.rev !errors);
    let s1 = if List.length l = 1 then "" else "s" in
    let msg = Printf.sprintf "%d error%s!" (List.length l) s1 in
    printf "%s in %.3fs. %d test%s ran.\n%!"
      (red_s msg) total_time runs s;
    exit 1

let list_tests () =
  let list = Hashtbl.fold (fun k v l -> (k,v) :: l) docs [] in
  let list = List.sort (fun (x,_) (y,_) -> compare (List.rev x) (List.rev y)) list in
  List.iter (fun (path, doc) ->
      printf "%s    %s\n" (string_of_path path) doc
    ) list

let register name (ts:test_case list) =
  let ts = List.mapi (fun i (doc, speed, t) ->
      max_label := max !max_label (String.length name);
      max_doc := max !max_doc (String.length doc);
      let path = [ OUnit.ListItem i; OUnit.Label name ] in
      let doc =
        if doc.[String.length doc - 1] = '.' then doc
        else doc ^ "." in
      Hashtbl.add docs path doc;
      Hashtbl.add speeds path speed;
      OUnit.TestCase t
    ) ts in
  tests := !tests @ [ OUnit.TestLabel (name, OUnit.TestList ts) ]

let run_registred_tests dir verb quick =
  verbose := verb;
  log_dir := dir;
  speed_level := (if quick then `Quick else `Slow);
  run (OUnit.TestList !tests)

let run_subtest dir verb quick labels =
  verbose := verb || true;
  speed_level := (if quick then `Quick else `Slow);
  log_dir := dir;
  let is_empty = filter_tests ~subst:false labels !tests = [] in
  if is_empty then (
    printf "%s\n" (red "Invalid request!"); exit 1
  ) else
    let tests = filter_tests ~subst:true labels !tests in
    run (OUnit.TestList tests)

open Cmdliner

let test_dir =
  let doc = "Where to store the log files of the tests." in
  Arg.(value & opt string "./_tests/"  & info ["-o"] ~docv:"DIR" ~doc)

let verbose =
  let doc = "Display the output for test errors." in
  Arg.(value & flag & info ["v";"verbose"] ~docv:"" ~doc)

let quicktests =
  let doc = "Run only the quick tests." in
  Arg.(value & flag & info ["q"; "quick-tests"] ~docv:"" ~doc)

let default_cmd =
  let doc = "Run all the tests." in
  Term.(pure run_registred_tests $ test_dir $ verbose $ quicktests),
  Term.info !global_name ~version:"0.1.0" ~doc

let test_cmd =
  let doc = "Run a given test." in
  let test =
    let doc = "The list of labels identifying a subsets of the tests to run" in
    Arg.(value & pos_all string [] & info [] ~doc ~docv:"LABEL") in
  Term.(pure run_subtest $ test_dir $ verbose $ quicktests $ test),
  Term.info "test" ~doc

let list_cmd =
  let doc = "List all available tests." in
  Term.(pure list_tests $ pure ()),
  Term.info "list" ~doc

let run name (tl:test list) =
  global_name := name;
  List.iter (fun (name, tests) -> register name tests) tl;
  match Term.eval_choice default_cmd [list_cmd; test_cmd] with
  | `Error _ -> exit 1
  | _ -> exit 0
