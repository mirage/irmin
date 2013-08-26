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

let get_terminal_columns () =
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

let terminal_columns =
  let v = Lazy.lazy_from_fun get_terminal_columns in
  fun () ->
    if Unix.isatty Unix.stdout
    then Lazy.force v
    else max_int

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

let left_column = 70
  let right_column =
    terminal_columns () - left_column + 16 (* padding due to escape chars *)

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

let dup oc =
  Unix.out_channel_of_descr (Unix.dup (Unix.descr_of_out_channel oc))

let mystdout = dup stdout
let mystderr = dup stderr

let printf fmt = Printf.fprintf mystdout fmt

let log_dir = ref (Sys.getcwd ())

let file_of_path path ext =
  let path = List.rev path in
  Printf.sprintf "%s.%s" (String.concat "-" (List.map OUnit.string_of_node path)) ext

let output_file path =
  Filename.concat !log_dir (file_of_path path "output")

let errors = ref []

let error path fmt =
  let filename = output_file path in
  let file = open_in filename in
  let output = string_of_channel file in
  close_in file;
  printf "%s\n%!" (indent_right (red "[ERROR]") right_column);
  Printf.kprintf (fun str ->
      let error =
        Printf.sprintf "%s\n%s\n%s\n%s\n%!"
          (red "-- %s --" filename) output
          (red "error:") str in
      errors := error :: !errors
    ) fmt


let string_of_node ~head = function
  | OUnit.ListItem i -> Printf.sprintf "%3d" i
  | OUnit.Label l    -> if head then Printf.sprintf "%-20s" (blue_s l) else l

let string_of_path path =
  let rec aux = function
    | []   -> "--"
    | OUnit.ListItem _ :: t -> aux t
    | h::t -> string_of_node ~head:true h
              ^ String.concat " " (List.map (string_of_node ~head:false) t) in
  aux (List.rev path)

let right msg =
  printf "%s\n%!" (indent_right msg right_column)

let print_result = function
  | OUnit.RSuccess p     -> right (green "[OK]")
  | OUnit.RFailure (p,s) -> error p "Failure: %s\n" s
  | OUnit.RError (p, s)  -> error p "%s\n" s
  | OUnit.RSkip _        -> right (yellow "[SKIP]")
  | OUnit.RTodo _        -> right (yellow "[TODO]")

let print_event = function
  | OUnit.EStart p  -> printf "%s%!" (indent_left (string_of_path p) left_column)
  | OUnit.EResult r -> print_result r
  | OUnit.EEnd p    -> ()

let failure = function
  | OUnit.RSuccess _
  | OUnit.RSkip _ -> false
  | _ -> true

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

let filter_test labels test =
  let rec aux prefix suffix test = match suffix, test with
    | []       , _
    | _        , OUnit.TestCase _ -> Some test
    | h::suffix, OUnit.TestLabel (l ,t) ->
      if h = l then match aux (h :: prefix) suffix t with
        | None  -> None
        | Some t ->Some (OUnit.TestLabel (l, t))
      else None
    | h::suffix, OUnit.TestList tl ->
      let tl, _ = List.fold_left (fun (tl, i) t ->
          if string_of_int i = h then match aux (h :: prefix) suffix t with
            | None   -> tl   , i+1
            | Some t -> t::tl, i+1
          else tl, i+1
        ) ([], 0) tl in
      match List.rev tl with
      | [] -> None
      | tl -> Some (OUnit.TestList tl) in
  aux [] labels test

let redirect_test_output labels test_fun =
  fun () ->
    let output_file = output_file labels in
    if not (Sys.file_exists !log_dir) then Unix.mkdir !log_dir 0o755;
    redirect stdout output_file;
    redirect stderr output_file;
    test_fun ()

let run test =
  IrminMisc.set_debug_mode true;
  let start_time = Sys.time () in
  let test = map_test redirect_test_output test in
  let results = OUnit.perform_test print_event test in
  let total_time = Sys.time () -. start_time in
  match List.filter failure results with
  | [] ->
    printf "%s in %.3fs. %d tests run.\n%!"
      (green "Test Successfull") total_time (List.length results)
  | l  ->
    List.iter (fun error -> printf "%s\n" error) (List.rev !errors);
    let s = if List.length l = 1 then "" else "s" in
    let msg = Printf.sprintf "%d error%s!" (List.length l) s in
    printf "%s in %.3fs. %d tests run.\n%!"
      (red_s msg) total_time (List.length results);
    exit 1

let docs = Hashtbl.create 16
let tests = ref []

let list_tests () =
  Hashtbl.iter (fun path doc ->
      printf "%s    %s\n" (string_of_path path) doc
    ) docs

let register name ts =
  let ts = List.mapi (fun i (doc, t) ->
      let path = [ OUnit.ListItem i;OUnit.Label name ] in
      Hashtbl.add docs path doc;
      OUnit.TestCase t
    ) ts in
  tests := !tests @ [ OUnit.TestLabel (name, OUnit.TestList ts) ]

let run_registred_tests dir =
  log_dir := dir;
  run (OUnit.TestList !tests)

let run_subtest dir labels =
  log_dir := dir;
  let test = OUnit.TestList !tests in
  match filter_test labels test with
  | None   -> ()
  | Some t -> run test

open Cmdliner

let test_dir =
  let doc = "Where to store the log files of the tests." in
  Arg.(value & opt string "./_tests/"  & info ["-o"] ~docv:"DIR" ~doc)

let default_cmd =
  let doc = "Run all the tests." in
  Term.(pure run_registred_tests $ test_dir),
  Term.info (Filename.basename Sys.argv.(0)) ~version:"0.1.0" ~doc

let test_cmd =
  let doc = "Run a given test." in
  let test =
    let doc = "A ':' separated list of labels, designing the prefix of \
               the tests to run." in
    Arg.(required & pos 0 (some & list ~sep:':' string) (Some [])
         & info [] ~doc ~docv:"TEST") in
  Term.(pure run_subtest $ test_dir $ test),
  Term.info "test" ~doc

let list_cmd =
  let doc = "List all available tests." in
  Term.(pure list_tests $ pure ()),
  Term.info "list" ~doc

type test_case = string * (unit -> unit)

type test = string * test_case list

let run tl =
  List.iter (fun (name, tests) -> register name tests) tl;
  match Term.eval_choice default_cmd [list_cmd; test_cmd] with
  | `Error _ -> exit 1
  | _ -> exit 0
