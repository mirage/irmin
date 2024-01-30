(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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
open Astring

let what =
  "This example demonstrates custom merges on Irmin datastructures.\n\n\
   It models log files as a sequence of lines, ordered by timestamps.\n\n\
   The log files of `branch 1` and `branch 2` are merged by using the following \n\
   strategy:\n\
  \  - find the log file corresponding the lowest common ancestor: `lca`\n\
  \  - remove the prefix `lca` from `branch 1`; this gives `l1`;\n\
  \  - remove the prefix `lca` from `branch 2`; this gives `l2`;\n\
  \  - interleave `l1` and `l2` by ordering the timestamps; This gives `l3`;\n\
  \  - concatenate `lca` and `l3`; This gives the final result."

let time = ref 0L
let failure fmt = Fmt.kstr failwith fmt

(* A log entry *)
module Entry : sig
  include Irmin.Type.S

  val v : string -> t
  val timestamp : t -> int64
end = struct
  type t = { timestamp : int64; message : string } [@@deriving irmin]

  let compare x y = Int64.compare x.timestamp y.timestamp

  let v message =
    time := Int64.add 1L !time;
    { timestamp = !time; message }

  let timestamp t = t.timestamp
  let pp ppf { timestamp; message } = Fmt.pf ppf "%04Ld: %s" timestamp message

  let of_string str =
    match String.cut ~sep:": " str with
    | None -> Error (`Msg ("invalid entry: " ^ str))
    | Some (x, message) -> (
        try Ok { timestamp = Int64.of_string x; message }
        with Failure e -> Error (`Msg e))

  let t = Irmin.Type.like ~pp ~of_string ~compare t
end

(* A log file *)
module Log : sig
  include Irmin.Contents.S

  val add : t -> Entry.t -> t
  val empty : t
end = struct
  type t = Entry.t list [@@deriving irmin]

  let empty = []
  let pp_entry = Irmin.Type.pp Entry.t
  let lines ppf l = List.iter (Fmt.pf ppf "%a\n" pp_entry) (List.rev l)

  let of_string str =
    let lines = String.cuts ~empty:false ~sep:"\n" str in
    try
      List.fold_left
        (fun acc l ->
          match Irmin.Type.of_string Entry.t l with
          | Ok x -> x :: acc
          | Error (`Msg e) -> failwith e)
        [] lines
      |> fun l -> Ok l
    with Failure e -> Error (`Msg e)

  let t = Irmin.Type.like ~pp:lines ~of_string t
  let timestamp = function [] -> 0L | e :: _ -> Entry.timestamp e

  let newer_than timestamp file =
    let rec aux acc = function
      | [] -> List.rev acc
      | h :: _ when Entry.timestamp h <= timestamp -> List.rev acc
      | h :: t -> aux (h :: acc) t
    in
    aux [] file

  let compare_entry = Irmin.Type.(unstage (compare Entry.t))

  let merge ~old t1 t2 =
    let open Irmin.Merge.Infix in
    old () >>=* fun old ->
    let old = match old with None -> [] | Some o -> o in
    let ts = timestamp old in
    let t1 = newer_than ts t1 in
    let t2 = newer_than ts t2 in
    let t3 = List.sort compare_entry (List.rev_append t1 t2) in
    Irmin.Merge.ok (List.rev_append t3 old)

  let merge = Irmin.Merge.(option (v t merge))
  let add t e = e :: t
end

(* Build an Irmin store containing log files. *)
module Store = Irmin_git_unix.FS.KV (Log)

(* Set-up the local configuration of the Git repository. *)
let config = Irmin_git.config ~bare:true Config.root

(* Convenient alias for the info function for commit messages *)
let info = Irmin_git_unix.info
let log_file = [ "local"; "debug" ]

let all_logs t =
  let logs = Store.find t log_file in
  match logs with None -> Log.empty | Some l -> l

(** Persist a new entry in the log. Pretty inefficient as it reads/writes the
    whole file every time. *)
let log t fmt =
  Printf.ksprintf
    (fun message ->
      let logs = all_logs t in
      let logs = Log.add logs (Entry.v message) in
      Store.set_exn t ~info:(info "Adding a new entry") log_file logs)
    fmt

let print_logs name t =
  let logs = all_logs t in
  Fmt.pr "-----------\n%s:\n-----------\n%a%!" name (Irmin.Type.pp Log.t) logs

let main () =
  Eio.Switch.run @@ fun sw ->
  Config.init ();
  let repo = Store.Repo.v ~sw config in
  let t = Store.main repo in

  (* populate the log with some random messages *)
  List.iter
    (fun msg -> log t "This is my %s " msg)
    [ "first"; "second"; "third" ];
  Printf.printf "%s\n\n" what;
  print_logs "lca" t;
  let x = Store.clone ~src:t ~dst:"test" in
  log x "Adding new stuff to x";
  log x "Adding more stuff to x";
  log x "More. Stuff. To x.";
  print_logs "branch 1" x;
  log t "I can add stuff on t also";
  log t "Yes. On t!";
  print_logs "branch 2" t;
  let r = Store.merge_into ~info:(info "Merging x into t") x ~into:t in
  match r with Ok () -> print_logs "merge" t | Error _ -> failwith "conflict!"

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
