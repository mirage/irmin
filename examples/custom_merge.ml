let what =
  "This example demonstrates custom merges on Irmin datastructures.\n\
   \n\
   It models log files as a sequence of lines, ordered by timestamps.\n\
   \n\
   The log files of `branch 1` and `branch 2` are merged by using the following \n\
   strategy:\n\
  \  - find the log file corresponding the lowest common ancestor: `lca`\n\
  \  - remove the prefix `lca` from `branch 1`; this gives `l1`;\n\
  \  - remove the prefix `lca` from `branch 2`; this gives `l2`;\n\
  \  - interleave `l1` and `l2` by ordering the timestamps; This gives `l3`;\n\
  \  - concatenate `lca` and `l3`; This gives the final result."

open Lwt.Infix
open Astring

let info = Irmin_unix.info

let time = ref 0

let failure fmt = Fmt.kstrf failwith fmt

(* A log entry *)
module Entry: sig
  include Irmin.Contents.Conv
  val v: string -> t
  val compare: t -> t -> int
  val timestamp: t -> int
end = struct

  type t = {
    timestamp: int;
    message  : string;
  }

  let compare x y = compare x.timestamp y.timestamp

  let v message =
    incr time;
    { timestamp = !time; message }

  let t =
    let open Irmin.Type in
    record "entry" (fun timestamp message -> { timestamp; message })
    |+ field "timestamp" int    (fun t -> t.timestamp)
    |+ field "message"   string (fun t -> t.message)
    |> sealr

  let timestamp t = t.timestamp

  let pp ppf { timestamp; message } =
    Fmt.pf ppf  "%04d: %s\n" timestamp message

  let of_string str =
    match String.cut ~sep:": " str with
    | None -> Error (`Msg ("invalid entry: " ^ str))
    | Some (x, message) ->
      try Ok { timestamp = int_of_string x; message }
      with Failure e -> Error (`Msg e)

end

(* A log file *)
module Log: sig
  include Irmin.Contents.S
  val add: t -> Entry.t -> t
  val empty: t
end = struct

  type t = Entry.t list
  let t = Irmin.Type.(list Entry.t)

  let empty = []

  let pp ppf l = List.iter (Fmt.pf ppf "%a\n" Entry.pp ) (List.rev l)

  let of_string str =
    let lines = String.cuts ~sep:"\n" str in
    try
      List.fold_left (fun acc l ->
          match Entry.of_string l with
          | Ok x           -> x :: acc
          | Error (`Msg e) -> failwith e
        ) [] lines
      |> fun l -> Ok l
    with Failure e ->
      Error (`Msg e)

  let timestamp = function
    | [] -> 0
    | e :: _ -> Entry.timestamp e

  let newer_than timestamp file =
    let rec aux acc = function
      | [] -> List.rev acc
      | h:: _ when Entry.timestamp h <= timestamp -> List.rev acc
      | h::t -> aux (h::acc) t
    in
    aux [] file

  let merge ~old t1 t2 =
    let open Irmin.Merge.Infix in
    old () >>=* fun old ->
    let old = match old with None -> [] | Some o -> o in
    let ts = timestamp old in
    let t1 = newer_than ts t1 in
    let t2 = newer_than ts t2 in
    let t3 = List.sort Entry.compare (List.rev_append t1 t2) in
    Irmin.Merge.ok (List.rev_append t3 old)

  let merge = Irmin.Merge.(option (v t merge))

  let add t e = e :: t

end

module Store = Irmin_unix.Git.FS.KV(Log)

let config = Irmin_git.config ~bare:true Config.root

let log_file = [ "local"; "debug" ]

let all_logs t =
  Store.find t log_file >>= function
  | None   -> Lwt.return Log.empty
  | Some l -> Lwt.return l

(* Persist a new entry in the log. Pretty inefficient as it
   reads/writes the whole file every time. *)
let log t fmt =
  Printf.ksprintf (fun message ->
      all_logs t >>= fun logs ->
      let logs = Log.add logs (Entry.v message) in
      Store.set t ~info:(info "Adding a new entry") log_file logs
    ) fmt

let print_logs name t =
  all_logs t >|= fun logs ->
  Fmt.pr "-----------\n%s:\n-----------\n%a%!" name Log.pp logs

let main () =
  Config.init ();
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->

  (* populate the log with some random messages *)
  Lwt_list.iter_s (fun msg ->
      log t "This is my %s " msg
    ) [ "first"; "second"; "third"]
  >>= fun () ->

  Printf.printf "%s\n\n" what;

  print_logs "lca" t >>= fun () ->

  Store.clone ~src:t ~dst:"test" >>= fun x ->

  log x "Adding new stuff to x"  >>= fun () ->
  log x "Adding more stuff to x" >>= fun () ->
  log x "More. Stuff. To x."     >>= fun () ->

  print_logs "branch 1" x >>= fun () ->

  log t "I can add stuff on t also" >>= fun () ->
  log t "Yes. On t!"                >>= fun () ->

  print_logs "branch 2" t >>= fun () ->

  Store.merge ~info:(info "Merging x into t") x ~into:t  >>= function
  | Ok ()   -> print_logs "merge" t
  | Error _ -> failwith "conflict!"

let () =
  Lwt_main.run (main ())
