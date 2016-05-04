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

open Lwt
open Irmin_unix

let time = ref 0

let failure fmt = Printf.ksprintf failwith fmt

(* A log entry *)
module Entry: sig
  include Tc.S0
  val pretty: Buffer.t -> t -> unit
  val create: string -> t
  val timestamp: t -> int
end = struct

  let err_malformed_timestamp = failure "malformed timestamp: %S"

  type t = {
    timestamp: int;
    message  : string;
  }

  let timestamp t = t.timestamp
  let compare x y = Pervasives.compare x.timestamp y.timestamp
  let hash = Hashtbl.hash
  let equal = (=)

  let pretty buf { timestamp; message } =
    Printf.bprintf buf "%04d: %s\n" timestamp message

  let to_json t =
    `O [
      "timestamp", Ezjsonm.int t.timestamp;
      "message"  , Ezjsonm.string t.message;
    ]

  let of_json j =
    let timestamp = Ezjsonm.find j ["timestamp"] |> Ezjsonm.get_int in
    let message   = Ezjsonm.find j ["message"]   |> Ezjsonm.get_string in
    { timestamp; message }

  let header_len = 19

  let to_timestamp i = Printf.sprintf "%.*d" header_len i
  let of_timestamp s =
    if String.length s <> header_len then err_malformed_timestamp s
    else
      try int_of_string s
      with Failure _ -> err_malformed_timestamp s

  let read buf =
    let timestamp = Mstruct.get_string buf header_len |> of_timestamp in
    let message =
      match Mstruct.get_string_delim buf '\n' with
      | None   -> Mstruct.to_string buf
      | Some m -> m
    in
    let message = String.trim message in
    { timestamp; message }

  let size_of { message; _ } = header_len + String.length message + 2

  let write t buf0 =
    let buf = Mstruct.of_cstruct buf0 in
    let timestamp = to_timestamp t.timestamp in
    Mstruct.set_string buf timestamp;
    Mstruct.set_char buf ' ';
    Mstruct.set_string buf t.message;
    Mstruct.set_char buf '\n';
    Cstruct.shift buf0 (size_of t)

  let create message =
    incr time;
    { timestamp = !time; message }

end

(* A log file *)
module Log: sig
  include Irmin.Contents.S with type Path.t = string list
  val pretty: t -> string
  val add: t -> Entry.t -> t
  val empty: t
end = struct

  module Path = Irmin.Path.String_list
  module S = Tc.List(Entry)

  type t = Entry.t list
  let hash = Hashtbl.hash
  let compare = S.compare
  let equal = S.equal
  let empty = []
  let to_json = S.to_json
  let of_json = S.of_json

  let size_of l = List.fold_left (fun acc e -> acc + Entry.size_of e) 0 l

  let write l buf =
    List.fold_left (fun buf e -> Entry.write e buf) buf (List.rev l)

  let read buf =
    let rec aux acc =
      if Mstruct.length buf = 0 then List.rev acc
      else aux (Entry.read buf :: acc)
    in
    List.rev (aux [])

  let pretty l =
    let buf = Buffer.create 1024 in
    List.iter (Entry.pretty buf) (List.rev l);
    Buffer.contents buf

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

  let merge _path ~old t1 t2 =
    let open Irmin.Merge.OP in
    old () >>| fun old ->
    let old = match old with None -> [] | Some o -> o in
    let ts = timestamp old in
    let t1 = newer_than ts t1 in
    let t2 = newer_than ts t2 in
    let t3 = List.sort Entry.compare (List.rev_append t1 t2) in
    ok (List.rev_append t3 old)

  let merge path = Irmin.Merge.option (module S) (merge path)

  let add t e = e :: t

end

module Store = Irmin_git.FS(Log)(Irmin.Ref.String)(Irmin.Hash.SHA1)
let config = Irmin_git.config ~root:Config.root ~bare:true ()

let log_file = [ "local"; "debug" ]

let all_logs t =
  Store.read (t "Reading the log file") log_file >>= function
  | None   -> return Log.empty
  | Some l -> return l

(* Persist a new entry in the log. Pretty inefficient as it
   reads/writes the whole file every time. *)
let log t fmt =
  Printf.ksprintf (fun message ->
      all_logs t >>= fun logs ->
      let logs = Log.add logs (Entry.create message) in
      Store.update (t "Adding a new entry") log_file logs
    ) fmt

let print_logs name t =
  all_logs t >>= fun logs ->
  Printf.printf "-----------\n%s:\n-----------\n%s%!" name (Log.pretty logs);
  Lwt.return_unit

let main () =
  Config.init ();
  Store.Repo.create config >>= Store.master task >>= fun t ->

  (* populate the log with some random messages *)
  Lwt_list.iter_s (fun msg ->
      log t "This is my %s " msg
    ) [ "first"; "second"; "third"]
  >>= fun () ->

  Printf.printf "%s\n\n" what;

  print_logs "lca" t >>= fun () ->

  Store.clone_force task (t "Cloning the store") "test" >>= fun x ->

  log x "Adding new stuff to x"  >>= fun () ->
  log x "Adding more stuff to x" >>= fun () ->
  log x "More. Stuff. To x."     >>= fun () ->

  print_logs "branch 1" x >>= fun () ->

  log t "I can add stuff on t also" >>= fun () ->
  log t "Yes. On t!"                >>= fun () ->

  print_logs "branch 2" t >>= fun () ->

  Store.merge_exn "Merging x into t" x ~into:t  >>= fun () ->

  print_logs "merge" t

let () =
  Lwt_main.run (main ())
