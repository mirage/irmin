(* Connect to http://localhost:8080/dump *)

open Lwt

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

let () =
  let origin =
    Printf.sprintf "Irminsule (%s[%d])" (Unix.gethostname()) (Unix.getpid()) in
  IrminOrigin.set_date (fun () -> Int64.of_float (Unix.time ()));
  IrminOrigin.set_id (fun () -> origin);
  IrminOrigin.set_string_of_date (fun d ->
      let tm = Unix.localtime (Int64.to_float d) in
      Printf.sprintf "%2d:%2d:%2d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    );
  IrminFS.install_dir_polling_listener 0.5

module Config = struct
  let root = Some "/tmp/irmin/test"
  module Store = Git_fs
  let bare = true
  let disk = true
end

module Git = IrminGit.Make(Config)
module Store = Git.Make(IrminKey.SHA1)(IrminContents.String)(IrminTag.String)

let o id fmt = IrminOrigin.create ~id fmt
let fin () =
  let _ = Sys.command "cd /tmp/irmin/test && git reset HEAD --hard" in
  return_unit

let commands = [|
  "Getting an incoming connection";
  "Reading the contents of the cache";
  "sudo apt-get upgrade";
  "rsync filer://myfiles.com/me";
|]

let branches = [|
      "12345/cron";
      "112323/maidir";
      "1333/apache";
|]

let master = branches.(0)

let init () =
  let _ = Sys.command "rm -rf /tmp/irmin/test" in
  let _ = Sys.command "mkdir -p /tmp/irmin/test" in
  Store.create ~branch:master () >>= fun t ->
  Store.update t ["log"; master; "0"] (master ^ ":0") >>= fun () ->
  Lwt_list.iter_s (fun branch ->
      Store.create ~branch () >>= fun t ->
      Store.switch t branches.(0)
    ) (Array.to_list branches)

let random_array a =
  a.(Random.int (Array.length a))

let rec process ~id count =
  Store.create ~branch:id () >>= fun t ->
  Store.update t ["log"; id; string_of_int count] (id ^ ":" ^ (string_of_int count))
    ~origin:(o id "%s" (random_array commands))
  >>= fun () ->

  begin if Random.int 2 = 0 then
    let branch = random_array branches in
    Store.merge_exn t branch ~origin:(o id "merging %s and %s" id branch)
  else
    return_unit
  end >>= fun () ->

  Lwt_unix.sleep (Random.float 10.)
  >>= fun () ->
  process ~id (count+1)

let () =
  let aux () =
    init () >>= fun () ->
    Lwt.join (List.map
                (fun id -> process ~id 1)
                (Array.to_list branches))
  in
  Lwt_unix.run (aux ())
