(*

  Simple example showing how to define and use a custom merge operator.

  $ make                               # Compile
  $ ./custom_merge                     # Run
  $ cd /tmp/irmin/test && git log      # Show the Git history

*)


open Lwt
open Core_kernel.Std

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

module Log = struct

  (* A log file is a list of timestamped message (one per line). *)

  module Elt = struct

    type t = {
      timestamp: int;
      message  : string;
    } with bin_io, compare, sexp

    let output_buffer b elt =
      Printf.bprintf b "%d %s\n" elt.timestamp elt.message

    let of_string str =
      match String.lsplit2 str ~on:' ' with
      | None              -> failwith (str ^ " is not a valid log line.")
      | Some (t, message) -> { timestamp = int_of_string t; message }

  end

  module M = IrminIdent.Make(struct
      type t = Elt.t list with compare, sexp
    end)
  include M

  let equal e1 e2 =
    e1 = e2

  let to_string t =
    let b = Buffer.create 1024 in
    List.iter ~f:(Elt.output_buffer b) t;
    Buffer.contents b

  let of_string str =
    let l = String.split str ~on:'\n' in
    let l = List.filter ~f:(fun s -> String.(s <>"")) l in
    List.map ~f:Elt.of_string l

  let merge_t ~old:_ t1 t2 =
    let map t =
      let explode e = e.Elt.timestamp, e.Elt.message in
      let l = List.map ~f:explode t in
      Int.Map.of_alist_exn l in
    let list m =
      let implode (timestamp, message) = { Elt.timestamp; message } in
      let l = Int.Map.to_alist m in
      let l = List.map ~f:implode l in
      List.sort ~cmp:Elt.compare l in
    let m1 = map t1 in
    let m2 = map t2 in
    let m3 = Int.Map.merge m1 m2 ~f:(fun ~key -> function
        | `Left v | `Right v -> Some v
        | `Both (v1, v2)     ->
          if String.(v1 = v2) then Some v1
          else Some (v1 ^ " -- " ^ v2)
      ) in
    `Ok (list m3)

  let merge = IrminMerge.create (module M) merge_t

end

module Config = struct
  let root = Some "/tmp/irmin/test"
  module Store = Git_fs
  let bare = true
  let disk = true
end

module Git = IrminGit.Make(Config)

module Store = Git.Make(IrminKey.SHA1)(Log)(IrminTag.String)

let main () =
  let path = ["local"; "me"] in
  let mk timestamp message = { Log.Elt.timestamp; message } in
  let m1 = mk 1 "foo" in
  let m2 = mk 2 "bar" in
  let m2' = mk 2 "hoho" in
  let m3 = mk 3 "haha" in
  let m4 = mk 4 "xxxx" in

  Store.create () >>= fun t ->

  let add t message =
    begin
      Store.read t path >>= function
      | None   -> return_nil
      | Some l -> return l
    end >>= fun logs ->
    Store.update t path (message :: logs) in

  add t m1 >>= fun () ->
  Store.read_exn t path  >>= fun logs ->
  Printf.printf "I've just read:\n-----------\n%s-----------\n%!" (Log.to_string logs);

  Store.clone_force t "refs/heads/test" >>= fun x ->
  add x m2 >>= fun () ->
  add t m2' >>= fun () ->
  add t m3  >>= fun () ->
  add x m4  >>= fun () ->

  Store.merge_exn t (Store.branch x) >>= fun () ->

  Store.read_exn t path >>= fun logs ->
  Printf.printf "I've just read:\n%s\n%!" (Log.to_string logs);

  return_unit

let () =
  Lwt_unix.run (main ())
