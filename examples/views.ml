(*

  Simple example showing how to create and use a Git store.

  $ make                               # Compile
  $ ./views                            # Run
  $ cd /tmp/irmin/test && git log      # Show the Git history

*)

open Lwt
open Irmin_unix

(* Enable debug outputs if DEBUG is set *)
let () =
  try match Sys.getenv "DEBUG" with
    | "" -> ()
    | _  ->
      Log.color_on ();
      Log.set_log_level Log.DEBUG
  with Not_found -> ()

module Store = Irmin.Basic (Irmin_git.FS) (Irmin.Contents.String)
module View = Irmin.View(Store)

let fmt t x = Printf.ksprintf (fun str -> t str) x

type t1 = int

type t2 = {
  x: string;
  y: t1;
}

type t = t2 list

let view_of_t t =
  Log.debug "view_of_t";
  View.create task >>= fun v ->
  Lwt_list.iteri_s (fun i t2 ->
      let i = string_of_int i in
      View.update (fmt v "update %s/x" i) [i;"x"] t2.x >>= fun () ->
      View.update (fmt v "update %s/y" i) [i;"y"] (string_of_int t2.y);
    ) t >>= fun () ->
  return v

let t_of_view v =
  Log.debug "t_of_view";
  let aux acc i =
    let i = string_of_int i in
    View.read_exn (fmt v "read %s/x" i) [i;"x"] >>= fun x ->
    View.read_exn (fmt v "read %s/y" i) [i;"y"] >>= fun y ->
    return ({ x; y = int_of_string y } :: acc) in
  View.list (v "list") [] >>= fun t2s ->
  let t2s = List.map (function
      | [i] -> int_of_string i
      | _   -> assert false
    ) t2s in
  let t2s = List.rev (List.sort compare t2s) in
  Lwt_list.fold_left_s aux [] t2s

let main () =
  let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:false () in
  let t = [
    { x = "foo"; y = 3 };
    { x = "bar"; y = 5 };
    { x = "too"; y = 10 };
  ] in
  view_of_t t >>= fun v ->

  Store.create config task >>= fun t ->
  View.update_path "update a/b" t ["a";"b"] v >>= fun () ->
  View.of_path task (t "of-path a/b") ["a";"b"] >>= fun v ->
  t_of_view v >>= fun tt ->

  View.update_path "update a/c" t ["a";"c"] v >>= fun () ->

  let tt = tt @ [ { x = "ggg"; y = 4 } ] in
  view_of_t tt >>= fun vv ->
  View.rebase_exn "Rebase views" vv ~into:v >>= fun () ->
  View.merge_path_exn "merge view into a/b" t ["a";"b"] v >>= fun () ->

  return_unit

let () =
  Lwt_unix.run (main ())
