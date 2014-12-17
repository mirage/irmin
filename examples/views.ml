(*

  Simple example showing how to create and use a Git store.

  $ make                               # Compile
  $ ./views                            # Run
  $ cd /tmp/irmin/test && git log      # Show the Git history

*)

open Lwt
open Irmin_unix

module Store = Irmin.Default(Irmin_git.FS)(Irmin.Contents.String)
module View = Irmin.View(Store)

let config = Irmin_git.config ~root:"/tmp/irmin/test" ~bare:true ()
let fmt t x = Printf.ksprintf (fun str -> t str) x

type t1 = int

type t2 = {
  x: string;
  y: t1;
}

type t = t2 list

let view_of_t t =
  View.create config task >>= fun v ->
  Lwt_list.iteri_s (fun i t2 ->
      let i = string_of_int i in
      View.update (fmt v "update %s" i)  [i]     t2.x >>= fun () ->
      View.update (fmt v "update %s/y" i)[i;"y"] (string_of_int t2.y);
    ) t >>= fun () ->
  return v

let t_of_view v =
  let aux acc i =
    let i = string_of_int i in
    View.read_exn v [i]       >>= fun x ->
    View.read_exn v [i;"y"] >>= fun y ->
    return ({ x; y = int_of_string y } :: acc) in
  View.list v [] >>= fun t2s ->
  let t2s = List.map (function
      | [i] -> int_of_string i
      | _   -> assert false
    ) t2s in
  let t2s = List.sort compare t2s in
  Lwt_list.fold_left_s aux [] t2s

let main () =

  let t = [
    { x = "foo"; y = 3 };
    { x = "bar"; y = 5 };
    { x = "too"; y = 10 };
  ] in
  view_of_t t >>= fun v ->

  Store.create config task >>= fun t ->
  View.update_path (t "view-update-path a/b") ["a";"b"] (v "update") >>= fun () ->

  View.of_path (t "view-of-path a/b") ["a";"b"] >>= fun v ->
  t_of_view v >>= fun tt ->

  let tt = { x = "ggg"; y = 4 } :: tt in
  view_of_t tt >>= fun vv ->
  View.merge_path_exn (t "merge view") ["a";"c"] (vv "merge")

let () =
  Lwt_unix.run (main ())
