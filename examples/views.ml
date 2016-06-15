open Lwt
open Irmin_unix

module Store = Irmin_git.FS(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)
module View = Irmin.View(Store)

let fmt t x = Printf.ksprintf (fun str -> t str) x

type t1 = int

type t2 = {
  x: string;
  y: t1;
}

type t = t2 list

let view_of_t t =
  View.empty () >>= fun v ->
  Lwt_list.iteri_s (fun i t2 ->
      let i = string_of_int i in
      View.update v [i;"x"] t2.x >>= fun () ->
      View.update v [i;"y"] (string_of_int t2.y);
    ) t >>= fun () ->
  return v

let t_of_view v =
  let aux acc i =
    let i = string_of_int i in
    View.read_exn v [i;"x"] >>= fun x ->
    View.read_exn v [i;"y"] >>= fun y ->
    return ({ x; y = int_of_string y } :: acc) in
  View.list v [] >>= fun t2s ->
  let t2s = List.map (function
      | [i] -> int_of_string i
      | _   -> assert false
    ) t2s in
  let t2s = List.rev (List.sort compare t2s) in
  Lwt_list.fold_left_s aux [] t2s

let main () =
  Config.init ();
  let config = Irmin_git.config ~root:Config.root ~bare:false () in
  let t = [
    { x = "foo"; y = 3 };
    { x = "bar"; y = 5 };
    { x = "too"; y = 10 };
  ] in
  view_of_t t >>= fun v ->

  Store.Repo.create config >>= Store.master task >>= fun t ->
  View.update_path (t "update a/b") ["a";"b"] v >>= fun () ->
  View.of_path (t "of-path a/b") ["a";"b"] >>= fun v ->
  t_of_view v >>= fun tt ->

  View.update_path (t "update a/c") ["a";"c"] v >>= fun () ->

  let tt = tt @ [ { x = "ggg"; y = 4 } ] in
  view_of_t tt >>= fun vv ->
  View.rebase_exn vv ~into:v >>= fun () ->
  View.merge_path_exn (t "merge view into a/b") ["a";"b"] v >>= fun () ->

  return_unit

let () =
  Lwt_main.run (main ())
