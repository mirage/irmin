(*

  Simple example showing how to create and use a Git store.

  $ make                               # Compile
  $ ./views                            # Run
  $ cd /tmp/irmin/test && git log      # Show the Git history

*)

open Lwt

let path = "/tmp/irmin/test"
module Git = IrminGit.Make(IrminKey.SHA1)(IrminContents.String)(IrminReference.String)
module Store = (val Git.create ~bare:false ~kind:`Disk ~root:path ())

type t1 = int

type t2 = {
  x: string;
  y: t1;
}

type t = t2 list

let view_of_t t =
  Store.View.create () >>= fun v ->
  Lwt_list.iteri_s (fun i t2 ->
      let i = string_of_int i in
      Store.View.update v [i] t2.x >>= fun () ->
      Store.View.update v [i;"y"] (string_of_int t2.y);
    ) t >>= fun () ->
  return v

let t_of_view v =
  let aux acc i =
    let i = string_of_int i in
    Store.View.read_exn v [i]     >>= fun x ->
    Store.View.read_exn v [i;"y"] >>= fun y ->
    return ({ x; y = int_of_string y } :: acc) in
  Store.View.list v [[]] >>= fun t2s ->
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

  Store.create () >>= fun t ->
  Store.update_view t ["a";"b"] v >>= fun () ->

  Store.read_view t ["a";"b"] >>= fun v ->
  t_of_view v >>= fun tt ->

  let tt = { x = "ggg"; y = 4 } :: tt in
  view_of_t tt >>= fun vv ->
  Store.merge_view_exn t ["a";"c"] vv

let () =
  Lwt_unix.run (main ())
