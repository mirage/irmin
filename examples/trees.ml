open Lwt.Infix

let ( let* ) x f = Lwt.bind x f
let ( let+ ) x f = Lwt.map f x
let info = Irmin_unix.info

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Tree = Store.Tree

type t1 = int
type t2 = { x : string; y : t1 }
type t = t2 list

let tree_of_t t =
  let+ tree, _ =
    Lwt_list.fold_left_s
      (fun (v, i) t2 ->
        let si = string_of_int i in
        let* v = Tree.add v [ si; "x" ] t2.x in
        let+ v = Tree.add v [ si; "y" ] (string_of_int t2.y) in
        (v, i + 1))
      (Tree.empty (), 0)
      t
  in
  tree

let t_of_tree v =
  let aux acc i =
    let i = string_of_int i in
    let* x = Tree.get v [ i; "x" ] in
    let+ y = Tree.get v [ i; "y" ] in
    { x; y = int_of_string y } :: acc
  in
  let* t2s = Tree.list v [] in
  let t2s = List.map (fun (i, _) -> int_of_string i) t2s in
  let t2s = List.rev (List.sort compare t2s) in
  Lwt_list.fold_left_s aux [] t2s

let main () =
  Config.init ();
  let config = Irmin_git.config ~bare:false Config.root in
  let t =
    [ { x = "foo"; y = 3 }; { x = "bar"; y = 5 }; { x = "too"; y = 10 } ]
  in
  let* v = tree_of_t t in
  let* repo = Store.Repo.v config in
  let* t = Store.master repo in
  Store.set_tree_exn t ~info:(info "update a/b") [ "a"; "b" ] v >>= fun () ->
  let* v = Store.get_tree t [ "a"; "b" ] in
  let* tt = t_of_tree v in
  Store.set_tree_exn t ~info:(info "update a/c") [ "a"; "c" ] v >>= fun () ->
  let tt = tt @ [ { x = "ggg"; y = 4 } ] in
  let* vv = tree_of_t tt in
  Store.set_tree_exn t ~info:(info "merge tree into a/b") [ "a"; "b" ] vv

let () = Lwt_main.run (main ())
