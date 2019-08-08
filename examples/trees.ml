open Lwt.Infix

let info = Irmin_unix.info

module Store = Irmin_unix.Git.FS.KV (Irmin.Contents.String)
module Tree = Store.Tree

type t1 = int

type t2 = { x : string; y : t1 }

type t = t2 list

let tree_of_t t =
  Lwt_list.fold_left_s
    (fun (v, i) t2 ->
      let si = string_of_int i in
      Tree.add v [ si; "x" ] t2.x >>= fun v ->
      Tree.add v [ si; "y" ] (string_of_int t2.y) >|= fun v -> (v, i + 1))
    (Tree.empty, 0) t
  >|= fun (v, _) -> v

let t_of_tree v =
  let aux acc i =
    let i = string_of_int i in
    Tree.get v [ i; "x" ] >>= fun x ->
    Tree.get v [ i; "y" ] >|= fun y -> { x; y = int_of_string y } :: acc
  in
  Tree.list v [] >>= fun t2s ->
  let t2s = List.map (fun (i, _) -> int_of_string i) t2s in
  let t2s = List.rev (List.sort compare t2s) in
  Lwt_list.fold_left_s aux [] t2s

let main () =
  Config.init ();
  let config = Irmin_git.config ~bare:false Config.root in
  let t =
    [ { x = "foo"; y = 3 }; { x = "bar"; y = 5 }; { x = "too"; y = 10 } ]
  in
  tree_of_t t >>= fun v ->
  Store.Repo.v config >>= fun repo ->
  Store.master repo >>= fun t ->
  Store.set_tree_exn t ~info:(info "update a/b") [ "a"; "b" ] v >>= fun () ->
  Store.get_tree t [ "a"; "b" ] >>= fun v ->
  t_of_tree v >>= fun tt ->
  Store.set_tree_exn t ~info:(info "update a/c") [ "a"; "c" ] v >>= fun () ->
  let tt = tt @ [ { x = "ggg"; y = 4 } ] in
  tree_of_t tt >>= fun vv ->
  Store.set_tree_exn t ~info:(info "merge tree into a/b") [ "a"; "b" ] vv

let () = Lwt_main.run (main ())
