(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

(* example of using the tree API *)

module Store = Irmin_git_unix.FS.KV (Irmin.Contents.String)
module Tree = Store.Tree

let info = Irmin_git_unix.info

type t1 = int
type t2 = { x : string; y : t1 }
type t = t2 list

let tree_of_t t =
  let tree, _ =
    List.fold_left
      (fun (v, i) t2 ->
        let si = string_of_int i in
        let v = Tree.add v [ si; "x" ] t2.x in
        let v = Tree.add v [ si; "y" ] (string_of_int t2.y) in
        (v, i + 1))
      (Tree.empty (), 0)
      t
  in
  tree

let t_of_tree v =
  let aux acc i =
    let i = string_of_int i in
    let x = Tree.get v [ i; "x" ] in
    let y = Tree.get v [ i; "y" ] in
    { x; y = int_of_string y } :: acc
  in
  let t2s = Tree.list v [] in
  let t2s = List.map (fun (i, _) -> int_of_string i) t2s in
  let t2s = List.rev (List.sort compare t2s) in
  List.fold_left aux [] t2s

let main () =
  Eio.Switch.run @@ fun sw ->
  Config.init ();
  let config = Irmin_git.config ~bare:false Config.root in
  let t =
    [ { x = "foo"; y = 3 }; { x = "bar"; y = 5 }; { x = "too"; y = 10 } ]
  in
  let v = tree_of_t t in
  let repo = Store.Repo.v ~sw config in
  let t = Store.main repo in
  Store.set_tree_exn t ~info:(info "update a/b") [ "a"; "b" ] v;
  let v = Store.get_tree t [ "a"; "b" ] in
  let tt = t_of_tree v in
  Store.set_tree_exn t ~info:(info "update a/c") [ "a"; "c" ] v;
  let tt = tt @ [ { x = "ggg"; y = 4 } ] in
  let vv = tree_of_t tt in
  Store.set_tree_exn t ~info:(info "merge tree into a/b") [ "a"; "b" ] vv

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
