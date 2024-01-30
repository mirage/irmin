(*
 * Copyright (c) 2022 Tarides <contact@tarides.com>
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

(* example of using tree fold *)

module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Tree = Store.Tree

let config = Irmin_mem.config ()

let info =
  let counter = ref 0L in
  let inc () =
    let c = !counter in
    counter := Int64.add c 1L;
    c
  in
  fun message () -> Store.Info.v ~author:"fold.exe" ~message (inc ())

module Folder : sig
  (* Not accumulating anything so use unit as accumulator type *)
  val pre : (unit, Store.step list) Tree.folder
  val post : (unit, Store.step list) Tree.folder
  val node : (unit, Store.node) Tree.folder
  val contents : (unit, Store.contents) Tree.folder
  val tree : (unit, Store.tree) Tree.folder
end = struct
  let print_path newline path _ _ =
    let format : ('a, Format.formatter, unit) format =
      "Visit [%s]" ^^ if newline then "\n" else ""
    in
    Fmt.(pf stdout format (String.concat ";" path))

  let pre = print_path true
  let post = print_path true
  let node = print_path true

  let contents path c acc =
    print_path false path c acc;
    Fmt.(pf stdout " = '%s'\n" c)

  let tree path t acc =
    print_path false path t acc;
    match Tree.kind t [] with
    | Some k' -> (
        match k' with
        | `Node -> Fmt.(string stdout ", with `Node kind\n")
        | `Contents -> Fmt.(string stdout ", with `Contents kind\n"))
    | None -> failwith "no kind"
end

let main () =
  let ps name = Fmt.(pf stdout "\n%s\n" name) in
  ps "Demo of how tree folders visit nodes.";
  Eio.Switch.run @@ fun sw ->
  let repo = Store.Repo.v ~sw config in
  let main_b = Store.main repo in
  Store.set_exn ~info:(info "add c1") main_b [ "c1" ] "c1";
  Store.set_exn ~info:(info "add c2") main_b [ "c2" ] "c2";

  Store.set_exn ~info:(info "add n1/c1") main_b [ "n1"; "c1" ] "n1/c1";
  Store.set_exn ~info:(info "add n1/n1/c1") main_b [ "n1"; "n1"; "c1" ]
    "n1/n1/c1";
  Store.set_exn ~info:(info "add n2/c1") main_b [ "n2"; "c1" ] "n2/c1";
  let t = Store.tree main_b in
  (* let order = `Random (Random.State.make_self_init ()) in *)
  let order = `Sorted in
  ps "pre folder: preorder traversal of `Node kinds";
  Tree.fold ~order ~pre:Folder.pre t ();
  ps "post folder: postorder traversal of `Node kinds";
  Tree.fold ~order ~post:Folder.post t ();
  ps "node folder: visit all `Node kinds";
  Tree.fold ~order ~node:Folder.node t ();
  ps "contents folder: visit all `Contents kinds";
  Tree.fold ~order ~contents:Folder.contents t ();
  ps "tree folder: visit both `Node and `Contents kinds";
  Tree.fold ~order ~tree:Folder.tree t ()

let () =
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ -> main ()
