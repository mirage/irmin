module Contents = Irmin.Contents.String

module Conf = struct
  let entries = 4
  let stable_hash = 256
  let contents_length_header = Some `Varint
  let inode_child_order = `Seeded_hash
  let forbid_empty_dir_persistence = true
end

let root = "/tmp/irmin-db"

let config ~fresh ~sw ~fs =
  Irmin_pack.config ~fresh ~sw ~fs Eio.Path.(fs / root)

module StoreMaker = Irmin_pack_unix.KV (Conf)
module Store = StoreMaker.Make (Contents)

let date = ref 0L

let info () =
  let info = Store.Info.v ~author:"foo" ~message:"bar" !date in
  date := Int64.add !date 3600L;
  info

let set sw fs =
  let conf = config ~fresh:true ~sw ~fs in
  Fmt.pr "Store.Repo.v@.";
  let repo = Store.Repo.v conf in
  Fmt.pr "Store.main@.";
  let main = Store.main repo in
  let tree = Store.tree main in
  let tree = Store.Tree.add tree [ "a"; "b" ] "Hello" in
  let tree = Store.Tree.add tree [ "a"; "c" ] "!" in
  (* let tree = Store.Tree.add tree [ "a"; "d"; "e" ] "World" in
  let tree = Store.Tree.add tree [ "a"; "f" ] "!" in *)
  Store.set_tree_exn ~info main [] tree;
  Fmt.pr "Store.close@.";
  Store.Repo.close repo

(* let get sw fs =
  let conf = config ~fresh:false ~sw ~fs in
  Fmt.pr "Store.Repo.v@.";
  let repo = Store.Repo.v conf in
  Fmt.pr "Store.main@.";
  let main = Store.main repo in
  let value = "Hello" in
  Fmt.pr "Store.get_exn %S@." value;
  let s = Store.get main [ "a"; "b"; "c" ] in
  assert (s = value);
  let value = "World" in
  Fmt.pr "Store.get_exn %S@." value;
  let s = Store.get main [ "a"; "b"; "d" ] in
  assert (s = value);
  let value = "!" in
  Fmt.pr "Store.get_exn %S@." value;
  let s = Store.get main [ "a"; "e" ] in
  assert (s = value);
  Fmt.pr "Store.close@.";
  Store.Repo.close repo *)

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  Fmt_tty.setup_std_outputs ();
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  (* Logs.set_reporter (Logs_fmt.reporter ()); *)
  Logs.(set_level @@ Some Debug);
  set sw fs
(* get sw fs *)

(*
    "a" |-> "b" |-> "c" = "Hello"
        |       |-> "d" = "World"
        |-> "e"         = "!"
*)
