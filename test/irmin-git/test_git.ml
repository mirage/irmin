(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Irmin.Export_for_backends

let test_db = Filename.concat "_build" "test-db-git"

let config =
  let head = Git.Reference.v "refs/heads/test" in
  Irmin_git.config ~head ~bare:true test_db

module type S = sig
  include Irmin_test.S

  val init : unit -> unit Lwt.t
end

module type G = sig
  include S
  module Git : Irmin_git.G
end

module X = struct
  type t = X of (int * int) | Y of string list [@@deriving irmin]

  let merge = Irmin.Merge.idempotent [%typ: t option]
end

module type X =
  Irmin.S
    with type Schema.Path.step = string
     and type Schema.Path.t = string list
     and type Schema.Contents.t = X.t
     and type Schema.Branch.t = string

module Mem (C : Irmin.Contents.S) = struct
  module G = Irmin_git.Mem
  module M = Irmin_git.KV (G) (Git_unix.Sync (G) (Git_cohttp_unix))
  module S = M.Make (C)
  include S

  let init () =
    Git.v (Fpath.v test_db) >>= function
    | Ok t -> S.Git.reset t >|= fun _ -> ()
    | _ -> Lwt.return_unit
end

module Generic (C : Irmin.Contents.S) = struct
  module CA = Irmin.Content_addressable.Make (Irmin_mem.Append_only)
  module M = Irmin_git.Generic_KV (CA) (Irmin_mem.Atomic_write)
  include M.Make (C)

  let init () =
    let* repo = Repo.v config in
    Repo.branches repo >>= Lwt_list.iter_p (Branch.remove repo)

  let clean () =
    let* repo = Repo.v config in
    Repo.branches repo >>= Lwt_list.iter_p (Branch.remove repo) >>= fun () ->
    Repo.close repo
end

let suite =
  let module S = Mem (Irmin.Contents.String) in
  let store = (module S : Irmin_test.S) in
  let init () = S.init () in
  let clean () = S.init () in
  let stats = None in
  {
    Irmin_test.name = "GIT";
    clean;
    init;
    store;
    stats;
    config;
    layered_store = None;
  }

let suite_generic =
  let module S = Generic (Irmin.Contents.String) in
  let store = (module S : Irmin_test.S) in
  let clean () = S.clean () in
  let init () = S.init () in
  let stats = None in
  {
    Irmin_test.name = "GIT.generic";
    clean;
    init;
    store;
    stats;
    config;
    layered_store = None;
  }

let get = function Some x -> x | None -> Alcotest.fail "get"

let test_sort_order (module S : S) =
  S.init () >>= fun () ->
  let* repo = S.Repo.v (Irmin_git.config test_db) in
  let commit_t = S.Private.Repo.commit_t repo in
  let node_t = S.Private.Repo.node_t repo in
  let head_tree_id branch =
    let* head = S.Head.get branch in
    let+ commit = S.Private.Commit.find commit_t (S.Commit.key head) in
    S.Private.Commit.Val.node (get commit)
  in
  let ls branch =
    let* tree_id = head_tree_id branch in
    let+ tree = S.Private.Node.find node_t tree_id in
    S.Private.Node.Val.list (get tree) |> List.map fst
  in
  let info = S.Info.none in
  let* master = S.master repo in
  S.remove_exn master ~info [] >>= fun () ->
  S.set_exn master ~info [ "foo.c" ] "foo.c" >>= fun () ->
  S.set_exn master ~info [ "foo1" ] "foo1" >>= fun () ->
  S.set_exn master ~info [ "foo"; "foo.o" ] "foo.o" >>= fun () ->
  let* items = ls master in
  Alcotest.(check (list string)) "Sort order" [ "foo.c"; "foo"; "foo1" ] items;
  let* tree_id = head_tree_id master in
  Alcotest.(check string)
    "Sort hash" "00c5f5e40e37fde61911f71373813c0b6cad1477"
    (Irmin.Type.to_string S.Private.Node.Key.t tree_id);

  (* Convert dir to file; changes order in listing *)
  S.set_exn master ~info [ "foo" ] "foo" >>= fun () ->
  let* items = ls master in
  Alcotest.(check (list string)) "Sort order" [ "foo"; "foo.c"; "foo1" ] items;
  Lwt.return_unit

module Ref (S : Irmin_git.G) = struct
  module M = Irmin_git.Ref (S) (Git_unix.Sync (S) (Git_cohttp_unix))
  include M.Make (Irmin.Contents.String)
end

let pp_reference ppf = function
  | `Branch s -> Fmt.pf ppf "branch: %s" s
  | `Remote s -> Fmt.pf ppf "remote: %s" s
  | `Tag s -> Fmt.pf ppf "tag: %s" s
  | `Other s -> Fmt.pf ppf "other: %s" s

let reference = Alcotest.testable pp_reference ( = )

let test_list_refs (module S : G) =
  let module R = Ref (S.Git) in
  S.init () >>= fun () ->
  let* repo = R.Repo.v (Irmin_git.config test_db) in
  let* master = R.master repo in
  R.set_exn master ~info:R.Info.none [ "test" ] "toto" >>= fun () ->
  let* head = R.Head.get master in
  R.Branch.set repo (`Remote "datakit/master") head >>= fun () ->
  R.Branch.set repo (`Other "foo/bar/toto") head >>= fun () ->
  R.Branch.set repo (`Branch "foo") head >>= fun () ->
  let* bs = R.Repo.branches repo in
  Alcotest.(check (slist reference compare))
    "raw branches"
    [
      `Branch "foo";
      `Branch "master";
      `Other "foo/bar/toto";
      `Remote "datakit/master";
    ]
    bs;
  let* repo = S.Repo.v (Irmin_git.config test_db) in
  let* bs = S.Repo.branches repo in
  Alcotest.(check (slist string String.compare))
    "filtered branches" [ "master"; "foo" ] bs;

  (* XXX: re-add
     if S.Git.kind = `Disk then
       let i = Fmt.kstrf Sys.command "cd %s && git gc" test_db in
       if i <> 0 then Alcotest.fail "git gc failed";
       S.Repo.branches repo >|= fun bs ->
       Alcotest.(check (slist string String.compare)) "filtered branches"
         ["master";"foo"] bs
      else *)
  Lwt.return_unit

let bin_string = Alcotest.testable (Fmt.fmt "%S") ( = )

let pre_hash t v =
  let buf = Buffer.create 13 in
  let pre_hash = Irmin.Type.(unstage (pre_hash t)) in
  pre_hash v (Buffer.add_string buf);
  Buffer.contents buf

let test_blobs (module S : S) =
  let str = pre_hash S.Contents.t "foo" in
  Alcotest.(check bin_string) "blob foo" "blob 3\000foo" str;
  let str = pre_hash S.Contents.t "" in
  Alcotest.(check bin_string) "blob ''" "blob 0\000" str;
  let module X = Mem (X) in
  let str = pre_hash X.Contents.t (Y [ "foo"; "bar" ]) in
  Alcotest.(check bin_string)
    "blob foo" "blob 19\000{\"Y\":[\"foo\",\"bar\"]}" str;
  let str = pre_hash X.Contents.t (X (1, 2)) in
  Alcotest.(check bin_string) "blob ''" "blob 11\000{\"X\":[1,2]}" str;
  let t = X.Tree.empty in
  let* t = X.Tree.add t [ "foo" ] (X (1, 2)) in
  let k1 = X.Tree.key t in
  let* repo = X.Repo.v (Irmin_git.config test_db) in
  let* k2 =
    X.Private.Repo.batch repo (fun x y _ -> X.save_tree ~clear:false repo x y t)
  in
  let key = Irmin_test.testable X.Hash.t in
  Alcotest.(check key) "blob" k1 k2;
  Lwt.return_unit

let test_import_export (module S : S) =
  let module Generic = Generic (Irmin.Contents.String) in
  let module Sync = Irmin.Sync.Make (Generic) in
  S.init () >>= fun () ->
  let* _ = Generic.init () in
  let* repo = S.Repo.v (Irmin_git.config test_db) in
  let* t = S.master repo in
  S.set_exn t ~info:S.Info.none [ "test" ] "toto" >>= fun () ->
  let remote = Irmin.remote_store (module S) t in
  let* repo = Generic.Repo.v (Irmin_mem.config ()) in
  let* t = Generic.master repo in
  let* _ = Sync.pull_exn t remote `Set in
  let+ toto = Generic.get t [ "test" ] in
  Alcotest.(check string) "import" toto "toto"

let misc (module S : G) =
  let s = (module S : S) in
  let g = (module S : G) in
  let generic = (module Generic (Irmin.Contents.String) : S) in
  let run f x () = Lwt_main.run (f x) in
  [
    ("Testing sort order", `Quick, run test_sort_order s);
    ("Testing sort order (generic)", `Quick, run test_sort_order generic);
    ("Testing listing refs", `Quick, run test_list_refs g);
    ("git -> mem", `Quick, run test_import_export s);
    ("git blobs", `Quick, run test_blobs s);
    ("git blobs of generic", `Quick, run test_blobs s);
  ]

let mem = (module Mem (Irmin.Contents.String) : G)
