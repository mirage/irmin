(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let test_db = Filename.concat "_build" "test-db-git"

let config =
  let head = Git.Reference.of_string "refs/heads/test" in
  Irmin_git.config ~head ~bare:true test_db

module Net = Git_unix.Net

module type S = sig
  include Irmin_test.S

  val init : unit -> unit Lwt.t
end

module type G = sig
  include S

  module Git : Irmin_git.G
end

module X = struct
  type t = X of (int * int) | Y of string list

  let t : t Irmin.Type.t =
    let open Irmin.Type in
    variant "test" (fun x y -> function X i -> x i | Y i -> y i)
    |~ case1 "x" (pair int int) (fun x -> X x)
    |~ case1 "y" (list string) (fun y -> Y y)
    |> sealv

  let merge = Irmin.Merge.idempotent Irmin.Type.(option t)
end

module type X =
  Irmin.S
    with type step = string
     and type key = string list
     and type contents = X.t
     and type branch = string

module Mem (C : Irmin.Contents.S) = struct
  module G = Irmin_git.Mem
  module S = Irmin_git.KV (G) (Git_unix.Sync (G)) (C)
  include S

  let init () =
    Git.v (Fpath.v test_db) >>= function
    | Ok t -> S.Git.reset t >|= fun _ -> ()
    | _ -> Lwt.return_unit
end

module Generic (C : Irmin.Contents.S) = struct
  include Irmin_git.Generic_KV
            (Irmin.Content_addressable
               (Irmin_mem.Append_only))
               (Irmin_mem.Atomic_write)
            (C)

  let init () =
    Repo.v config >>= fun repo ->
    Repo.branches repo >>= Lwt_list.iter_p (Branch.remove repo)

  let clean () =
    Repo.v config >>= fun repo ->
    Repo.branches repo >>= Lwt_list.iter_p (Branch.remove repo) >>= fun () ->
    Repo.close repo
end

let suite =
  let module S = Mem (Irmin.Contents.String) in
  let store = (module S : Irmin_test.S) in
  let init () = S.init () in
  let clean () = S.init () in
  let stats = None in
  { Irmin_test.name = "GIT"; clean; init; store; stats; config }

let suite_generic =
  let module S = Generic (Irmin.Contents.String) in
  let store = (module S : Irmin_test.S) in
  let clean () = S.clean () in
  let init () = S.init () in
  let stats = None in
  { Irmin_test.name = "GIT.generic"; clean; init; store; stats; config }

let get = function Some x -> x | None -> Alcotest.fail "get"

let test_sort_order (module S : S) =
  S.init () >>= fun () ->
  S.Repo.v (Irmin_git.config test_db) >>= fun repo ->
  let commit_t = S.Private.Repo.commit_t repo in
  let node_t = S.Private.Repo.node_t repo in
  let head_tree_id branch =
    S.Head.get branch >>= fun head ->
    S.Private.Commit.find commit_t (S.Commit.hash head) >|= fun commit ->
    S.Private.Commit.Val.node (get commit)
  in
  let ls branch =
    head_tree_id branch >>= fun tree_id ->
    S.Private.Node.find node_t tree_id >|= fun tree ->
    S.Private.Node.Val.list (get tree) |> List.map fst
  in
  let info = Irmin.Info.none in
  S.master repo >>= fun master ->
  S.remove_exn master ~info [] >>= fun () ->
  S.set_exn master ~info [ "foo.c" ] "foo.c" >>= fun () ->
  S.set_exn master ~info [ "foo1" ] "foo1" >>= fun () ->
  S.set_exn master ~info [ "foo"; "foo.o" ] "foo.o" >>= fun () ->
  ls master >>= fun items ->
  Alcotest.(check (list string)) "Sort order" [ "foo.c"; "foo"; "foo1" ] items;
  head_tree_id master >>= fun tree_id ->
  Alcotest.(check string)
    "Sort hash" "00c5f5e40e37fde61911f71373813c0b6cad1477"
    (Irmin.Type.to_string S.Private.Node.Key.t tree_id);

  (* Convert dir to file; changes order in listing *)
  S.set_exn master ~info [ "foo" ] "foo" >>= fun () ->
  ls master >>= fun items ->
  Alcotest.(check (list string)) "Sort order" [ "foo"; "foo.c"; "foo1" ] items;
  Lwt.return_unit

module Ref (S : Irmin_git.G) =
  Irmin_git.Ref (S) (Git_unix.Sync (S)) (Irmin.Contents.String)

let pp_reference ppf = function
  | `Branch s -> Fmt.pf ppf "branch: %s" s
  | `Remote s -> Fmt.pf ppf "remote: %s" s
  | `Tag s -> Fmt.pf ppf "tag: %s" s
  | `Other s -> Fmt.pf ppf "other: %s" s

let reference = Alcotest.testable pp_reference ( = )

let test_list_refs (module S : G) =
  let module R = Ref (S.Git) in
  S.init () >>= fun () ->
  R.Repo.v (Irmin_git.config test_db) >>= fun repo ->
  R.master repo >>= fun master ->
  R.set_exn master ~info:Irmin.Info.none [ "test" ] "toto" >>= fun () ->
  R.Head.get master >>= fun head ->
  R.Branch.set repo (`Remote "datakit/master") head >>= fun () ->
  R.Branch.set repo (`Other "foo/bar/toto") head >>= fun () ->
  R.Branch.set repo (`Branch "foo") head >>= fun () ->
  R.Repo.branches repo >>= fun bs ->
  Alcotest.(check (slist reference compare))
    "raw branches"
    [
      `Branch "foo";
      `Branch "master";
      `Other "foo/bar/toto";
      `Remote "datakit/master";
    ]
    bs;
  S.Repo.v (Irmin_git.config test_db) >>= fun repo ->
  S.Repo.branches repo >>= fun bs ->
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
    "blob foo" "blob 19\000{\"y\":[\"foo\",\"bar\"]}" str;
  let str = pre_hash X.Contents.t (X (1, 2)) in
  Alcotest.(check bin_string) "blob ''" "blob 11\000{\"x\":[1,2]}" str;
  let t = X.Tree.empty in
  X.Tree.add t [ "foo" ] (X (1, 2)) >>= fun t ->
  let k1 = X.Tree.hash t in
  X.Repo.v (Irmin_git.config test_db) >>= fun repo ->
  X.Private.Repo.batch repo (fun x y _ -> X.save_tree ~clear:false repo x y t)
  >>= fun k2 ->
  let hash = Irmin_test.testable X.Hash.t in
  Alcotest.(check hash) "blob" k1 k2;
  Lwt.return_unit

let test_import_export (module S : S) =
  let module Generic = Generic (Irmin.Contents.String) in
  let module Sync = Irmin.Sync (Generic) in
  S.init () >>= fun () ->
  Generic.init () >>= fun _ ->
  S.Repo.v (Irmin_git.config test_db) >>= fun repo ->
  S.master repo >>= fun t ->
  S.set_exn t ~info:Irmin.Info.none [ "test" ] "toto" >>= fun () ->
  let remote = Irmin.remote_store (module S) t in
  Generic.Repo.v (Irmin_mem.config ()) >>= fun repo ->
  Generic.master repo >>= fun t ->
  Sync.pull_exn t remote `Set >>= fun _ ->
  Generic.get t [ "test" ] >|= fun toto ->
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
