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

open! Import
open Common

module Json = struct
  include Yojson.Safe
  include Yojson.Safe.Util
end

type concrete = Store.Tree.concrete

module Alcotest = struct
  include Alcotest

  let yojson = testable Json.pp Json.equal
end

(** Helpers for constructing data *)

(** Tree with a single child *)
let stree only_key only_child = `Tree [ (only_key, only_child) ]

(** Sequence of nested trees each with exactly one child *)
let strees : string list -> concrete -> concrete = List.fold_right stree

let contents v = `Contents (v, ())

let set_tree store tree =
  Store.Tree.of_concrete tree
  |> Store.set_tree_exn ~info:Store.Info.none store []

type test_case = Store.t -> unit

let test_get_contents_list : test_case =
 fun store ->
  let data = strees [ "a"; "b"; "c" ] (contents "data")
  and query =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "get_contents"
         ~params:[ ("path", string "a/b/c") ]
         (list [ field "path"; field "__typename" ])
  in
  set_tree store data;
  let (result : (string * Json.t) list) = exec query Json.to_assoc in
  Alcotest.(check (list (pair string yojson)))
    "Returned entry data is valid"
    [ ("path", `String "/a/b/c"); ("__typename", `String "Contents") ]
    result;
  ()

let test_list_contents_recursively : test_case =
 fun store ->
  let () = Store.set_exn store [ "a"; "b"; "c" ] "data" ~info:Store.Info.none in
  let () =
    Store.set_exn store [ "a"; "b"; "d" ] "data1" ~info:Store.Info.none
  in
  let q =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "list_contents_recursively" (list [ field "path"; field "value" ])
  in
  let contents = exec q Json.to_list |> List.map Json.to_assoc in
  Alcotest.(check (list (list (pair string yojson))))
    "Contents list matches"
    [
      [ ("path", `String "/a/b/c"); ("value", `String "data") ];
      [ ("path", `String "/a/b/d"); ("value", `String "data1") ];
    ]
    contents

let test_get_tree_list : test_case =
 fun store ->
  let data =
    strees [ "a"; "b"; "c" ]
      (`Tree
        [ ("leaf", contents "data1"); ("branch", stree "f" (contents "data2")) ])
  and query =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "get_tree" ~params:[ ("path", string "a/b/c") ]
    @@ func "list" (list [ field "path"; field "__typename" ])
  in
  set_tree store data;
  let path_data = exec query Json.(fun x -> to_list x |> List.map to_assoc) in
  Alcotest.(check (list (list (pair string yojson))))
    "Returned entry data is valid"
    [
      [ ("path", `String "/a/b/c/branch"); ("__typename", `String "Tree") ];
      [ ("path", `String "/a/b/c/leaf"); ("__typename", `String "Contents") ];
    ]
    path_data;
  ()

let test_get_last_modified : test_case =
 fun store ->
  let data = stree "a" (contents "data")
  and query =
    query
    @@ func "main"
    @@ func "last_modified"
         ~params:[ ("path", string "a"); ("n", int 1); ("depth", int 1) ]
    @@ func "tree"
    @@ func "get_contents"
         ~params:[ ("path", string "a") ]
         (list [ field "value"; field "__typename" ])
  in
  set_tree store data;
  let result = exec query Json.(fun m -> to_list m |> List.map to_assoc) in
  Alcotest.(check (list (list (pair string yojson))))
    "Returned entry data is valid "
    [ [ ("value", `String "data"); ("__typename", `String "Contents") ] ]
    result;
  ()

let test_commit : test_case =
 fun _ ->
  let query0 =
    query @@ func "main" @@ func "head" (list [ field "hash"; field "key" ])
  in
  let result = exec query0 Json.to_assoc in
  let hash = List.assoc "hash" result |> Json.to_string in
  let key = List.assoc "key" result |> Json.to_string in
  let query1 =
    query @@ func "commit_of_key" ~params:[ ("key", var "key") ] @@ field "hash"
  in
  let vars = [ ("key", `String key) ] in
  let hash' = exec ~vars query1 Json.to_string in
  Alcotest.(check string) "Hashes equal" hash hash'

let test_mutation : test_case =
 fun store ->
  let m =
    mutation
    @@ func "set" ~params:[ ("path", string "foo"); ("value", string "bar") ]
    @@ field "hash"
  in
  let _hash = exec m Json.to_string in
  let q =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "get_contents" ~params:[ ("path", string "foo") ]
    @@ field "value"
  in
  let value = Store.get store [ "foo" ] in
  let result' = exec q Json.to_string in
  Alcotest.(check string) "Contents equal" "bar" result';
  Alcotest.(check string) "Contents equal stored value" "bar" value

let test_mutation_test_set_and_get : test_case =
 fun store ->
  (* {parents: [], allow_empty: false, retries: 1, message: "Hello", author: "Me"} *)
  let message = "A commit message" in
  let vars =
    [
      ( "info",
        `Assoc
          [
            ("parents", `List []);
            ("allow_empty", `Bool false);
            ("retries", `Int 1);
            ("message", `String message);
            ("author", `String "irmin-test-graphql");
          ] );
    ]
  in
  let m =
    mutation
    @@ func "test_set_and_get"
         ~params:
           [
             ("info", var "info");
             ("path", string "foo");
             ("test", string "bar");
             ("set", string "baz");
           ]
    @@ func "info"
    @@ field "message"
  in
  let exec_message = exec ~vars m Json.to_string in
  let q =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "get_contents" ~params:[ ("path", string "foo") ]
    @@ field "value"
  in
  let value = Store.get store [ "foo" ] in
  let result' = exec q Json.to_string in
  Alcotest.(check string) "Contents equal" "baz" result';
  Alcotest.(check string) "Contents equal stored value" "baz" value;
  Alcotest.(check string) "Same commit message" message exec_message

let test_contents_hash : test_case =
 fun _store ->
  let v = "bar" in
  let m =
    query
    @@ func "contents_hash" ~params:[ ("value", string v) ]
    @@ field "hash"
  in
  let hash = exec m Json.to_string in
  let actual_hash =
    Store.Contents.hash v |> Irmin.Type.to_string Store.Hash.t
  in
  Alcotest.(check string) "Content hash equal" actual_hash hash

let test_update_tree : test_case =
 fun store ->
  let commit = Store.Head.get store in
  let hash = Store.Commit.hash commit |> Irmin.Type.to_string Store.hash_t in
  let m =
    mutation
    @@ func "update_tree" ~params:[ ("path", string "/"); ("tree", raw "[]") ]
    @@ field "hash"
  in
  let hash' = exec m Json.to_string in
  Alcotest.(check string) "Hashes equal" hash hash';
  let m =
    mutation
    @@ func "update_tree"
         ~params:
           [
             ("path", string "/");
             ("tree", raw {| [{path: "foo", value: "bar1"}] |});
           ]
    @@ field "hash"
  in
  let hash' = exec m Json.to_string in
  if String.equal hash hash' then
    Alcotest.fail "Hashes should not be equal after update";
  let contents = Store.get store [ "foo" ] in
  Alcotest.(check string) "Contents at foo" contents "bar1";
  let m =
    mutation
    @@ func "update_tree"
         ~params:[ ("path", string "/"); ("tree", raw {| [{path: "foo"}] |}) ]
    @@ field "hash"
  in
  let () = exec m ignore in
  let contents = Store.find store [ "foo" ] in
  Alcotest.(check (option string)) "Contents empty after update" contents None

let test_remove : test_case =
 fun store ->
  let info () = Store.Info.v 0L in
  let path_param = string "test/remove" in
  let () = Store.set_exn store [ "test"; "remove" ] "XXX" ~info in
  let m =
    mutation @@ func "remove" ~params:[ ("path", path_param) ] @@ field "hash"
  in
  let () = exec m ignore in
  let q =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "get_contents" ~params:[ ("path", path_param) ]
    @@ field "value"
  in
  let c = exec q Json.to_string_option in
  Alcotest.(check (option string)) "Contents have been removed" c None

let test_branch_list : test_case =
 fun store ->
  let repo = Store.repo store in
  let head = Store.Head.get store in
  let () = Store.Branch.set repo "A" head in
  let () = Store.Branch.set repo "B" head in
  let q = query @@ func "branches" @@ list [ field "name" ] in
  let branches =
    exec q (fun x ->
        Json.to_list x |> List.map Json.to_assoc |> List.map List.hd)
  in
  let branches =
    List.sort
      (fun (_, a) (_, b) ->
        String.compare (Json.to_string a) (Json.to_string b))
      branches
  in
  Alcotest.(check (list (pair string yojson)))
    "Check branch list" branches
    [ ("name", `String "A"); ("name", `String "B"); ("name", `String "main") ]

let test_revert store =
  let head = Store.Head.get store in
  let parents = Store.Commit.parents head in
  let parent = List.hd parents in
  let parent_s = Irmin.Type.to_string Store.hash_t parent in
  let q =
    mutation
    @@ func "revert" ~params:[ ("commit", string parent_s) ]
    @@ field "hash"
  in
  let hash = exec q Json.to_string in
  Alcotest.(check string) "hash is parent hash" hash parent_s;
  let new_head = Store.Head.get store in
  let new_hash =
    Store.Commit.hash new_head |> Irmin.Type.to_string Store.hash_t
  in
  Alcotest.(check string) "parent is new head" parent_s new_hash

let suite store =
  let test_case : string -> test_case -> unit Alcotest.test_case =
   fun name f -> Alcotest.test_case name `Quick (fun () -> f store)
  in
  [
    ( "GRAPHQL",
      [
        test_case "get_contents-list" test_get_contents_list;
        test_case "list_contents_recursively" test_list_contents_recursively;
        test_case "get_tree-list" test_get_tree_list;
        test_case "get_last_modified" test_get_last_modified;
        test_case "commit" test_commit;
        test_case "contents_hash" test_contents_hash;
        test_case "mutation" test_mutation;
        test_case "mutation_test-set-and-get" test_mutation_test_set_and_get;
        test_case "update_tree" test_update_tree;
        test_case "remove" test_remove;
        test_case "branches" test_branch_list;
        test_case "revert" test_revert;
      ] );
  ]

let () =
  Random.self_init ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  Eio_main.run @@ fun env ->
  Lwt_eio.with_event_loop ~clock:env#clock @@ fun _ ->
  Lwt_eio.run_lwt @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let { event_loop; store } = spawn_graphql_server ~sw in
  Lwt.pick
    [
      event_loop;
      (Lwt_eio.run_eio @@ fun () -> Alcotest.run "irmin-graphql" (suite store));
    ]
