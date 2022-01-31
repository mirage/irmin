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

open! Import
open Common

type concrete = Store.Tree.concrete

module Alcotest = struct
  include Alcotest

  let yojson = testable Yojson.Safe.pp Yojson.Safe.equal
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

type test_case = Store.t -> unit Lwt.t

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
  set_tree store data >>= fun () ->
  let+ (result : (string * Yojson.Safe.t) list) =
    exec query Yojson.Safe.Util.to_assoc
  in
  Alcotest.(check (list (pair string yojson)))
    "Returned entry data is valid"
    [ ("path", `String "/a/b/c"); ("__typename", `String "Contents") ]
    result;
  ()

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
  set_tree store data >>= fun () ->
  let+ path_data =
    exec query Yojson.Safe.Util.(fun x -> to_list x |> List.map to_assoc)
  in
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
  set_tree store data >>= fun () ->
  let+ result =
    exec query Yojson.Safe.Util.(fun m -> to_list m |> List.map to_assoc)
  in
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
  let* result = exec query0 Yojson.Safe.Util.to_assoc in
  let hash = List.assoc "hash" result |> Yojson.Safe.Util.to_string in
  let key = List.assoc "key" result |> Yojson.Safe.Util.to_string in
  let query1 =
    query @@ func "commit_of_key" ~params:[ ("key", var "key") ] @@ field "hash"
  in
  let vars = [ ("key", `String key) ] in
  let+ hash' = exec ~vars query1 Yojson.Safe.Util.to_string in
  Alcotest.(check string) "Hashes equal" hash hash'

let test_mutation : test_case =
 fun store ->
  let m =
    mutation
    @@ func "set" ~params:[ ("path", string "foo"); ("value", string "bar") ]
    @@ field "hash"
  in
  let* _hash = exec m Yojson.Safe.Util.to_string in
  let q =
    query
    @@ func "main"
    @@ func "tree"
    @@ func "get_contents" ~params:[ ("path", string "foo") ]
    @@ field "value"
  in
  let* value = Store.get store [ "foo" ] in
  let+ result' = exec q Yojson.Safe.Util.to_string in
  Alcotest.(check string) "Contents equal" "bar" result';
  Alcotest.(check string) "Contents equal stored value" "bar" value

let suite store =
  let test_case : string -> test_case -> unit Alcotest_lwt.test_case =
   fun name f -> Alcotest_lwt.test_case name `Quick (fun _ () -> f store)
  in
  [
    ( "GRAPHQL",
      [
        test_case "get_contents-list" test_get_contents_list;
        test_case "get_tree-list" test_get_tree_list;
        test_case "get_last_modified" test_get_last_modified;
        test_case "commit" test_commit;
        test_case "mutation" test_mutation;
      ] );
  ]

let () =
  Random.self_init ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let main =
    let* { event_loop; store } = spawn_graphql_server () in
    Lwt.pick [ event_loop; Alcotest_lwt.run "irmin-graphql" (suite store) ]
  in
  Lwt_main.run main
