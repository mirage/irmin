open! Import
open Common

type concrete = Store.Tree.concrete

let members keys json =
  List.fold_left (fun key json -> Yojson.Safe.Util.member json key) json keys

let assert_ok : type a. (a, [ `Msg of string ]) result -> a = function
  | Ok x -> x
  | Error (`Msg msg) -> Alcotest.fail msg

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

type test_case = set_tree:(concrete -> unit Lwt.t) -> unit -> unit Lwt.t
(** Test cases consume a setter for updating the server state *)

let test_get_contents_list : test_case =
 fun ~set_tree () ->
  let data = strees [ "a"; "b"; "c" ] (contents "data")
  and query =
    {|{
  master {
    tree {
      get_contents(key: "/a/b/c") {
        key
        __typename
      }
    }
  }
}|}
  in
  set_tree data >>= fun () ->
  let+ result = send_query query >|= assert_ok >|= Yojson.Safe.from_string in
  let result : (string * Yojson.Safe.t) list =
    let open Yojson.Safe.Util in
    result |> members [ "data"; "master"; "tree"; "get_contents" ] |> to_assoc
  in
  Alcotest.(check (list (pair string yojson)))
    "Returned entry data is valid"
    [ ("key", `String "/a/b/c"); ("__typename", `String "Contents") ]
    result;
  ()

let test_get_tree_list : test_case =
 fun ~set_tree () ->
  let data =
    strees [ "a"; "b"; "c" ]
      (`Tree
        [ ("leaf", contents "data1"); ("branch", stree "f" (contents "data2")) ])
  and query =
    {|{
  master {
    tree {
      get_tree(key: "/a/b/c") {
        list {
          key
          __typename
        }
      }
    }
  }
}|}
  in
  set_tree data >>= fun () ->
  let+ result = send_query query >|= assert_ok >|= Yojson.Safe.from_string in
  let key_data : (string * Yojson.Safe.t) list list =
    let open Yojson.Safe.Util in
    result
    |> members [ "data"; "master"; "tree"; "get_tree"; "list" ]
    |> to_list
    |> List.map to_assoc
  in
  Alcotest.(check (list (list (pair string yojson))))
    "Returned entry data is valid"
    [
      [ ("key", `String "/a/b/c/branch"); ("__typename", `String "Tree") ];
      [ ("key", `String "/a/b/c/leaf"); ("__typename", `String "Contents") ];
    ]
    key_data;
  ()

let test_get_last_modified : test_case =
 fun ~set_tree () ->
  let data = stree "a" (contents "data")
  and query =
    {|{
        master {
          last_modified(key: "a", n: 1, depth:1) {
            tree {
              get_contents(key: "a") {
                value,
                __typename
              }
            }
          }
        }
    }|}
  in
  set_tree data >>= fun () ->
  let+ result = send_query query >|= assert_ok >|= Yojson.Safe.from_string in
  let result : (string * Yojson.Safe.t) list list =
    let open Yojson.Safe.Util in
    result
    |> members [ "data"; "master"; "last_modified" ]
    |> to_list
    |> List.map (members [ "tree"; "get_contents" ])
    |> List.map to_assoc
  in
  Alcotest.(check (list (list (pair string yojson))))
    "Returned entry data is valid "
    [ [ ("value", `String "data"); ("__typename", `String "Contents") ] ]
    result;
  ()

let suite ~set_tree =
  let test_case : string -> test_case -> unit Alcotest_lwt.test_case =
   fun name f -> Alcotest_lwt.test_case name `Quick (fun _ () -> f ~set_tree ())
  in
  [
    ( "GRAPHQL",
      [
        test_case "get_contents-list" test_get_contents_list;
        test_case "get_tree-list" test_get_tree_list;
        test_case "get_last_modified" test_get_last_modified;
      ] );
  ]

let () =
  Random.self_init ();
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let main =
    let* { event_loop; set_tree } = spawn_graphql_server () in
    Lwt.pick [ event_loop; Alcotest_lwt.run "irmin-graphql" (suite ~set_tree) ]
  in
  Lwt_main.run main
