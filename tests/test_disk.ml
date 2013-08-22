open IrminLwt
open OUnit

(* values *)
let v1 = Value.blob "foo"
let v2 = Value.blob "bar"

let cmp_opt fn x y =
  match x, y with
  | Some x, Some y -> fn x y
  | _ -> false

let test_db = "test-db"

let clean () =
  if Sys.file_exists test_db then
    let cmd = Printf.sprintf "rm -rf %s" test_db in
    let _ = Sys.command cmd in
    ()

let with_db fn =
  clean ();
  lwt () = Disk.init test_db in
  let t = Disk.create test_db in
  try_lwt fn t
  with e ->
    clean ();
    raise_lwt e

let assert_key_equal =
  assert_equal ~cmp:Key.equal ~printer:Key.pretty

let assert_value_opt_equal =
  let printer = function
    | None   -> "<none>"
    | Some v -> Value.pretty v in
  assert_equal ~cmp:(cmp_opt Value.equal) ~printer

let test_values () =
  let module DV = Disk.Value_store in
  let test t =
    lwt k1 = DV.write t v1 in
    lwt k1' = DV.write t v1 in
    lwt k2 = DV.write t v2 in
    lwt k2' = DV.write t v2 in
    lwt v1' = DV.read t k1 in
    lwt v2' = DV.read t k2 in
    assert_key_equal k1 k1';
    assert_key_equal k2 k2';
    assert_value_opt_equal (Some v1) v1';
    assert_value_opt_equal (Some v2) v2';
    Lwt.return ()
  in
  Lwt_unix.run (with_db test)

let suite =
  "disk" >:::
    [" Basic disk operations for values"
     >:: test_values
    ]

let _ =
  run_test_tt_main suite
