open Common
open Lwt.Infix

let test_dir = Filename.concat "_build" "test-concurrent"

let dict_size = 100

let tbl t =
  let tbl = Hashtbl.create 0 in
  let rec loop i =
    if i = dict_size then (
      Dict.sync t;
      tbl )
    else
      let str = random_string () in
      ( match Dict.index t str with
      | Some a -> Hashtbl.replace tbl a str
      | None -> Alcotest.failf "dictionary out of bounds" );
      loop (i + 1)
  in
  loop 0

let test_find_present t tbl =
  let check_index k i =
    Alcotest.(check (option int)) k (Some i) (Dict.index t k)
  in
  Hashtbl.iter (fun k v -> check_index v k) tbl

let test_dict () =
  let w = Dict.v ~fresh:true test_dir in
  let tbl = tbl w in
  let pid = Unix.fork () in
  if pid <> 0 then (
    test_find_present w tbl;
    exit 0 )
  else
    let r = Dict.v ~fresh:false ~readonly:true test_dir in
    test_find_present r tbl

let index_size = 100

let log_size = 500_000

let parent_pid = Unix.getpid ()

let ls t =
  let rec loop acc i =
    if i = 0 then (
      Pack.sync t;
      acc )
    else
      let x = random_string () in
      let h = sha1 x in
      Pack.unsafe_append t h x;
      loop ((h, x) :: acc) (i - 1)
  in
  loop [] index_size

let test_find_present t ls =
  Lwt_list.iter_p
    (fun (h, x) ->
      Pack.find t h >|= get >|= fun y -> Alcotest.(check string) "iter" x y)
    ls

let test_pack _switch () =
  let index = Index.v ~fresh:true ~log_size test_dir in
  Pack.v ~fresh:true ~index test_dir >>= fun w ->
  let ls = ls w in
  Lwt_io.flush_all () >>= fun () ->
  match Lwt_unix.fork () with
  | 0 -> test_find_present w ls >|= fun () -> exit 0
  | _pid ->
      let ro_index = Index.v ~fresh:false ~readonly:true ~log_size test_dir in
      Pack.v ~fresh:false ~readonly:true ~index:ro_index test_dir >>= fun r ->
      test_find_present r ls

let () =
  if Unix.getpid () = parent_pid then
    let tests =
      ( "tests",
        [
          Alcotest_lwt.test_case "pack" `Quick test_pack;
          Alcotest.test_case "dict" `Quick test_dict;
        ] )
    in
    Alcotest.run "concurrent tests" [ tests ]
