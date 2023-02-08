open Irmin_pack_unix

type geq = int option [@@deriving irmin ~pp ~equal]

let geq = Alcotest.testable pp_geq equal_geq

let test_nearest_geq () =
  let arr = Array.of_list [ 1; 3; 5; 7 ] in
  let get arr i = arr.(i) in
  let lo, hi = (0, Array.length arr - 1) in
  let nearest_geq key = Utils.nearest_geq ~arr ~get ~lo ~hi ~key in
  Alcotest.(check geq) "0" (nearest_geq 0) (Some 0);
  Alcotest.(check geq) "1" (nearest_geq 1) (Some 0);
  Alcotest.(check geq) "2" (nearest_geq 2) (Some 1);
  Alcotest.(check geq) "3" (nearest_geq 3) (Some 1);
  Alcotest.(check geq) "4" (nearest_geq 4) (Some 2);
  Alcotest.(check geq) "5" (nearest_geq 5) (Some 2);
  Alcotest.(check geq) "6" (nearest_geq 6) (Some 3);
  Alcotest.(check geq) "7" (nearest_geq 7) (Some 3);
  Alcotest.(check geq) "8" (nearest_geq 8) None;
  Lwt.return_unit

let tests =
  [
    Alcotest_lwt.test_case "test_nearest_geq" `Quick (fun _switch () ->
        test_nearest_geq ());
  ]
