open Irmin_pack_unix

type leq = [ `All_gt_key | `Some of int ] [@@deriving irmin ~pp ~equal]

let leq = Alcotest.testable pp_leq equal_leq

let test_nearest_leq () =
  let arr = Array.of_list [ 1; 3; 5; 7 ] in
  let get arr i = arr.(i) in
  let lo, hi = (0, Array.length arr - 1) in
  let nearest_leq key = Utils.nearest_leq ~arr ~get ~lo ~hi ~key in
  Alcotest.(check leq) "0" (nearest_leq 0) `All_gt_key;
  Alcotest.(check leq) "1" (nearest_leq 1) (`Some 0);
  Alcotest.(check leq) "2" (nearest_leq 2) (`Some 0);
  Alcotest.(check leq) "3" (nearest_leq 3) (`Some 1);
  Alcotest.(check leq) "4" (nearest_leq 4) (`Some 1);
  Alcotest.(check leq) "5" (nearest_leq 5) (`Some 2);
  Alcotest.(check leq) "6" (nearest_leq 6) (`Some 2);
  Alcotest.(check leq) "7" (nearest_leq 7) (`Some 3);
  Alcotest.(check leq) "8" (nearest_leq 8) (`Some 3);
  Alcotest.(check leq) "100" (nearest_leq 100) (`Some 3)

let tests =
  [
    Alcotest.test_case "test_nearest_leq" `Quick (fun () -> test_nearest_leq ());
  ]
