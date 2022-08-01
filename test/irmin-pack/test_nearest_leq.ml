open Irmin_pack_unix

let nearest_leq = Utils.nearest_leq

let test_nearest_leq () =
  let arr = Array.of_list [ 1; 3; 5; 7 ] in
  let get arr i = arr.(i) in
  let lo, hi = (0, Array.length arr - 1) in
  let nearest_leq_ key = nearest_leq ~arr ~get ~lo ~hi ~key in
  assert (nearest_leq_ 0 = `All_gt_key);
  assert (nearest_leq_ 1 = `Some 0);
  assert (nearest_leq_ 2 = `Some 0);
  assert (nearest_leq_ 3 = `Some 1);
  assert (nearest_leq_ 3 = `Some 1);
  assert (nearest_leq_ 4 = `Some 1);
  assert (nearest_leq_ 5 = `Some 2);
  assert (nearest_leq_ 6 = `Some 2);
  assert (nearest_leq_ 7 = `Some 3);
  assert (nearest_leq_ 8 = `Some 3);
  assert (nearest_leq_ 100 = `Some 3);
  ()

let tests : unit Alcotest_lwt.test_case list =
  Alcotest_lwt.
    [
      test_case "test_nearest_leq.1" `Quick (fun _switch () ->
          test_nearest_leq ();
          Lwt.return ());
    ]
