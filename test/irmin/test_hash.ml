open Irmin

let test_short_hash () =
  let h = Hash.BLAKE2B.hash (fun f -> f "") in
  let () =
    Hash.BLAKE2B.short_hash h
    |> Alcotest.(check int) "Specialised short hash" 2020213495
  in
  let () =
    Type.(unstage (short_hash Hash.BLAKE2B.t)) ~seed:0 h
    |> Alcotest.(check int) "Generic seeded short hash" 674923654
  in
  let () =
    Type.(unstage (short_hash Hash.BLAKE2B.t)) ?seed:None h
    |> Alcotest.(check int) "Generic unseeded short hash" 2020213495
  in
  ()

let suite = [ Alcotest_lwt.test_case_sync "short_hash" `Quick test_short_hash ]
