let check typ pos ~expected actual =
  Alcotest.(check ~pos typ) "" expected actual

let check_bool = check Alcotest.bool

let check_ok_or_duplicate =
  let pp : [ `Ok | `Duplicate ] Fmt.t =
    Fmt.of_to_string (function `Ok -> "`Ok" | `Duplicate -> "`Duplicate")
  in
  check (Alcotest.testable pp ( = ))

let check_invalid_arg pos f =
  let fail got =
    Alcotest.failf ~pos
      "Expected function to raise `Invalid_argument`, but raised: %a"
      Fmt.(Dump.option exn)
      got
  in
  match f () with
  | _ -> fail None
  | exception Invalid_argument _ -> ()
  | exception exn -> fail (Some exn)
