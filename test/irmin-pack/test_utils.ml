open Astring
open Irmin_pack.Private.Utils

let ( - ), ( * ), ( / ) = Int63.(sub, mul, div)

let read_bar () =
  Format.flush_str_formatter ()
  |> String.trim ~drop:(function '\r' | '\n' -> true | _ -> false)

let test_progress_bar_lifecycle () =
  let k i = Int63.of_int i * Int63.of_int 1024 in
  let m i = k i * Int63.of_int 1024 in
  let g i = m i * Int63.of_int 1024 in
  let _bar, report =
    Progress.counter ~total:(g 1) ~sampling_interval:1 ~columns:60
      ~message:"<msg>" ~pp_count:pp_bytes ~ppf:Format.str_formatter ()
  in
  let check_bar expected =
    read_bar ()
    |> Alcotest.(check string) ("Expected state: " ^ expected) expected
  in
  check_bar "<msg>     0.0 B    00:00  [...........................]   0%";
  report (k 1 - Int63.one);
  check_bar "<msg>  1023.0 B    00:00  [...........................]   0%";
  report Int63.one;
  check_bar "<msg>     1.0 KiB  00:00  [...........................]   0%";
  report (m 1 - k 1 - Int63.one);
  (* Should always round downwards. *)
  check_bar "<msg>  1023.9 KiB  00:00  [...........................]   0%";
  report Int63.one;
  check_bar "<msg>     1.0 MiB  00:00  [...........................]   0%";
  report (m 49);
  check_bar "<msg>    50.0 MiB  00:00  [#..........................]   4%";
  report (m 450);
  check_bar "<msg>   500.0 MiB  00:00  [############...............]  48%";
  report (g 1 - m 500 - Int63.one);
  (* 1 byte from completion. Should show 99% and not a full 1024 MiB. *)
  check_bar "<msg>  1023.9 MiB  00:00  [##########################.]  99%";
  report Int63.one;
  (* Now exactly complete *)
  check_bar "<msg>     1.0 GiB  00:00  [###########################] 100%";
  (* Subsequent reports don't overflow the bar *)
  report (g 1 / Int63.of_int 2);
  check_bar "<msg>     1.5 GiB  00:00  [###########################] 100%";
  ()

let test_progress_bar_width () =
  let check_width ~columns ~message ?pp_count () =
    let _, _ =
      Progress.counter ~total:Int63.one ~sampling_interval:1
        ~ppf:Format.str_formatter ~columns ~message ?pp_count ()
    in

    let count_width = match pp_count with Some (_, c) -> c | None -> 0 in
    String.length (read_bar ())
    |> Alcotest.(check int)
         (Fmt.str
            "Expected width for configuration { columns = %d; count_width = \
             %d; message = %s }"
            columns count_width message)
         columns
  in
  check_width ~columns:80 ~message:"<msg>" ~pp_count:pp_bytes ();
  check_width ~columns:40 ~message:"" ~pp_count:(Fmt.(const string "XX"), 2) ();
  check_width ~columns:40 ~message:"Very long message" ();

  Alcotest.check_raises "Overly small progress bar"
    (Invalid_argument "Not enough space for a progress bar") (fun () ->
      ignore
        (Progress.counter ~total:Int63.one ~sampling_interval:1
           ~ppf:Format.str_formatter ~columns:18 ~message:"" ()));
  ()

let tests =
  [
    Alcotest.test_case "Progress bar lifecycle" `Quick
      test_progress_bar_lifecycle;
    Alcotest.test_case "Progress bar width" `Quick test_progress_bar_width;
  ]
