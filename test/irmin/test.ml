module T = Irmin.Type

let size = function
  | `Size s -> s
  | _ -> Alcotest.fail "size"

let test_base () =

  let s = T.to_json_string T.string "foo" in
  Alcotest.(check string) "JSON string" "\"foo\"" s;

  let s = T.encode_bin T.string "foo" in
  Alcotest.(check string) "binary string" "foo" s;
  Alcotest.(check int) "binary size"
    (String.length "foo") (size (T.size_of T.string "foo"));

  let s = T.to_string T.string "foo" in
  Alcotest.(check string) "CLI string" "foo" s;

  let s = T.to_json_string T.int 42 in
  Alcotest.(check string) "JSON int" "42" s;

  let s = T.encode_bin T.int 42 in
  Alcotest.(check string) "binery int" "\000\000\000\000\000\000\000*" s;

  let s = T.to_string T.int 42 in
  Alcotest.(check string) "CLI string" "42" s

let id x = x
let pp_hex ppf s = let `Hex x = Hex.of_string s in Fmt.string ppf x
let of_hex_string x = Ok (Hex.to_string (`Hex x))

let hex = T.like T.string ~cli:(pp_hex, of_hex_string) id id

let hex2 =
  let encode_json e x =
    let encode x = ignore (Jsonm.encode e (`Lexeme x)) in
    encode `As;
    encode (`String x);
    encode (`String (Fmt.to_to_string pp_hex x));
    encode `Ae;
  in
  let decode_json e =
    let decode () = match T.Json.decode e with
      | `Lexeme e -> e
      | `Error e  -> Alcotest.failf "%a" Jsonm.pp_error e
      | `End | `Await -> assert false
    in
    assert (decode () = `As);
    let x = decode () in
    let y = decode () in
    assert (decode () = `Ae);
    match x, y with
    | `String x, `String y -> assert (of_hex_string y = Ok x); Ok x
    | _ -> Alcotest.failf "invalid strings: %a %a"
             Jsonm.pp_lexeme x Jsonm.pp_lexeme y
  in
  T.like T.string ~json:(encode_json, decode_json) id id

let error = Alcotest.testable (fun ppf (`Msg e) -> Fmt.string ppf e) (=)
let ok x = Alcotest.result x error

let test_json () =

  let s = T.to_string hex "foo" in
  Alcotest.(check string) "CLI hex" "666f6f" s;

  let s = T.to_json_string hex "foo" in
  Alcotest.(check string) "JSON hex" "\"666f6f\"" s;

  let s = T.encode_bin hex "foo" in
  Alcotest.(check string) "CLI hex" "foo" s;

  let x = T.of_json_string hex "\"666f6f\"" in
  Alcotest.(check (ok string)) "JSON of hex" (Ok "foo") x;

  let x = T.of_json_string hex2 "[\"foo\", \"666f6f\"]" in
  Alcotest.(check (ok string)) "JSON to hex2" (Ok "foo") x;

  let x = T.to_json_string hex2 "foo" in
  Alcotest.(check string) "JSON of hex2" "[\"foo\",\"666f6f\"]" x

let l =
  let hex = T.like (T.string_of (`Fixed 3)) ~cli:(pp_hex, of_hex_string) id id in
  T.list ~len:(`Fixed 2) hex

let tl = Alcotest.testable (T.pp l) (T.equal l)

let test_bin () =

  let s = T.to_string l ["foo"; "foo"] in
  Alcotest.(check string) "hex list" "[\"666f6f\",\"666f6f\"]" s;

  let s = T.encode_bin l ["foo"; "bar"] in
  Alcotest.(check string) "encode list" "foobar" s;
  Alcotest.(check int) "size of list" 6 (size (T.size_of l ["foo"; "bar"]));

  let s = T.decode_bin l "foobar" in
  Alcotest.(check (ok tl)) "decode list" (Ok ["foo"; "bar"]) s

let x = T.like' ~compare:(fun x y -> y - x - 1) T.int

let test_compare () =
  Alcotest.(check int) "rev compare" (T.compare x 1 2) 0;
  Alcotest.(check int) "rev compare" (T.compare x 2 1) (-2);
  Alcotest.(check int) "rev compare" (T.compare x 1 1) (-1);
  Alcotest.(check bool) "rev equal" (T.equal x 1 2) true;
  Alcotest.(check bool) "rev equal" (T.equal x 1 1) false

let x = T.like' ~equal:(fun x y -> x - y = 2) T.int

let test_equal () =
  Alcotest.(check int) "eq" (T.compare x 1 2) (compare 1 2);
  Alcotest.(check int) "eq" (T.compare x 3 1) (compare 3 1);
  Alcotest.(check bool) "eq" (T.equal x 3 1) true;
  Alcotest.(check bool) "eq" (T.equal x 0 0) false

let suite = [
  "type", [
    "base"   , `Quick, test_base;
    "json"   , `Quick, test_json;
    "bin"    , `Quick, test_bin;
    "compare", `Quick, test_compare;
    "equal"  , `Quick, test_equal;
  ]
]

let () = Alcotest.run "irmin" suite
