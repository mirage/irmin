module T = Irmin.Type

let size = function
  | `Size s -> s
  | _ -> Alcotest.fail "size"

let test_base () =

  let s = T.to_json_string T.string "foo" in
  Alcotest.(check string) "JSON string" "\"foo\"" s;

  let s = T.to_bin_string T.string "foo" in
  Alcotest.(check string) "binary string" "foo" s;
  Alcotest.(check int) "binary size"
    (String.length "foo") (size (T.size_of T.(string_of (`Fixed 3)) "foo"));

  let s = T.to_string T.string "foo" in
  Alcotest.(check string) "CLI string" "foo" s;

  let s = T.to_json_string T.int 42 in
  Alcotest.(check string) "JSON int" "42" s;

  let s = T.to_bin_string T.int 42 in
  Alcotest.(check string) "binary int" "*" s;

  let s = T.to_string T.int 42 in
  Alcotest.(check string) "CLI string" "42" s

let id x = x
let pp_hex ppf s = let `Hex x = Hex.of_string s in Fmt.string ppf x
let of_hex_string x = Ok (Hex.to_string (`Hex x))

let hex = T.like_map T.string ~cli:(pp_hex, of_hex_string) id id

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
  T.like_map T.string ~json:(encode_json, decode_json) id id

let error = Alcotest.testable (fun ppf (`Msg e) -> Fmt.string ppf e) (=)
let ok x = Alcotest.result x error

let test_json () =

  let s = T.to_string hex "foo" in
  Alcotest.(check string) "CLI hex" "666f6f" s;

  let s = T.to_json_string hex "foo" in
  Alcotest.(check string) "JSON hex" "\"666f6f\"" s;

  let s = T.to_bin_string hex "foo" in
  Alcotest.(check string) "CLI hex" "foo" s;

  let x = T.of_json_string hex "\"666f6f\"" in
  Alcotest.(check (ok string)) "JSON of hex" (Ok "foo") x;

  let x = T.of_json_string hex2 "[\"foo\", \"666f6f\"]" in
  Alcotest.(check (ok string)) "JSON to hex2" (Ok "foo") x;

  let x = T.to_json_string hex2 "foo" in
  Alcotest.(check string) "JSON of hex2" "[\"foo\",\"666f6f\"]" x

let l =
  let hex = T.like_map (T.string_of (`Fixed 3)) ~cli:(pp_hex, of_hex_string) id id in
  T.list ~len:(`Fixed 2) hex

let tl = Alcotest.testable (T.pp l) (T.equal l)

let test_bin () =

  let s = T.to_string l ["foo"; "foo"] in
  Alcotest.(check string) "hex list" "[\"666f6f\",\"666f6f\"]" s;

  let s = T.to_bin_string l ["foo"; "bar"] in
  Alcotest.(check string) "encode list" "foobar" s;
  Alcotest.(check int) "size of list" 6 (size (T.size_of l ["foo"; "bar"]));

  let s = T.of_bin_string l "foobar" in
  Alcotest.(check (ok tl)) "decode list" (Ok ["foo"; "bar"]) s

let x = T.like ~compare:(fun x y -> y - x - 1) T.int

let test_compare () =
  Alcotest.(check int) "rev compare" (T.compare x 1 2) 0;
  Alcotest.(check int) "rev compare" (T.compare x 2 1) (-2);
  Alcotest.(check int) "rev compare" (T.compare x 1 1) (-1);
  Alcotest.(check bool) "rev equal" (T.equal x 1 2) true;
  Alcotest.(check bool) "rev equal" (T.equal x 1 1) false

let x = T.like ~equal:(fun x y -> x - y = 2) T.int

let test_equal () =
  Alcotest.(check int) "eq" (T.compare x 1 2) (compare 1 2);
  Alcotest.(check int) "eq" (T.compare x 3 1) (compare 3 1);
  Alcotest.(check bool) "eq" (T.equal x 3 1) true;
  Alcotest.(check bool) "eq" (T.equal x 0 0) false;
  let a = `O  ["b", `Float 2.; "c", `A [`String "test"]; "a", `Bool true] in
  let b = `O ["a", `Bool true; "b", `Float 2.; "c", `A [`String "test"]] in
  Alcotest.(check bool) "json eq" (T.equal Irmin.Contents.Json_value.t a b) true

let test_int () =
  let test dx x =
    let tt = Alcotest.testable (T.pp dx) (T.equal dx) in
    match T.of_bin_string dx (T.to_bin_string dx x) with
    | Error (`Msg e) -> Alcotest.fail e
    | Ok y -> Alcotest.(check tt) "eq" x y
  in
  let size x s =
    match T.size_of T.int x with
    | `Size ss -> Alcotest.(check int) (Fmt.strf "size:%d" x) s ss
    | _ -> Alcotest.fail "size"
  in
  let p7  = 128 in
  let p14 = 16384 in
  let p21 = 2097152 in
  let p28 = 268435456 in
  let p35 = 34359738368 in
  let p42 = 4398046511104 in
  let p49 = 562949953421312 in
  let p56 = 72057594037927936 in
  (*  let p63 = max_int in *)
  let ps = [p7; p14; p21; p28; p35; p42; p49; p56; (* p63 *) ] in
  List.iter (fun p ->
      test T.int (p - 1);
      test T.int p;
      test T.int (p + 1)
    ) (0 :: ps);
  test T.(list int) [];
  test T.string "";
  test T.string (String.make p14 'x');
  List.iter (fun p ->
      if p > 0 && p < p28 then test T.(array int) (Array.make p 42)
    ) ps;
  size 0 1;
  List.iteri (fun i p ->
      size (p - 1) (i + 1);
      size p (i + 2)
    ) ps

let test_sharing () =
  let make () = Bytes.of_string "xxxxxxxx" in

  let buf = make () in
  let n = T.encode_bin T.string buf 0 "foo" in
  Alcotest.(check string) "foo 1" (Bytes.to_string buf) "\003fooxxxx";
  Alcotest.(check int) "foo 1 len" 4 n;

  let buf = make () in
  let n = T.encode_bin T.string buf 1 "foo" in
  Alcotest.(check string) "foo 2" (Bytes.to_string buf) "x\003fooxxx";
  Alcotest.(check int) "foo 2 len" 5 n;

  let buf = make () in
  let n = T.encode_bin T.bytes buf 2 (Bytes.of_string "foo") in
  Alcotest.(check string) "foo 3" (Bytes.to_string buf) "xx\003fooxx";
  Alcotest.(check int) "foo 3 len" 6 n;

  let buf = make () in
  let n = T.encode_bin T.bytes buf 3 (Bytes.of_string "foo") in
  Alcotest.(check string) "foo 4" (Bytes.to_string buf) "xxx\003foox";
  Alcotest.(check int) "foo 4 len" 7 n;

  let buf = make () in
  let n = T.encode_bin T.int buf 3 4 in
  Alcotest.(check string) "foo 4" (Bytes.to_string buf) "xxx\004xxxx";
  Alcotest.(check int) "foo 4 len" 4 n

let test_decode () =
  let wrap f =
    try Ok (f ())
    with e -> Fmt.kstrf (fun s -> Error s) "%a" Fmt.exn e
  in
  let decode ~off buf exp =
      match exp, wrap (fun () -> T.decode_bin T.string buf off) with
        | Error (), Error _   -> ()
        | Ok x    , Ok (_, y) -> Alcotest.(check string) ("decode " ^ x) x y
        | Error _ , Ok (_, y) -> Alcotest.failf "error expected, got %s" y
        | Ok x    , Error e   -> Alcotest.failf "expected: %s, got error: %s" x e
  in
  decode ~off:2 "xx\003aaayyy" (Ok "aaa");
  decode ~off:2 "xx\003aa" (Error ());
  decode ~off:2 "xx\002aa" (Ok "aa");
  decode ~off:2 "xx\000aaaaa" (Ok "")

let test_size () =
  let check t v n = match Irmin.Type.size_of t v with
    | `Size s   ->
      let name = Fmt.strf "size: %a" (Irmin.Type.pp t) v  in
      Alcotest.(check int) name n s
    | `Buffer _ -> Alcotest.fail "size expected"
  in
  check Irmin.Type.int 0   1;
  check Irmin.Type.int 128 2;
  check Irmin.Type.int 16384 3;
  check Irmin.Type.string "foo" (1+3);
  check Irmin.Type.string (String.make 128 'x') (2+128);
  check Irmin.Type.bytes (Bytes.of_string "foo") 4;
  check Irmin.Type.(list string) [] 1

module Hash = Irmin.Hash.SHA1
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None

module Node = Irmin.Private.Node.Make(Hash)(Path)(Metadata)
module Node_v1 = Irmin.Private.Node.V1(Node)

module Commit = Irmin.Private.Commit.Make(Hash)
module Commit_v1 = Irmin.Private.Commit.V1(Commit)

module Hash_v1 = Irmin.Hash.V1(Hash)

let hash c =
  Hash.digest (Irmin.Type.to_bin_string Irmin.Contents.String.t c)

let hash_v1 c =
  Hash_v1.digest (Irmin.Type.to_bin_string Irmin.Contents.V1.String.t c)

let test_hashes () =
  let digest t x =
    let s = Irmin.Type.to_bin_string t x in
    Printf.eprintf "to_bin_string: %S\n" s;
    Irmin.Type.to_string Hash.t (Hash.digest s)
  in
  Alcotest.(check string) "empty contents"
    "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    (digest Irmin.Contents.String.t "");
  Alcotest.(check string) "empty v1 contents"
    "05fe405753166f125559e7c9ac558654f107c7e9"
    (digest Irmin.Contents.V1.String.t "");
  Alcotest.(check string) "empty bytes"
    "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    (digest Irmin.Contents.Bytes.t (Bytes.of_string ""));
  Alcotest.(check string) "contents"
    "b60d121b438a380c343d5ec3c2037564b82ffef3"
    (digest Irmin.Contents.String.t "xxx");

  Alcotest.(check string) "empty node"
    "5ba93c9db0cff93f52b521d7420e43f6eda2784f"
    (digest Node.t Node.empty);
  Alcotest.(check string) "empty v1 node"
    "05fe405753166f125559e7c9ac558654f107c7e9"
    (digest Node_v1.t Node_v1.empty);

  let n1 v hash = v [
    "foo", `Contents (hash "", Metadata.default);
    "bar", `Node (hash "bar");
  ]
  in

  Alcotest.(check string) "node"
    "38920183f8b667f6b643b1c4e524a95b55b20d31"
    (digest Node.t (n1 Node.v hash));

  Alcotest.(check string) "node v1"
    "bc5615e070d2838b278a1de0bb63fe325dd9cb11"
    (digest Node_v1.t (n1 Node_v1.v hash_v1));

  let v1 v hash = v
      ~info:(Irmin.Info.empty)
      ~node:(hash "toto")
      ~parents:[]
  in
  let v2 v hash = v
      ~info:(Irmin.Info.v ~date:42L ~author:"yay" "\bfoo\bar")
      ~node:(hash "toto")
      ~parents:[hash "xxx"; hash"yyy"]
  in
  Alcotest.(check string) "commit 1"
    "31c7871af72105ccf25e527fc00c14c9cafbd280"
    (digest Commit.t (v1 Commit.v hash));
  Alcotest.(check string) "commit v1 1"
    "d37cc867cc6eca1b6818b0bc36fef3ffed5cc6d2"
    (digest Commit_v1.t (v1 Commit_v1.v hash_v1));

  Alcotest.(check string) "commit 2"
    "2311a8c81b36dd2360a6c3a581c5699940423470"
    (digest Commit.t (v2 Commit.v hash));
  Alcotest.(check string) "commit v1 2"
    "e113f06cd69ed367dd7e4690f373a743cd4852b4"
    (digest Commit_v1.t (v2 Commit_v1.v hash_v1))

let suite = [
  "type", [
    "base"   , `Quick, test_base;
    "json"   , `Quick, test_json;
    "bin"    , `Quick, test_bin;
    "compare", `Quick, test_compare;
    "equal"  , `Quick, test_equal;
    "ints"   , `Quick, test_int;
    "sharing", `Quick, test_sharing;
    "decode" , `Quick, test_decode;
    "size_of", `Quick, test_size;
    "test_hashes", `Quick, test_hashes
  ]
]

let () = Alcotest.run "irmin" suite
