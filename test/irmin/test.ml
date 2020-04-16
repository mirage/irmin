module T = Irmin.Type

type foo = { a : int; b : int }

type bar = { c : int option; d : int option option }

let test_base () =
  let s = T.to_json_string T.string "foo" in
  Alcotest.(check string) "JSON string" "\"foo\"" s;
  let s = T.to_bin_string T.string "foo" in
  Alcotest.(check string) "binary string" "foo" s;
  Alcotest.(check (option int))
    "binary size"
    (Some (String.length "foo"))
    (T.size_of T.(string_of (`Fixed 3)) "foo");
  let s = T.to_string T.string "foo" in
  Alcotest.(check string) "CLI string" "foo" s;
  let s = T.to_json_string T.int 42 in
  Alcotest.(check string) "JSON int" "42" s;
  let s = T.to_bin_string T.int 42 in
  Alcotest.(check string) "binary int" "*" s;
  let s = T.to_string T.int 42 in
  Alcotest.(check string) "CLI string" "42" s;
  let s = T.to_json_string T.unit () in
  Alcotest.(check string) "JSON unit" "{}" s

let id x = x

let pp_hex ppf s =
  let (`Hex x) = Hex.of_string s in
  Fmt.string ppf x

let of_hex_string x = Ok (Hex.to_string (`Hex x))

let hex = T.map T.string ~cli:(pp_hex, of_hex_string) id id

let hex2 =
  let encode_json e x =
    let encode x = ignore (Jsonm.encode e (`Lexeme x)) in
    encode `As;
    encode (`String x);
    encode (`String (Fmt.to_to_string pp_hex x));
    encode `Ae
  in
  let decode_json e =
    let decode () =
      match T.Json.decode e with
      | `Lexeme e -> e
      | `Error e -> Alcotest.failf "%a" Jsonm.pp_error e
      | `End | `Await -> assert false
    in
    assert (decode () = `As);
    let x = decode () in
    let y = decode () in
    assert (decode () = `Ae);
    match (x, y) with
    | `String x, `String y ->
        assert (of_hex_string y = Ok x);
        Ok x
    | _ ->
        Alcotest.failf "invalid strings: %a %a" Jsonm.pp_lexeme x
          Jsonm.pp_lexeme y
  in
  T.map T.string ~json:(encode_json, decode_json) id id

let error = Alcotest.testable (fun ppf (`Msg e) -> Fmt.string ppf e) ( = )

let ok x = Alcotest.result x error

let test_json () =
  let s = T.to_json_string hex "foo" in
  Alcotest.(check string) "JSON hex" "\"666f6f\"" s;
  let s = T.to_bin_string hex "foo" in
  Alcotest.(check string) "CLI hex" "foo" s;
  let x = T.of_json_string hex "\"666f6f\"" in
  Alcotest.(check (ok string)) "JSON of hex" (Ok "foo") x;
  let x = T.of_json_string hex2 "[\"foo\", \"666f6f\"]" in
  Alcotest.(check (ok string)) "JSON to hex2" (Ok "foo") x;
  let x = T.to_json_string hex2 "foo" in
  Alcotest.(check string) "JSON of hex2" "[\"foo\",\"666f6f\"]" x;
  let x = T.to_json_string T.char (char_of_int 128) in
  Alcotest.(check string) "JSON char larger than 127" "{\"base64\":\"gA==\"}" x;
  let x = T.to_json_string T.string "\128\129a" in
  Alcotest.(check (ok string))
    "JSON string with chars larger than 127"
    (T.of_json_string T.string x)
    (Ok "\128\129a");
  let x = T.of_json_string T.unit "{}" in
  Alcotest.(check (ok unit)) "JSON of unit" (Ok ()) x

let test_json_option () =
  let open T in
  (* Test JSON encoding. *)
  let x = to_json_string (option int) (Some 1) in
  Alcotest.(check string) "Option outside of record 1" "{\"some\":1}" x;
  let x = to_json_string (option int) None in
  Alcotest.(check string) "Option outside of record 2" "null" x;
  let x = to_json_string (option (option int)) (Some (Some 1)) in
  Alcotest.(check string)
    "Nested option outside of record 1" "{\"some\":{\"some\":1}}" x;
  let x = to_json_string (option (option int)) (Some None) in
  Alcotest.(check string)
    "Nested option outside of record 2" "{\"some\":null}" x;
  let x = to_json_string (option (option int)) None in
  Alcotest.(check string) "Nested option outside of record 3" "null" x;
  let t =
    record "foo" (fun c d -> { c; d })
    |+ field "c" (option int) (fun r -> r.c)
    |+ field "d" (option (option int)) (fun r -> r.d)
    |> sealr
  in
  let x = to_json_string t { c = None; d = None } in
  Alcotest.(check string) "Nested option within record 1" "{}" x;
  let x = to_json_string t { c = Some 1; d = None } in
  Alcotest.(check string) "Nested option within record 2" "{\"c\":1}" x;
  let x = to_json_string t { c = None; d = Some (Some 1) } in
  Alcotest.(check string)
    "Nested option within record 2" "{\"d\":{\"some\":1}}" x;
  let x = to_json_string t { c = None; d = Some None } in
  Alcotest.(check string) "Nested option within record 3" "{\"d\":null}" x;

  (* Test JSON decoding. *)
  let x = of_json_string (option int) "null" in
  Alcotest.(check (ok (option int))) "Decode null option" (Ok None) x;
  let x = of_json_string (option int) "{\"some\":1}" in
  Alcotest.(check (ok (option int))) "Decode some option" (Ok (Some 1)) x;
  let x = of_json_string (option (option int)) "{\"some\":null}" in
  Alcotest.(check (ok (option (option int))))
    "Decode nested null option" (Ok (Some None)) x;

  let testable_t = Alcotest.testable (T.pp t) (T.equal t) in
  let x = of_json_string t "{}" in
  Alcotest.(check (ok testable_t))
    "Decode nested option"
    (Ok { c = None; d = None })
    x;
  let x = of_json_string t "{\"c\":1}" in
  Alcotest.(check (ok testable_t))
    "Decode nested option 2"
    (Ok { c = Some 1; d = None })
    x;
  let x = of_json_string t "{\"d\":{\"some\":1}}" in
  Alcotest.(check (ok testable_t))
    "Decode nested option 3"
    (Ok { c = None; d = Some (Some 1) })
    x;
  let x = of_json_string t "{\"d\":null}" in
  Alcotest.(check (ok testable_t))
    "Decode nested option 4"
    (Ok { c = None; d = Some None })
    x

let l =
  let hex = T.map (T.string_of (`Fixed 3)) ~cli:(pp_hex, of_hex_string) id id in
  T.list ~len:(`Fixed 2) hex

let tl = Alcotest.testable (T.pp l) (T.equal l)

let hash =
  Alcotest.testable (T.pp Irmin.Hash.BLAKE2B.t) (T.equal Irmin.Hash.BLAKE2B.t)

let sha1 x = Irmin.Hash.BLAKE2B.hash (fun l -> l x)

let test_bin () =
  let s = T.to_string l [ "foo"; "foo" ] in
  Alcotest.(check string) "hex list" "[\"666f6f\",\"666f6f\"]" s;
  let s = T.to_bin_string l [ "foo"; "bar" ] in
  Alcotest.(check string) "encode list" "foobar" s;
  Alcotest.(check (option int))
    "size of list" (Some 6)
    (T.size_of l [ "foo"; "bar" ]);
  let s = T.of_bin_string l "foobar" in
  Alcotest.(check (ok tl)) "decode list" (Ok [ "foo"; "bar" ]) s;
  let buf = Buffer.create 10 in
  T.encode_bin ~headers:true T.string "foo" (Buffer.add_string buf);
  Alcotest.(check string) "foo 1" (Buffer.contents buf) "\003foo";
  let buf = Buffer.create 10 in
  T.encode_bin ~headers:false T.string "foo" (Buffer.add_string buf);
  Alcotest.(check string) "foo 1" (Buffer.contents buf) "foo";
  let buf = Buffer.create 10 in
  let h = sha1 "foo" in
  T.encode_bin ~headers:false Irmin.Hash.BLAKE2B.t h (Buffer.add_string buf);
  let x = Buffer.contents buf in
  let buf = Bytes.create 100 in
  Bytes.blit_string x 0 buf 0 (String.length x);
  let n, v =
    T.decode_bin ~headers:false Irmin.Hash.BLAKE2B.t
      (Bytes.unsafe_to_string buf)
      0
  in
  Alcotest.(check int) "hash size" n Irmin.Hash.BLAKE2B.hash_size;
  Alcotest.(check hash) "hash" v h

module Algebraic = struct
  (* Dummy algebraic types and corresponding generics *)

  type my_enum = Alpha | Beta | Gamma | Delta [@@deriving irmin]

  type my_variant = Left of int | Right of int list [@@deriving irmin]

  type my_recursive_variant =
    | Branch of my_recursive_variant list
    | Leaf of int
  [@@deriving irmin]

  type my_record = { foo : int; flag : bool; letter : my_enum }
  [@@deriving irmin]

  type my_recursive_record = { head : int; tail : my_recursive_record option }
  [@@deriving irmin]
end

(** Test the behaviour of {!Irmin.Type.to_string}. *)
let test_to_string () =
  let test : type a. string -> a T.t -> a -> string -> unit =
   fun case_name typ input expected_output ->
    let assertion =
      Fmt.strf "Expected output of `to_string` for representation of `%s`"
        case_name
    in
    T.to_string typ input |> Alcotest.(check string) assertion expected_output
  in

  (* Test cases for basic types *)
  test "unit" T.unit () "";
  test "bool{true}" T.bool true "true";
  test "bool{false}" T.bool false "false";
  test "char" T.char 'a' "a";
  test "int" T.int (-100) "-100";
  test "int32" T.int32 Int32.max_int "2147483647";
  test "int64" T.int64 Int64.max_int "9223372036854775807";
  test "float" T.float (-1.5) "-1.5";
  test "float{NaN}" T.float Stdlib.nan "nan";
  test "bytes" T.bytes (Bytes.make 5 'a') "aaaaa";
  test "string" T.string "foo\nbar\\" "foo\nbar\\";

  (* Test cases for non-algebraic combinators *)
  test "int list{nil}" T.(list int) [] "[]";
  test "int list{cons}" T.(list int) [ 1; 2; 3 ] "[1,2,3]";
  test "float array"
    T.(array float)
    [|
      Stdlib.neg_infinity;
      ~-.0.;
      0.;
      Stdlib.epsilon_float;
      Stdlib.infinity;
      Stdlib.nan;
    |]
    "[-inf,-0,0,2.220446049250313e-16,inf,nan]";
  test "(unit * int)" T.(pair unit int) ((), 1) "[{},1]";
  test "unit option{some}" T.(option unit) (Some ()) "{\"some\":{}}";
  test "int option{none}" T.(option unit) None "null";
  test "(int * string * bool)"
    T.(triple int string bool)
    (1, "foo", true) "[1,\"foo\",1]";
  test "(string, bool) result{ok}"
    T.(result string bool)
    (Ok "foo") "{\"ok\":\"foo\"}";
  test "(string, bool) result{error}"
    T.(result string bool)
    (Error false) "{\"error\":0}";

  (* Test cases for algebraic combinators *)
  let open Algebraic in
  test "enum" my_enum_t Alpha "\"Alpha\"";
  test "variant" my_variant_t (Right [ 1; 2 ]) "{\"Right\":[1,2]}";
  test "recursive variant" my_recursive_variant_t
    (Branch [ Branch [ Leaf 1 ]; Leaf 2 ])
    "{\"Branch\":[{\"Branch\":[{\"Leaf\":1}]},{\"Leaf\":2}]}";
  test "record" my_record_t
    { foo = 2; flag = false; letter = Delta }
    "{\"foo\":2,\"flag\":0,\"letter\":\"Delta\"}";

  test "recursive record" my_recursive_record_t
    { head = 1; tail = Some { head = 2; tail = None } }
    "{\"head\":1,\"tail\":{\"head\":2}}";

  ()

(** Test the behaviour of {!Irmin.Type.pp_ty}. *)
let test_pp_ty () =
  let test : type a. ?case_name:string -> a T.t -> string -> unit =
   fun ?case_name input expected_output ->
    let case_name =
      match case_name with Some x -> x | None -> expected_output
    in
    let assertion =
      Fmt.strf "Expected output of `pp_ty` for representation of `%s`" case_name
    in
    (Fmt.to_to_string T.pp_ty) input
    |> Alcotest.(check string) assertion expected_output
  in

  (* Test cases for basic types *)
  test T.unit "unit";
  test T.bool "bool";
  test T.char "char";
  test T.int "int";
  test T.int32 "int32";
  test T.int64 "int64";
  test T.float "float";
  test T.bytes "bytes";
  test T.string "string";

  (* Test cases for non-algebraic combinators *)
  test T.(list int) "int list";
  test T.(array float) "float array";
  test T.(pair unit int) "(unit * int)";
  test T.(option unit) "unit option";
  test T.(triple int string bool) "(int * string * bool)";

  test ~case_name:"(string, bool) result"
    T.(result string bool)
    "([ Ok of string | Error of bool ] as result)";

  (* Test cases for fixed-size refinement types *)
  test ~case_name:"string {size=Int}" T.(string_of `Int) "string";
  test ~case_name:"string {size=Int8}" T.(string_of `Int8) "string:8";
  test ~case_name:"bytes {size=Int16}" T.(bytes_of `Int16) "bytes:16";
  test ~case_name:"bytes {size=Int32}" T.(bytes_of `Int32) "bytes:32";
  test ~case_name:"array {size=Int64}"
    T.(array ~len:`Int64 unit)
    "unit array:64";
  test ~case_name:"array {size=3}"
    T.(array ~len:(`Fixed 3) unit)
    "unit array:<3>";

  (* Test cases for algebraic combinators *)
  test ~case_name:"empty" T.empty "({} as empty)";

  test ~case_name:"enum" Algebraic.my_enum_t
    "([ Alpha | Beta | Gamma | Delta ] as my_enum)";

  test ~case_name:"variant" Algebraic.my_variant_t
    "([ Left of int | Right of int list ] as my_variant)";

  test ~case_name:"recursive variant" Algebraic.my_recursive_variant_t
    "([ Branch of my_recursive_variant list | Leaf of int ] as \
     my_recursive_variant)";

  test ~case_name:"record" Algebraic.my_record_t
    {|(< foo : int
 ; flag : bool
 ; letter : ([ Alpha | Beta | Gamma | Delta ] as my_enum)
 > as my_record)|};

  test ~case_name:"recursive record" Algebraic.my_recursive_record_t
    "(< head : int; tail : my_recursive_record option > as my_recursive_record)";

  (* Test cases for mutually-recursive types *)
  let module Mu = struct
    type tree = Empty | Node of node

    and node = tree * int * tree

    let tree_t, node_t =
      let open T in
      mu2 (fun tree node ->
          ( variant "tree" (fun empty node ->
              function Empty -> empty | Node n -> node n)
            |~ case0 "empty" Empty
            |~ case1 "node" node (fun n -> Node n)
            |> sealv,
            triple tree int tree ))
  end in
  test ~case_name:"tree_t" Mu.tree_t
    "([ Empty | Node of ((tree * int * tree) as 'a) ] as tree)";
  test ~case_name:"node_t" Mu.node_t
    "((([ Empty | Node of 'a ] as tree) * int * ([ Empty | Node of 'a ] as \
     tree)) as 'a)";

  (* Test cases for 'custom' types *)
  let module Custom = struct
    type empty = { v : 'a. 'a }

    let v : empty T.t =
      let a1 _ = assert false in
      let a2 _ _ = assert false in
      let hdr f ?headers:_ = f in
      T.v ~cli:(a2, a1) ~json:(a2, a1)
        ~bin:(hdr a2, hdr a2, hdr a1)
        ~equal:a2 ~compare:a2
        ~short_hash:(fun ?seed:_ -> a1)
        ~pre_hash:a2

    let like_prim : int T.t = T.(like int)

    let like_custom : empty T.t = T.like v

    let map : int T.t = T.(map int) (fun x -> x) (fun x -> x)
  end in
  test ~case_name:"custom v" Custom.v "Custom (-)";
  test ~case_name:"custom like prim" Custom.like_prim "Custom (int)";
  test ~case_name:"custom like custom" Custom.like_custom "Custom (Custom (-))";
  test ~case_name:"map" Custom.map "Map (int)";

  ()

let x = T.like ~compare:(fun x y -> y - x - 1) T.int

let test_compare () =
  Alcotest.(check int) "rev compare" (T.compare x 1 2) 0;
  Alcotest.(check int) "rev compare" (T.compare x 2 1) (-2);
  Alcotest.(check int) "rev compare" (T.compare x 1 1) (-1);
  Alcotest.(check bool) "rev equal" (T.equal x 1 2) true;
  Alcotest.(check bool) "rev equal" (T.equal x 1 1) false

let x = T.like ~equal:(fun x y -> x - y = 2) T.int

let test_equal () =
  Alcotest.(check int) "eq1" (T.compare x 1 2) (compare 1 2);
  Alcotest.(check int) "eq2" (T.compare x 3 1) (compare 3 1);
  Alcotest.(check bool) "eq3" (T.equal x 3 1) true;
  Alcotest.(check bool) "eq4" (T.equal x 0 0) false;
  let a =
    `O [ ("b", `Float 2.); ("c", `A [ `String "test" ]); ("a", `Bool true) ]
  in
  let b =
    `O [ ("a", `Bool true); ("b", `Float 2.); ("c", `A [ `String "test" ]) ]
  in
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
    | Some ss -> Alcotest.(check int) (Fmt.strf "size:%d" x) s ss
    | None -> Alcotest.fail "size"
  in
  let p7 = 128 in
  let p14 = 16384 in
  let p21 = 2097152 in
  let p28 = 268435456 in
  let p35 = 34359738368 in
  let p42 = 4398046511104 in
  let p49 = 562949953421312 in
  let p56 = 72057594037927936 in
  (*  let p63 = max_int in *)
  let ps = [ p7; p14; p21; p28; p35; p42; p49; p56 (* p63 *) ] in
  List.iter
    (fun p ->
      test T.int (p - 1);
      test T.int p;
      test T.int (p + 1))
    (0 :: ps);
  test T.(list int) [];
  test T.string "";
  test T.string (String.make p14 'x');
  List.iter
    (fun p -> if p > 0 && p < p28 then test T.(array int) (Array.make p 42))
    ps;
  size 0 1;
  List.iteri
    (fun i p ->
      size (p - 1) (i + 1);
      size p (i + 2))
    ps

let test_decode () =
  let wrap f =
    try Ok (f ()) with e -> Fmt.kstrf (fun s -> Error s) "%a" Fmt.exn e
  in
  let decode ~off buf exp =
    match (exp, wrap (fun () -> T.decode_bin T.string buf off)) with
    | Error (), Error _ -> ()
    | Ok x, Ok (_, y) -> Alcotest.(check string) ("decode " ^ x) x y
    | Error _, Ok (_, y) -> Alcotest.failf "error expected, got %s" y
    | Ok x, Error e -> Alcotest.failf "expected: %s, got error: %s" x e
  in
  decode ~off:2 "xx\003aaayyy" (Ok "aaa");
  decode ~off:2 "xx\003aa" (Error ());
  decode ~off:2 "xx\002aa" (Ok "aa");
  decode ~off:2 "xx\000aaaaa" (Ok "")

let test_size () =
  let check t v n =
    match Irmin.Type.size_of t v with
    | Some s ->
        let name = Fmt.strf "size: %a" (Irmin.Type.pp t) v in
        Alcotest.(check int) name n s
    | None -> Alcotest.fail "size expected"
  in
  check Irmin.Type.int 0 1;
  check Irmin.Type.int 128 2;
  check Irmin.Type.int 16384 3;
  check Irmin.Type.string "foo" (1 + 3);
  check Irmin.Type.string (String.make 128 'x') (2 + 128);
  check Irmin.Type.bytes (Bytes.of_string "foo") 4;
  check Irmin.Type.(list string) [] 1;
  let s = T.size_of ~headers:false T.string "foo" in
  Alcotest.(check (option int)) "foo 1" (Some 3) s;
  let s = T.size_of ~headers:true T.string "foo" in
  Alcotest.(check (option int)) "foo 1" (Some 4) s

module Hash = Irmin.Hash.SHA1
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None
module Node = Irmin.Private.Node.Make (Hash) (Path) (Metadata)
module Node_v1 = Irmin.Private.Node.V1 (Node)
module Commit = Irmin.Private.Commit.Make (Hash)
module Commit_v1 = Irmin.Private.Commit.V1 (Commit)
module Hash_v1 = Irmin.Hash.V1 (Hash)
module H = Irmin.Hash.Typed (Hash) (Irmin.Contents.String)

let hash = H.hash

module H_v1 = Irmin.Hash.Typed (Hash_v1) (Irmin.Contents.V1.String)

let hash_v1 = H_v1.hash

let test_hashes () =
  let digest t x =
    let s = Irmin.Type.to_bin_string t x in
    Printf.eprintf "to_bin_string: %S\n" s;
    Irmin.Type.to_string Hash.t (Hash.hash (fun l -> l s))
  in
  Alcotest.(check string)
    "empty contents" "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    (digest Irmin.Contents.String.t "");
  Alcotest.(check string)
    "empty v1 contents" "05fe405753166f125559e7c9ac558654f107c7e9"
    (digest Irmin.Contents.V1.String.t "");
  Alcotest.(check string)
    "contents" "b60d121b438a380c343d5ec3c2037564b82ffef3"
    (digest Irmin.Contents.String.t "xxx");
  Alcotest.(check string)
    "empty node" "5ba93c9db0cff93f52b521d7420e43f6eda2784f"
    (digest Node.t Node.empty);
  Alcotest.(check string)
    "empty v1 node" "05fe405753166f125559e7c9ac558654f107c7e9"
    (digest Node_v1.t Node_v1.empty);
  let n1 v hash =
    v
      [
        ("foo", `Contents (hash "", Metadata.default));
        ("bar", `Node (hash "bar"));
      ]
  in
  Alcotest.(check string)
    "node" "38920183f8b667f6b643b1c4e524a95b55b20d31"
    (digest Node.t (n1 Node.v hash));
  Alcotest.(check string)
    "node v1" "bc5615e070d2838b278a1de0bb63fe325dd9cb11"
    (digest Node_v1.t (n1 Node_v1.v hash_v1));
  let v1 v hash = v ~info:Irmin.Info.empty ~node:(hash "toto") ~parents:[] in
  let v2 v hash =
    v
      ~info:(Irmin.Info.v ~date:42L ~author:"yay" "\bfoo\bar")
      ~node:(hash "toto") ~parents:[ hash "xxx"; hash "yyy" ]
  in
  Alcotest.(check string)
    "commit 1" "31c7871af72105ccf25e527fc00c14c9cafbd280"
    (digest Commit.t (v1 Commit.v hash));
  Alcotest.(check string)
    "commit v1 1" "d37cc867cc6eca1b6818b0bc36fef3ffed5cc6d2"
    (digest Commit_v1.t (v1 Commit_v1.v hash_v1));
  Alcotest.(check string)
    "commit 2" "2311a8c81b36dd2360a6c3a581c5699940423470"
    (digest Commit.t (v2 Commit.v hash));
  Alcotest.(check string)
    "commit v1 2" "e113f06cd69ed367dd7e4690f373a743cd4852b4"
    (digest Commit_v1.t (v2 Commit_v1.v hash_v1))

type v =
  [ `X000 | `X001 | `X002 | `X003 of int | `X004 of int | `X005 of int
  | `X006 of int | `X007 of int | `X008 of int | `X009 of int | `X010 of int
  | `X011 of int | `X012 of int | `X013 of int | `X014 of int | `X015 of int
  | `X016 of int | `X017 of int | `X018 of int | `X019 of int | `X020 of int
  | `X021 of int | `X022 of int | `X023 of int | `X024 of int | `X025 of int
  | `X026 of int | `X027 of int | `X028 of int | `X029 of int | `X030 of int
  | `X031 of int | `X032 of int | `X033 of int | `X034 of int | `X035 of int
  | `X036 of int | `X037 of int | `X038 of int | `X039 of int | `X040 of int
  | `X041 of int | `X042 of int | `X043 of int | `X044 of int | `X045 of int
  | `X046 of int | `X047 of int | `X048 of int | `X049 of int | `X050 of int
  | `X051 of int | `X052 of int | `X053 of int | `X054 of int | `X055 of int
  | `X056 of int | `X057 of int | `X058 of int | `X059 of int | `X060 of int
  | `X061 of int | `X062 of int | `X063 of int | `X064 of int | `X065 of int
  | `X066 of int | `X067 of int | `X068 of int | `X069 of int | `X070 of int
  | `X071 of int | `X072 of int | `X073 of int | `X074 of int | `X075 of int
  | `X076 of int | `X077 of int | `X078 of int | `X079 of int | `X080 of int
  | `X081 of int | `X082 of int | `X083 of int | `X084 of int | `X085 of int
  | `X086 of int | `X087 of int | `X088 of int | `X089 of int | `X090 of int
  | `X091 of int | `X092 of int | `X093 of int | `X094 of int | `X095 of int
  | `X096 of int | `X097 of int | `X098 of int | `X099 of int | `X100 of int
  | `X101 of int | `X102 of int | `X103 of int | `X104 of int | `X105 of int
  | `X106 of int | `X107 of int | `X108 of int | `X109 of int | `X110 of int
  | `X111 of int | `X112 of int | `X113 of int | `X114 of int | `X115 of int
  | `X116 of int | `X117 of int | `X118 of int | `X119 of int | `X120 of int
  | `X121 of int | `X122 of int | `X123 of int | `X124 of int | `X125 of int
  | `X126 of int | `X127 of int | `X128 of int | `X129 of int | `X130 of int
  | `X131 of int | `X132 of int | `X133 of int | `X134 of int | `X135 of int
  | `X136 of int | `X137 of int | `X138 of int | `X139 of int | `X140 of int
  | `X141 of int | `X142 of int | `X143 of int | `X144 of int | `X145 of int
  | `X146 of int | `X147 of int | `X148 of int | `X149 of int | `X150 of int
  | `X151 of int | `X152 of int | `X153 of int | `X154 of int | `X155 of int
  | `X156 of int | `X157 of int | `X158 of int | `X159 of int | `X160 of int
  | `X161 of int | `X162 of int | `X163 of int | `X164 of int | `X165 of int
  | `X166 of int | `X167 of int | `X168 of int | `X169 of int | `X170 of int
  | `X171 of int | `X172 of int | `X173 of int | `X174 of int | `X175 of int
  | `X176 of int | `X177 of int | `X178 of int | `X179 of int | `X180 of int
  | `X181 of int | `X182 of int | `X183 of int | `X184 of int | `X185 of int
  | `X186 of int | `X187 of int | `X188 of int | `X189 of int | `X190 of int
  | `X191 of int | `X192 of int | `X193 of int | `X194 of int | `X195 of int
  | `X196 of int | `X197 of int | `X198 of int | `X199 of int | `X200 of int
  | `X201 of int | `X202 of int | `X203 of int | `X204 of int | `X205 of int
  | `X206 of int | `X207 of int | `X208 of int | `X209 of int | `X210 of int
  | `X211 of int | `X212 of int | `X213 of int | `X214 of int | `X215 of int
  | `X216 of int | `X217 of int | `X218 of int | `X219 of int | `X220 of int
  | `X221 of int | `X222 of int | `X223 of int | `X224 of int | `X225 of int
  | `X226 of int | `X227 of int | `X228 of int | `X229 of int | `X230 of int
  | `X231 of int | `X232 of int | `X233 of int | `X234 of int | `X235 of int
  | `X236 of int | `X237 of int | `X238 of int | `X239 of int | `X240 of int
  | `X241 of int | `X242 of int | `X243 of int | `X244 of int | `X245 of int
  | `X246 of int | `X247 of int | `X248 of int | `X249 of int | `X250 of int
  | `X251 of int | `X252 of int | `X253 of int | `X254 of int | `X255 of int
  | `X256 of int | `X257 of int | `X258 of int | `X259 of int ]
[@@deriving irmin { name = "v"}] [@@ocamlformat "disable"]

let v_t = Alcotest.testable (Irmin.Type.pp v) (Irmin.Type.equal v)

let test_variants () =
  let test i =
    let x = Irmin.Type.to_bin_string v i in
    let y =
      match Irmin.Type.of_bin_string v x with
      | Ok x -> x
      | Error (`Msg e) -> failwith e
    in
    let n = Irmin.Type.size_of v i in
    let s = Irmin.Type.to_string v i in
    Alcotest.(check (option int)) ("sizes " ^ s) (Some (String.length x)) n;
    Alcotest.(check v_t) ("bij " ^ s) i y
  in
  test `X000;
  test (`X259 0);
  test (`X259 1024);
  test (`X259 (1024 * 1024))

(* Test that reusing the same name for different fields raises. *)
let test_duplicate_names () =
  let open Irmin.Type in
  Alcotest.check_raises "Two record fields with the same name."
    (Invalid_argument
       "The name foo was used for two or more fields in record bar.") (fun () ->
      ignore
        ( record "bar" (fun a b -> { a; b })
        |+ field "foo" int (fun r -> r.a)
        |+ field "foo" int (fun r -> r.b)
        |> sealr ));

  Alcotest.check_raises "Two variant case0 with the same name."
    (Invalid_argument
       "The name Foo was used for two or more case0 in variant or enum bar.")
    (fun () ->
      ignore
        ( variant "bar" (fun a b -> function `A -> a | `B -> b)
        |~ case0 "Foo" `A
        |~ case0 "Foo" `B
        |> sealv ));

  Alcotest.check_raises "Two variant case1 with the same name."
    (Invalid_argument
       "The name Foo was used for two or more case1 in variant or enum bar.")
    (fun () ->
      ignore
        ( variant "bar" (fun a b -> function `A i -> a i | `B i -> b i)
        |~ case1 "Foo" int (fun i -> `A i)
        |~ case1 "Foo" int (fun i -> `B i)
        |> sealv ));

  (* Check that we don't raise when two cases have the same name but different arity. *)
  ignore
    ( variant "bar" (fun a b -> function `A -> a | `B i -> b i)
    |~ case0 "Foo" `A
    |~ case1 "Foo" int (fun i -> `B i)
    |> sealv );

  Alcotest.check_raises "Two enum cases with the same name."
    (Invalid_argument
       "The name Foo was used for two or more case0 in variant or enum bar.")
    (fun () -> ignore (enum "bar" [ ("Foo", `A); ("Foo", `B) ]))

(* Test that using malformed UTF-8 in field and case names raises. *)
let test_malformed_utf8 () =
  Alcotest.check_raises "Malformed UTF-8 in field name"
    (Invalid_argument "Malformed UTF-8") (fun () ->
      let open Irmin.Type in
      ignore
        ( record "foo" (fun a b -> { a; b })
        |+ field "a" int (fun r -> r.a)
        |+ field "\128\255\255\r\012\247" int (fun r -> r.b) ));
  Alcotest.check_raises "Malformed UTF-8 in case0 name"
    (Invalid_argument "Malformed UTF-8") (fun () ->
      let open Irmin.Type in
      ignore
        ( variant "foo" (fun a -> function `A -> a)
        |~ case0 "\128\255\255\r\012\247" `A ));
  Alcotest.check_raises "Malformed UTF-8 in case1 name"
    (Invalid_argument "Malformed UTF-8") (fun () ->
      let open Irmin.Type in
      ignore
        ( variant "foo" (fun a -> function `A i -> a i)
        |~ case1 "\128\255\255\r\012\247" int (fun i -> `A i) ));
  Alcotest.check_raises "Malformed UTF-8 in enum tag name"
    (Invalid_argument "Malformed UTF-8") (fun () ->
      let open Irmin.Type in
      ignore (enum "foo" [ ("\128\255\255\r\012\247", `A) ]))

let suite =
  [
    ( "type",
      [
        ("base", `Quick, test_base);
        ("json", `Quick, test_json);
        ("json_option", `Quick, test_json_option);
        ("bin", `Quick, test_bin);
        ("to_string", `Quick, test_to_string);
        ("pp_ty", `Quick, test_pp_ty);
        ("compare", `Quick, test_compare);
        ("equal", `Quick, test_equal);
        ("ints", `Quick, test_int);
        ("decode", `Quick, test_decode);
        ("size_of", `Quick, test_size);
        ("test_hashes", `Quick, test_hashes);
        ("test_variants", `Quick, test_variants);
        ("test_duplicate_names", `Quick, test_duplicate_names);
        ("test_malformed_utf8", `Quick, test_malformed_utf8);
      ] );
  ]

let () = Alcotest.run "irmin" suite
