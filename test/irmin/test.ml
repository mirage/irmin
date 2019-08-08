module T = Irmin.Type

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
  Alcotest.(check string) "CLI string" "42" s

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
    (Ok "\128\129a")

let l =
  let hex =
    T.map (T.string_of (`Fixed 3)) ~cli:(pp_hex, of_hex_string) id id
  in
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
  Alcotest.(check bool)
    "json eq"
    (T.equal Irmin.Contents.Json_value.t a b)
    true

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

let v =
  let open Irmin.Type in
  variant "v"
    (fun x000
         x001
         x002
         x003
         x004
         x005
         x006
         x007
         x008
         x009
         x010
         x011
         x012
         x013
         x014
         x015
         x016
         x017
         x018
         x019
         x020
         x021
         x022
         x023
         x024
         x025
         x026
         x027
         x028
         x029
         x030
         x031
         x032
         x033
         x034
         x035
         x036
         x037
         x038
         x039
         x040
         x041
         x042
         x043
         x044
         x045
         x046
         x047
         x048
         x049
         x050
         x051
         x052
         x053
         x054
         x055
         x056
         x057
         x058
         x059
         x060
         x061
         x062
         x063
         x064
         x065
         x066
         x067
         x068
         x069
         x070
         x071
         x072
         x073
         x074
         x075
         x076
         x077
         x078
         x079
         x080
         x081
         x082
         x083
         x084
         x085
         x086
         x087
         x088
         x089
         x090
         x091
         x092
         x093
         x094
         x095
         x096
         x097
         x098
         x099
         x100
         x101
         x102
         x103
         x104
         x105
         x106
         x107
         x108
         x109
         x110
         x111
         x112
         x113
         x114
         x115
         x116
         x117
         x118
         x119
         x120
         x121
         x122
         x123
         x124
         x125
         x126
         x127
         x128
         x129
         x130
         x131
         x132
         x133
         x134
         x135
         x136
         x137
         x138
         x139
         x140
         x141
         x142
         x143
         x144
         x145
         x146
         x147
         x148
         x149
         x150
         x151
         x152
         x153
         x154
         x155
         x156
         x157
         x158
         x159
         x160
         x161
         x162
         x163
         x164
         x165
         x166
         x167
         x168
         x169
         x170
         x171
         x172
         x173
         x174
         x175
         x176
         x177
         x178
         x179
         x180
         x181
         x182
         x183
         x184
         x185
         x186
         x187
         x188
         x189
         x190
         x191
         x192
         x193
         x194
         x195
         x196
         x197
         x198
         x199
         x200
         x201
         x202
         x203
         x204
         x205
         x206
         x207
         x208
         x209
         x210
         x211
         x212
         x213
         x214
         x215
         x216
         x217
         x218
         x219
         x220
         x221
         x222
         x223
         x224
         x225
         x226
         x227
         x228
         x229
         x230
         x231
         x232
         x233
         x234
         x235
         x236
         x237
         x238
         x239
         x240
         x241
         x242
         x243
         x244
         x245
         x246
         x247
         x248
         x249
         x250
         x251
         x252
         x253
         x254
         x255
         x256
         x257
         x258
         x259
         ->
    function
    | `X000 -> x000
    | `X001 -> x001
    | `X002 -> x002
    | `X003 x -> x003 x
    | `X004 x -> x004 x
    | `X005 x -> x005 x
    | `X006 x -> x006 x
    | `X007 x -> x007 x
    | `X008 x -> x008 x
    | `X009 x -> x009 x
    | `X010 x -> x010 x
    | `X011 x -> x011 x
    | `X012 x -> x012 x
    | `X013 x -> x013 x
    | `X014 x -> x014 x
    | `X015 x -> x015 x
    | `X016 x -> x016 x
    | `X017 x -> x017 x
    | `X018 x -> x018 x
    | `X019 x -> x019 x
    | `X020 x -> x020 x
    | `X021 x -> x021 x
    | `X022 x -> x022 x
    | `X023 x -> x023 x
    | `X024 x -> x024 x
    | `X025 x -> x025 x
    | `X026 x -> x026 x
    | `X027 x -> x027 x
    | `X028 x -> x028 x
    | `X029 x -> x029 x
    | `X030 x -> x030 x
    | `X031 x -> x031 x
    | `X032 x -> x032 x
    | `X033 x -> x033 x
    | `X034 x -> x034 x
    | `X035 x -> x035 x
    | `X036 x -> x036 x
    | `X037 x -> x037 x
    | `X038 x -> x038 x
    | `X039 x -> x039 x
    | `X040 x -> x040 x
    | `X041 x -> x041 x
    | `X042 x -> x042 x
    | `X043 x -> x043 x
    | `X044 x -> x044 x
    | `X045 x -> x045 x
    | `X046 x -> x046 x
    | `X047 x -> x047 x
    | `X048 x -> x048 x
    | `X049 x -> x049 x
    | `X050 x -> x050 x
    | `X051 x -> x051 x
    | `X052 x -> x052 x
    | `X053 x -> x053 x
    | `X054 x -> x054 x
    | `X055 x -> x055 x
    | `X056 x -> x056 x
    | `X057 x -> x057 x
    | `X058 x -> x058 x
    | `X059 x -> x059 x
    | `X060 x -> x060 x
    | `X061 x -> x061 x
    | `X062 x -> x062 x
    | `X063 x -> x063 x
    | `X064 x -> x064 x
    | `X065 x -> x065 x
    | `X066 x -> x066 x
    | `X067 x -> x067 x
    | `X068 x -> x068 x
    | `X069 x -> x069 x
    | `X070 x -> x070 x
    | `X071 x -> x071 x
    | `X072 x -> x072 x
    | `X073 x -> x073 x
    | `X074 x -> x074 x
    | `X075 x -> x075 x
    | `X076 x -> x076 x
    | `X077 x -> x077 x
    | `X078 x -> x078 x
    | `X079 x -> x079 x
    | `X080 x -> x080 x
    | `X081 x -> x081 x
    | `X082 x -> x082 x
    | `X083 x -> x083 x
    | `X084 x -> x084 x
    | `X085 x -> x085 x
    | `X086 x -> x086 x
    | `X087 x -> x087 x
    | `X088 x -> x088 x
    | `X089 x -> x089 x
    | `X090 x -> x090 x
    | `X091 x -> x091 x
    | `X092 x -> x092 x
    | `X093 x -> x093 x
    | `X094 x -> x094 x
    | `X095 x -> x095 x
    | `X096 x -> x096 x
    | `X097 x -> x097 x
    | `X098 x -> x098 x
    | `X099 x -> x099 x
    | `X100 x -> x100 x
    | `X101 x -> x101 x
    | `X102 x -> x102 x
    | `X103 x -> x103 x
    | `X104 x -> x104 x
    | `X105 x -> x105 x
    | `X106 x -> x106 x
    | `X107 x -> x107 x
    | `X108 x -> x108 x
    | `X109 x -> x109 x
    | `X110 x -> x110 x
    | `X111 x -> x111 x
    | `X112 x -> x112 x
    | `X113 x -> x113 x
    | `X114 x -> x114 x
    | `X115 x -> x115 x
    | `X116 x -> x116 x
    | `X117 x -> x117 x
    | `X118 x -> x118 x
    | `X119 x -> x119 x
    | `X120 x -> x120 x
    | `X121 x -> x121 x
    | `X122 x -> x122 x
    | `X123 x -> x123 x
    | `X124 x -> x124 x
    | `X125 x -> x125 x
    | `X126 x -> x126 x
    | `X127 x -> x127 x
    | `X128 x -> x128 x
    | `X129 x -> x129 x
    | `X130 x -> x130 x
    | `X131 x -> x131 x
    | `X132 x -> x132 x
    | `X133 x -> x133 x
    | `X134 x -> x134 x
    | `X135 x -> x135 x
    | `X136 x -> x136 x
    | `X137 x -> x137 x
    | `X138 x -> x138 x
    | `X139 x -> x139 x
    | `X140 x -> x140 x
    | `X141 x -> x141 x
    | `X142 x -> x142 x
    | `X143 x -> x143 x
    | `X144 x -> x144 x
    | `X145 x -> x145 x
    | `X146 x -> x146 x
    | `X147 x -> x147 x
    | `X148 x -> x148 x
    | `X149 x -> x149 x
    | `X150 x -> x150 x
    | `X151 x -> x151 x
    | `X152 x -> x152 x
    | `X153 x -> x153 x
    | `X154 x -> x154 x
    | `X155 x -> x155 x
    | `X156 x -> x156 x
    | `X157 x -> x157 x
    | `X158 x -> x158 x
    | `X159 x -> x159 x
    | `X160 x -> x160 x
    | `X161 x -> x161 x
    | `X162 x -> x162 x
    | `X163 x -> x163 x
    | `X164 x -> x164 x
    | `X165 x -> x165 x
    | `X166 x -> x166 x
    | `X167 x -> x167 x
    | `X168 x -> x168 x
    | `X169 x -> x169 x
    | `X170 x -> x170 x
    | `X171 x -> x171 x
    | `X172 x -> x172 x
    | `X173 x -> x173 x
    | `X174 x -> x174 x
    | `X175 x -> x175 x
    | `X176 x -> x176 x
    | `X177 x -> x177 x
    | `X178 x -> x178 x
    | `X179 x -> x179 x
    | `X180 x -> x180 x
    | `X181 x -> x181 x
    | `X182 x -> x182 x
    | `X183 x -> x183 x
    | `X184 x -> x184 x
    | `X185 x -> x185 x
    | `X186 x -> x186 x
    | `X187 x -> x187 x
    | `X188 x -> x188 x
    | `X189 x -> x189 x
    | `X190 x -> x190 x
    | `X191 x -> x191 x
    | `X192 x -> x192 x
    | `X193 x -> x193 x
    | `X194 x -> x194 x
    | `X195 x -> x195 x
    | `X196 x -> x196 x
    | `X197 x -> x197 x
    | `X198 x -> x198 x
    | `X199 x -> x199 x
    | `X200 x -> x200 x
    | `X201 x -> x201 x
    | `X202 x -> x202 x
    | `X203 x -> x203 x
    | `X204 x -> x204 x
    | `X205 x -> x205 x
    | `X206 x -> x206 x
    | `X207 x -> x207 x
    | `X208 x -> x208 x
    | `X209 x -> x209 x
    | `X210 x -> x210 x
    | `X211 x -> x211 x
    | `X212 x -> x212 x
    | `X213 x -> x213 x
    | `X214 x -> x214 x
    | `X215 x -> x215 x
    | `X216 x -> x216 x
    | `X217 x -> x217 x
    | `X218 x -> x218 x
    | `X219 x -> x219 x
    | `X220 x -> x220 x
    | `X221 x -> x221 x
    | `X222 x -> x222 x
    | `X223 x -> x223 x
    | `X224 x -> x224 x
    | `X225 x -> x225 x
    | `X226 x -> x226 x
    | `X227 x -> x227 x
    | `X228 x -> x228 x
    | `X229 x -> x229 x
    | `X230 x -> x230 x
    | `X231 x -> x231 x
    | `X232 x -> x232 x
    | `X233 x -> x233 x
    | `X234 x -> x234 x
    | `X235 x -> x235 x
    | `X236 x -> x236 x
    | `X237 x -> x237 x
    | `X238 x -> x238 x
    | `X239 x -> x239 x
    | `X240 x -> x240 x
    | `X241 x -> x241 x
    | `X242 x -> x242 x
    | `X243 x -> x243 x
    | `X244 x -> x244 x
    | `X245 x -> x245 x
    | `X246 x -> x246 x
    | `X247 x -> x247 x
    | `X248 x -> x248 x
    | `X249 x -> x249 x
    | `X250 x -> x250 x
    | `X251 x -> x251 x
    | `X252 x -> x252 x
    | `X253 x -> x253 x
    | `X254 x -> x254 x
    | `X255 x -> x255 x
    | `X256 x -> x256 x
    | `X257 x -> x257 x
    | `X258 x -> x258 x
    | `X259 x -> x259 x)
  |~ case0 "x000" `X000
  |~ case0 "x001" `X001
  |~ case0 "x002" `X002
  |~ case1 "x003" int (fun x -> `X003 x)
  |~ case1 "x004" int (fun x -> `X004 x)
  |~ case1 "x005" int (fun x -> `X005 x)
  |~ case1 "x006" int (fun x -> `X006 x)
  |~ case1 "x007" int (fun x -> `X007 x)
  |~ case1 "x008" int (fun x -> `X008 x)
  |~ case1 "x009" int (fun x -> `X009 x)
  |~ case1 "x010" int (fun x -> `X010 x)
  |~ case1 "x011" int (fun x -> `X011 x)
  |~ case1 "x012" int (fun x -> `X012 x)
  |~ case1 "x013" int (fun x -> `X013 x)
  |~ case1 "x014" int (fun x -> `X014 x)
  |~ case1 "x015" int (fun x -> `X015 x)
  |~ case1 "x016" int (fun x -> `X016 x)
  |~ case1 "x017" int (fun x -> `X017 x)
  |~ case1 "x018" int (fun x -> `X018 x)
  |~ case1 "x019" int (fun x -> `X019 x)
  |~ case1 "x020" int (fun x -> `X020 x)
  |~ case1 "x021" int (fun x -> `X021 x)
  |~ case1 "x022" int (fun x -> `X022 x)
  |~ case1 "x023" int (fun x -> `X023 x)
  |~ case1 "x024" int (fun x -> `X024 x)
  |~ case1 "x025" int (fun x -> `X025 x)
  |~ case1 "x026" int (fun x -> `X026 x)
  |~ case1 "x027" int (fun x -> `X027 x)
  |~ case1 "x028" int (fun x -> `X028 x)
  |~ case1 "x029" int (fun x -> `X029 x)
  |~ case1 "x030" int (fun x -> `X030 x)
  |~ case1 "x031" int (fun x -> `X031 x)
  |~ case1 "x032" int (fun x -> `X032 x)
  |~ case1 "x033" int (fun x -> `X033 x)
  |~ case1 "x034" int (fun x -> `X034 x)
  |~ case1 "x035" int (fun x -> `X035 x)
  |~ case1 "x036" int (fun x -> `X036 x)
  |~ case1 "x037" int (fun x -> `X037 x)
  |~ case1 "x038" int (fun x -> `X038 x)
  |~ case1 "x039" int (fun x -> `X039 x)
  |~ case1 "x040" int (fun x -> `X040 x)
  |~ case1 "x041" int (fun x -> `X041 x)
  |~ case1 "x042" int (fun x -> `X042 x)
  |~ case1 "x043" int (fun x -> `X043 x)
  |~ case1 "x044" int (fun x -> `X044 x)
  |~ case1 "x045" int (fun x -> `X045 x)
  |~ case1 "x046" int (fun x -> `X046 x)
  |~ case1 "x047" int (fun x -> `X047 x)
  |~ case1 "x048" int (fun x -> `X048 x)
  |~ case1 "x049" int (fun x -> `X049 x)
  |~ case1 "x050" int (fun x -> `X050 x)
  |~ case1 "x051" int (fun x -> `X051 x)
  |~ case1 "x052" int (fun x -> `X052 x)
  |~ case1 "x053" int (fun x -> `X053 x)
  |~ case1 "x054" int (fun x -> `X054 x)
  |~ case1 "x055" int (fun x -> `X055 x)
  |~ case1 "x056" int (fun x -> `X056 x)
  |~ case1 "x057" int (fun x -> `X057 x)
  |~ case1 "x058" int (fun x -> `X058 x)
  |~ case1 "x059" int (fun x -> `X059 x)
  |~ case1 "x060" int (fun x -> `X060 x)
  |~ case1 "x061" int (fun x -> `X061 x)
  |~ case1 "x062" int (fun x -> `X062 x)
  |~ case1 "x063" int (fun x -> `X063 x)
  |~ case1 "x064" int (fun x -> `X064 x)
  |~ case1 "x065" int (fun x -> `X065 x)
  |~ case1 "x066" int (fun x -> `X066 x)
  |~ case1 "x067" int (fun x -> `X067 x)
  |~ case1 "x068" int (fun x -> `X068 x)
  |~ case1 "x069" int (fun x -> `X069 x)
  |~ case1 "x070" int (fun x -> `X070 x)
  |~ case1 "x071" int (fun x -> `X071 x)
  |~ case1 "x072" int (fun x -> `X072 x)
  |~ case1 "x073" int (fun x -> `X073 x)
  |~ case1 "x074" int (fun x -> `X074 x)
  |~ case1 "x075" int (fun x -> `X075 x)
  |~ case1 "x076" int (fun x -> `X076 x)
  |~ case1 "x077" int (fun x -> `X077 x)
  |~ case1 "x078" int (fun x -> `X078 x)
  |~ case1 "x079" int (fun x -> `X079 x)
  |~ case1 "x080" int (fun x -> `X080 x)
  |~ case1 "x081" int (fun x -> `X081 x)
  |~ case1 "x082" int (fun x -> `X082 x)
  |~ case1 "x083" int (fun x -> `X083 x)
  |~ case1 "x084" int (fun x -> `X084 x)
  |~ case1 "x085" int (fun x -> `X085 x)
  |~ case1 "x086" int (fun x -> `X086 x)
  |~ case1 "x087" int (fun x -> `X087 x)
  |~ case1 "x088" int (fun x -> `X088 x)
  |~ case1 "x089" int (fun x -> `X089 x)
  |~ case1 "x090" int (fun x -> `X090 x)
  |~ case1 "x091" int (fun x -> `X091 x)
  |~ case1 "x092" int (fun x -> `X092 x)
  |~ case1 "x093" int (fun x -> `X093 x)
  |~ case1 "x094" int (fun x -> `X094 x)
  |~ case1 "x095" int (fun x -> `X095 x)
  |~ case1 "x096" int (fun x -> `X096 x)
  |~ case1 "x097" int (fun x -> `X097 x)
  |~ case1 "x098" int (fun x -> `X098 x)
  |~ case1 "x099" int (fun x -> `X099 x)
  |~ case1 "x100" int (fun x -> `X100 x)
  |~ case1 "x101" int (fun x -> `X101 x)
  |~ case1 "x102" int (fun x -> `X102 x)
  |~ case1 "x103" int (fun x -> `X103 x)
  |~ case1 "x104" int (fun x -> `X104 x)
  |~ case1 "x105" int (fun x -> `X105 x)
  |~ case1 "x106" int (fun x -> `X106 x)
  |~ case1 "x107" int (fun x -> `X107 x)
  |~ case1 "x108" int (fun x -> `X108 x)
  |~ case1 "x109" int (fun x -> `X109 x)
  |~ case1 "x110" int (fun x -> `X110 x)
  |~ case1 "x111" int (fun x -> `X111 x)
  |~ case1 "x112" int (fun x -> `X112 x)
  |~ case1 "x113" int (fun x -> `X113 x)
  |~ case1 "x114" int (fun x -> `X114 x)
  |~ case1 "x115" int (fun x -> `X115 x)
  |~ case1 "x116" int (fun x -> `X116 x)
  |~ case1 "x117" int (fun x -> `X117 x)
  |~ case1 "x118" int (fun x -> `X118 x)
  |~ case1 "x119" int (fun x -> `X119 x)
  |~ case1 "x120" int (fun x -> `X120 x)
  |~ case1 "x121" int (fun x -> `X121 x)
  |~ case1 "x122" int (fun x -> `X122 x)
  |~ case1 "x123" int (fun x -> `X123 x)
  |~ case1 "x124" int (fun x -> `X124 x)
  |~ case1 "x125" int (fun x -> `X125 x)
  |~ case1 "x126" int (fun x -> `X126 x)
  |~ case1 "x127" int (fun x -> `X127 x)
  |~ case1 "x128" int (fun x -> `X128 x)
  |~ case1 "x129" int (fun x -> `X129 x)
  |~ case1 "x130" int (fun x -> `X130 x)
  |~ case1 "x131" int (fun x -> `X131 x)
  |~ case1 "x132" int (fun x -> `X132 x)
  |~ case1 "x133" int (fun x -> `X133 x)
  |~ case1 "x134" int (fun x -> `X134 x)
  |~ case1 "x135" int (fun x -> `X135 x)
  |~ case1 "x136" int (fun x -> `X136 x)
  |~ case1 "x137" int (fun x -> `X137 x)
  |~ case1 "x138" int (fun x -> `X138 x)
  |~ case1 "x139" int (fun x -> `X139 x)
  |~ case1 "x140" int (fun x -> `X140 x)
  |~ case1 "x141" int (fun x -> `X141 x)
  |~ case1 "x142" int (fun x -> `X142 x)
  |~ case1 "x143" int (fun x -> `X143 x)
  |~ case1 "x144" int (fun x -> `X144 x)
  |~ case1 "x145" int (fun x -> `X145 x)
  |~ case1 "x146" int (fun x -> `X146 x)
  |~ case1 "x147" int (fun x -> `X147 x)
  |~ case1 "x148" int (fun x -> `X148 x)
  |~ case1 "x149" int (fun x -> `X149 x)
  |~ case1 "x150" int (fun x -> `X150 x)
  |~ case1 "x151" int (fun x -> `X151 x)
  |~ case1 "x152" int (fun x -> `X152 x)
  |~ case1 "x153" int (fun x -> `X153 x)
  |~ case1 "x154" int (fun x -> `X154 x)
  |~ case1 "x155" int (fun x -> `X155 x)
  |~ case1 "x156" int (fun x -> `X156 x)
  |~ case1 "x157" int (fun x -> `X157 x)
  |~ case1 "x158" int (fun x -> `X158 x)
  |~ case1 "x159" int (fun x -> `X159 x)
  |~ case1 "x160" int (fun x -> `X160 x)
  |~ case1 "x161" int (fun x -> `X161 x)
  |~ case1 "x162" int (fun x -> `X162 x)
  |~ case1 "x163" int (fun x -> `X163 x)
  |~ case1 "x164" int (fun x -> `X164 x)
  |~ case1 "x165" int (fun x -> `X165 x)
  |~ case1 "x166" int (fun x -> `X166 x)
  |~ case1 "x167" int (fun x -> `X167 x)
  |~ case1 "x168" int (fun x -> `X168 x)
  |~ case1 "x169" int (fun x -> `X169 x)
  |~ case1 "x170" int (fun x -> `X170 x)
  |~ case1 "x171" int (fun x -> `X171 x)
  |~ case1 "x172" int (fun x -> `X172 x)
  |~ case1 "x173" int (fun x -> `X173 x)
  |~ case1 "x174" int (fun x -> `X174 x)
  |~ case1 "x175" int (fun x -> `X175 x)
  |~ case1 "x176" int (fun x -> `X176 x)
  |~ case1 "x177" int (fun x -> `X177 x)
  |~ case1 "x178" int (fun x -> `X178 x)
  |~ case1 "x179" int (fun x -> `X179 x)
  |~ case1 "x180" int (fun x -> `X180 x)
  |~ case1 "x181" int (fun x -> `X181 x)
  |~ case1 "x182" int (fun x -> `X182 x)
  |~ case1 "x183" int (fun x -> `X183 x)
  |~ case1 "x184" int (fun x -> `X184 x)
  |~ case1 "x185" int (fun x -> `X185 x)
  |~ case1 "x186" int (fun x -> `X186 x)
  |~ case1 "x187" int (fun x -> `X187 x)
  |~ case1 "x188" int (fun x -> `X188 x)
  |~ case1 "x189" int (fun x -> `X189 x)
  |~ case1 "x190" int (fun x -> `X190 x)
  |~ case1 "x191" int (fun x -> `X191 x)
  |~ case1 "x192" int (fun x -> `X192 x)
  |~ case1 "x193" int (fun x -> `X193 x)
  |~ case1 "x194" int (fun x -> `X194 x)
  |~ case1 "x195" int (fun x -> `X195 x)
  |~ case1 "x196" int (fun x -> `X196 x)
  |~ case1 "x197" int (fun x -> `X197 x)
  |~ case1 "x198" int (fun x -> `X198 x)
  |~ case1 "x199" int (fun x -> `X199 x)
  |~ case1 "x200" int (fun x -> `X200 x)
  |~ case1 "x201" int (fun x -> `X201 x)
  |~ case1 "x202" int (fun x -> `X202 x)
  |~ case1 "x203" int (fun x -> `X203 x)
  |~ case1 "x204" int (fun x -> `X204 x)
  |~ case1 "x205" int (fun x -> `X205 x)
  |~ case1 "x206" int (fun x -> `X206 x)
  |~ case1 "x207" int (fun x -> `X207 x)
  |~ case1 "x208" int (fun x -> `X208 x)
  |~ case1 "x209" int (fun x -> `X209 x)
  |~ case1 "x210" int (fun x -> `X210 x)
  |~ case1 "x211" int (fun x -> `X211 x)
  |~ case1 "x212" int (fun x -> `X212 x)
  |~ case1 "x213" int (fun x -> `X213 x)
  |~ case1 "x214" int (fun x -> `X214 x)
  |~ case1 "x215" int (fun x -> `X215 x)
  |~ case1 "x216" int (fun x -> `X216 x)
  |~ case1 "x217" int (fun x -> `X217 x)
  |~ case1 "x218" int (fun x -> `X218 x)
  |~ case1 "x219" int (fun x -> `X219 x)
  |~ case1 "x220" int (fun x -> `X220 x)
  |~ case1 "x221" int (fun x -> `X221 x)
  |~ case1 "x222" int (fun x -> `X222 x)
  |~ case1 "x223" int (fun x -> `X223 x)
  |~ case1 "x224" int (fun x -> `X224 x)
  |~ case1 "x225" int (fun x -> `X225 x)
  |~ case1 "x226" int (fun x -> `X226 x)
  |~ case1 "x227" int (fun x -> `X227 x)
  |~ case1 "x228" int (fun x -> `X228 x)
  |~ case1 "x229" int (fun x -> `X229 x)
  |~ case1 "x230" int (fun x -> `X230 x)
  |~ case1 "x231" int (fun x -> `X231 x)
  |~ case1 "x232" int (fun x -> `X232 x)
  |~ case1 "x233" int (fun x -> `X233 x)
  |~ case1 "x234" int (fun x -> `X234 x)
  |~ case1 "x235" int (fun x -> `X235 x)
  |~ case1 "x236" int (fun x -> `X236 x)
  |~ case1 "x237" int (fun x -> `X237 x)
  |~ case1 "x238" int (fun x -> `X238 x)
  |~ case1 "x239" int (fun x -> `X239 x)
  |~ case1 "x240" int (fun x -> `X240 x)
  |~ case1 "x241" int (fun x -> `X241 x)
  |~ case1 "x242" int (fun x -> `X242 x)
  |~ case1 "x243" int (fun x -> `X243 x)
  |~ case1 "x244" int (fun x -> `X244 x)
  |~ case1 "x245" int (fun x -> `X245 x)
  |~ case1 "x246" int (fun x -> `X246 x)
  |~ case1 "x247" int (fun x -> `X247 x)
  |~ case1 "x248" int (fun x -> `X248 x)
  |~ case1 "x249" int (fun x -> `X249 x)
  |~ case1 "x250" int (fun x -> `X250 x)
  |~ case1 "x251" int (fun x -> `X251 x)
  |~ case1 "x252" int (fun x -> `X252 x)
  |~ case1 "x253" int (fun x -> `X253 x)
  |~ case1 "x254" int (fun x -> `X254 x)
  |~ case1 "x255" int (fun x -> `X255 x)
  |~ case1 "x256" int (fun x -> `X256 x)
  |~ case1 "x257" int (fun x -> `X257 x)
  |~ case1 "x258" int (fun x -> `X258 x)
  |~ case1 "x259" int (fun x -> `X259 x)
  |> sealv

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

let suite =
  [
    ( "type",
      [
        ("base", `Quick, test_base);
        ("json", `Quick, test_json);
        ("bin", `Quick, test_bin);
        ("compare", `Quick, test_compare);
        ("equal", `Quick, test_equal);
        ("ints", `Quick, test_int);
        ("decode", `Quick, test_decode);
        ("size_of", `Quick, test_size);
        ("test_hashes", `Quick, test_hashes);
        ("test_variants", `Quick, test_variants);
      ] );
  ]

let () = Alcotest.run "irmin" suite
