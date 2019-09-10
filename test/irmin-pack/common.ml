module Dict = Irmin_pack.Dict

let get = function Some x -> x | None -> Alcotest.fail "None"

let pp_hash = Irmin.Type.pp Irmin.Hash.SHA1.t

let hash = Alcotest.testable pp_hash (Irmin.Type.equal Irmin.Hash.SHA1.t)

let sha1 x = Irmin.Hash.SHA1.hash (fun f -> f x)

module S = struct
  include Irmin.Contents.String

  let magic _ = 'S'

  module H = Irmin.Hash.Typed (Irmin.Hash.SHA1) (Irmin.Contents.String)

  let hash = H.hash

  let encode_bin ~dict:_ ~offset:_ x k =
    Irmin.Type.(encode_bin ~headers:false (pair H.t t) (k, x))

  let decode_bin ~dict:_ ~hash:_ x off =
    let _, (_, v) =
      Irmin.Type.(decode_bin ~headers:false (pair H.t t) x off)
    in
    v
end

module H = Irmin.Hash.SHA1
module I = Irmin_pack.Index.Make (H)
module P = Irmin_pack.Pack.File (I) (H)
module Pack = P.Make (S)
module Index = Irmin_pack.Index.Make (Irmin.Hash.SHA1)
module Branch = Irmin_pack.Atomic_write (Irmin.Branch.String) (Irmin.Hash.SHA1)

let string_size = 20

let () = Random.self_init ()

let random_char () = char_of_int (33 + Random.int 94)

let random_string () = String.init string_size (fun _i -> random_char ())
