open Higher
open Irmin_type
open Overture
module Prism = Optics.Effectful.Prism

[@@@warning "-32"]

module Blake2B = struct
  include Digestif.BLAKE2B

  external get_64 : string -> int -> int64 = "%caml_string_get64u"

  let short_hash c = Int64.to_int (get_64 (to_raw_string c) 0)

  let hash_size = digest_size

  let of_hex s =
    match consistent_of_hex s with
    | x -> Ok x
    | exception Invalid_argument e -> Error (`Msg e)

  let pp_hex ppf x = Fmt.string ppf (to_hex x)

  let t =
    Type.map ~cli:(pp_hex, of_hex)
      Type.(string_of (`Fixed hash_size))
      of_raw_string to_raw_string
end

(* Generic hash-consing library *)
module Hash_constructor : sig
  (* Hash of a value *)
  type addr

  (* Heap effects *)
  type io

  val run : ('a, io) app -> 'a

  val io_class : io monad

  type leaf = { foo : int }

  type value = Branch of (value, addr) app list | Leaf of leaf

  val pp_value : value Fmt.t

  val addr : (value, addr) app Type.t

  val pure : value -> ((value, addr) app, io) app

  type value_prism =
    (value, value, (value, addr) app, (value, addr) app, io) Prism.t

  (* Augment prism composition with dereferencing *)
  val ( / ) : value_prism -> value_prism -> value_prism
end = struct
  module Io = Identity

  type io = Io.t

  let run = Io.prj

  let io_class = Identity.v

  module Addr = Newtype1 (struct
    type 'a t = Digestif.BLAKE2B.t
  end)

  type addr = Addr.t

  type leaf = { foo : int }

  type value = Branch of (value, Addr.t) app list | Leaf of leaf

  let pp_value ppf = function
    | Branch cs ->
        Fmt.(brackets (list ~sep:(comma ++ cut) string))
          ppf
          (cs |> List.map (Addr.prj >>> Blake2B.to_hex))
    | Leaf { foo = i } -> Fmt.pf ppf "{ foo : %d }" i

  let addr : (value, addr) app Type.t = Type.map Blake2B.t Addr.inj Addr.prj

  module M = Map.Make (struct
    include Digestif.BLAKE2B

    let compare = unsafe_compare
  end)

  let instance = ref M.empty

  let pure : value -> ((value, Addr.t) app, Io.t) app =
   fun x ->
    let hash =
      let marshalled = Marshal.to_string x [] in
      Digestif.BLAKE2B.digest_string marshalled
    in
    instance := M.add hash x !instance;
    Io.inj (Addr.inj hash)

  let deref : (value, Addr.t) app -> (value, Io.t) app =
   fun hash -> Io.inj (M.find (Addr.prj hash) !instance)

  type value_prism =
    (value, value, (value, addr) app, (value, addr) app, io) Prism.t

  (* Augment prism composition with dereferencing *)
  let ( / ) : value_prism -> value_prism -> value_prism =
    Prism.natural_compose pure deref
end

let test () =
  let open Hash_constructor in
  let ( >>| ) x f = io_class#fmap f x in
  let io = pure (Leaf { foo = 1 }) >>| fun _addr -> () in
  run io

let suite = [ ("optics.effects", [ ("foo", `Quick, test) ]) ]
