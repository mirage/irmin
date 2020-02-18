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

module Addr = Newtype1 (struct
  type 'a t = Digestif.BLAKE2B.t
end)

type addr = Addr.t
(** Hash of a value *)

module Heap (Contents : sig
  type t
end) : sig
  type contents

  type io
  (** The brand of Heap effects ( - => - ) *)

  val io : io monad
  (** Effects are monadic. *)

  val put : contents -> ((contents, addr) app, io) app

  val run : ('a, io) app -> 'a * (string * contents) list
  (** Run an effectful operation with an initially empty heap, returning the
      final bindings in the heap. *)
end
with type contents = Contents.t = struct
  type contents = Contents.t

  module M = Map.Make (struct
    include Digestif.BLAKE2B

    let compare = unsafe_compare
  end)

  module Io = State (struct
    type t = contents M.t
  end)

  type io = Io.t

  let io = Io.t

  type addr = Addr.t

  let ( let+ ) x f = io#fmap f x

  let ( let* ) x f = io#bind f x

  let run op =
    let a, s = io#run op M.empty in
    ( a,
      M.bindings s
      |> List.map
           (Pair.first (Digestif.BLAKE2B.to_hex >>> fun s -> String.sub s 0 8))
    )

  let put : contents -> ((contents, addr) app, io) app =
   fun v ->
    let* instance = io#get in
    let hash =
      let marshalled = Marshal.to_string v [] in
      Digestif.BLAKE2B.digest_string marshalled
    in
    let+ () = io#put (M.add hash v instance) in
    Addr.inj hash

  (* Don't expose the state monad to the user *)
  let io = (io :> io monad)
end

(* Generic hash-consing library *)
module Hash_constructor = struct
  type tree = [ `Tree of (string * tree) list | `Contents of int ]

  type node = Tree_object of (string * (node, addr) app) list | Blob of int

  module Heap = Heap (struct
    type t = node
  end)

  type io = Heap.io

  let io = Heap.io

  let ( let+ ) x f = io#fmap f x

  let ( let* ) x f = io#bind f x

  let rec pure : tree -> ((node, addr) app, io) app = function
    | `Contents c -> Heap.put (Blob c)
    | `Tree children ->
        (* Marshal the children *)
        let* child_hashes =
          children
          |> List.map (fun (s, c) ->
                 let+ addr = pure c in
                 (s, addr))
          |> Monad.sequence io
        in
        Heap.put (Tree_object child_hashes)

  (* let deref : (node, Addr.t) app -> (node, io) app =
   *  fun hash -> o.inj (Heap.find (Addr.prj hash) !instance) *)

  (* type tree_prism = (tree, tree, (tree, addr) app, (node, addr) app, io) Prism.t *)

  (* Augment prism composition with dereferencing *)
  (* let ( / ) : tree_prism -> tree_prism -> node_prism =
   *   Prism.natural_compose pure deref *)

  let pp_key =
    Fmt.string
    |> Fmt.using (Digestif.BLAKE2B.to_hex >>> fun s -> String.sub s 0 8)

  let pp_node ppf = function
    | Tree_object cs ->
        Fmt.Dump.(list (pair string (Fmt.using Addr.prj pp_key))) ppf cs
    | Blob i -> Fmt.int ppf i
end

let dump_bindings =
  let open Hash_constructor in
  let open Fmt in
  epr "@[<v>heap internals:@,@,%a@,@,@]@."
    (Dump.list (Dump.pair (using (fun s -> String.sub s 0 8) string) pp_node))

let test () =
  let open Hash_constructor in
  let ( >>| ) x f = io#fmap f x in
  let ( >>= ) x f = io#bind f x in
  let op =
    pure (`Contents 1) >>= fun _addr ->
    pure (`Contents 2) >>= fun _addr ->
    pure (`Contents 1) >>= fun _addr ->
    pure (`Tree [ ("foo", `Contents 1); ("bar", `Contents 2) ]) >>| fun _addr ->
    ()
  in
  let (), bindings = Heap.run op in
  dump_bindings bindings

let suite = [ ("optics.effects", [ ("foo", `Quick, test) ]) ]
