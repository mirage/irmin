open Higher
open Irmin_root
open Irmin_type

[@@@warning "-32"]

[@@@warning "-34"]

[@@@warning "-37"]

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

type 'a addr = Digestif.BLAKE2B.t
(** Hash of a value *)

let addr : 'a Type.t -> 'a addr Type.t =
 fun _ -> Type.(map string) Digestif.BLAKE2B.of_hex Digestif.BLAKE2B.to_hex

module Heap (Contents : sig
  type t
end) : sig
  type contents

  type io
  (** The brand of Heap effects ( - => - ) *)

  val io : io monad
  (** Effects are monadic. *)

  val put : contents -> (contents addr, io) app

  val get : contents addr -> (contents option, io) app

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

  let ( let+ ) x f = io#fmap f x

  let ( let* ) x f = io#bind f x

  let run op =
    let a, s = io#run op M.empty in
    ( a,
      M.bindings s
      |> List.map
           (Pair.first (Digestif.BLAKE2B.to_hex >>> fun s -> String.sub s 0 8))
    )

  let put : contents -> (contents addr, io) app =
   fun v ->
    let* instance = io#get in
    let hash =
      let marshalled = Marshal.to_string v [] in
      Digestif.BLAKE2B.digest_string marshalled
    in
    let+ () = io#put (M.add hash v instance) in
    hash

  let get : contents addr -> (contents option, io) app =
   fun a ->
    let+ instance = io#get in
    M.find_opt a instance

  (* Don't expose the state monad to the user *)
  let io = (io :> io monad)
end

(* Generic hash-consing library *)
module Hash_constructor (Contents : sig
  type t

  val t : t Type.t
end) =
struct
  type contents = Contents.t

  type concrete = [ `Tree of (string * concrete) list | `Contents of contents ]

  type node = Tree_object of (string * node addr) list | Blob of string

  module Heap = Heap (struct
    type t = contents
  end)

  type io = Heap.io

  let io = Heap.io

  let addr =
    let open Heap in
    Irmin_optics.Effectful.(Prism.v io put get |> Optional.of_prism)

  let ( let+ ) x f = io#fmap f x

  let ( let* ) x f = io#bind f x

  (* let set_opt :
   *     ('contents, 'contents, 'a addr, 'b addr, io) Optics.Effectful.Optional.t ->
   *     'b ->
   *     ('t, io) app =
   *  fun opt b ->
   *   Optics.Effectful.Optional.modify opt (fun _ -> io#return @@ Some b) s *)

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
    | Tree_object cs -> Fmt.Dump.(list (pair string pp_key)) ppf cs
    | Blob s -> Fmt.string ppf s
end

type 'a tree = Branch of (string * 'a tree) list | Leaf of 'a

let tree t =
  let open Type in
  mu_optics (fun tree ->
      variant "tree" (fun branch leaf ->
        function Branch b -> branch b | Leaf l -> leaf l)
      |~ case1 "branch" (list (pair string tree)) (fun b -> Branch b)
      |~ case1 "leaf" t (fun l -> Leaf l)
      |> sealv_with_optics)

let branch :
    string -> ('a tree, 'a tree, Identity.t) Irmin_optics.Effectful.Prism.mono =
 fun s ->
  Irmin_optics.Effectful.Prism.v Identity.v
    (fun b -> Branch [ (s, b) ] |> Identity.inj)
    (function
      | Leaf _ -> None |> Identity.inj
      | Branch children -> Some (List.assoc s children) |> Identity.inj)

let leaf : ('a tree, 'a, Identity.t) Irmin_optics.Effectful.Prism.mono =
  Irmin_optics.Effectful.Prism.v Identity.v
    (fun l -> Leaf l |> Identity.inj)
    (function
      | Branch _ -> None |> Identity.inj | Leaf l -> Some l |> Identity.inj)

let path : string list -> ('a tree, 'a, 'm) Irmin_optics.Effectful.Prism.mono =
  let open Irmin_optics.Effectful.Prism in
  fun ps -> List.map branch ps |> List.fold_left ( >> ) (id Identity.v) >> leaf

type my_store = {
  names : string tree addr;
  flags : bool tree addr;
  counts : int tree addr;
}

(* let ( (my_store : my_store Type.t),
 *       Irmin_optics.Effectful.Lens.[ names; flags; counts ] ) =
 *   let open Type in
 *   record "my_record" (fun names flags counts -> { names; flags; counts })
 *   |+ field "name"
 *        (tree string |> fst |> addr)
 *        ~set:(fun s names -> { s with names })
 *        (fun { names; _ } -> names)
 *   |+ field "flag"
 *        (tree bool |> fst |> addr)
 *        ~set:(fun s flags -> { s with flags })
 *        (fun { flags; _ } -> flags)
 *   |+ field "count"
 *        (tree int |> fst |> addr)
 *        ~set:(fun s counts -> { s with counts })
 *        (fun { counts; _ } -> counts)
 *   |> sealr_with_optics
 * 
 * module Optional = Irmin_optics.Effectful.Optional
 * 
 * let names, flags, counts =
 *   (Optional.of_lens names, Optional.of_lens flags, Optional.of_lens counts)
 * 
 * let path s = Optional.of_prism (path s)
 * 
 * module H = Hash_constructor (struct
 *   type t = my_store
 * 
 *   let t = my_store
 * end)
 * 
 * let dump_bindings =
 *   let open H in
 *   let open Fmt in
 *   epr "@[<v>heap internals:@,@,%a@,@,@]@."
 *     (Dump.list (Dump.pair (using (fun s -> String.sub s 0 8) string) pp_node))
 * 
 * let test () =
 *   let open H in
 *   let op =
 *     let ( >> ) = Optional.( >> ) in
 *     Optional.modify (Optional.lift io names >> H.addr >> path [ "a"; "b" ])
 *     (\* >>= fun _addr ->
 *      * pure (`Contents "2") >>= fun _addr ->
 *      * pure (`Contents "1") >>= fun _addr ->
 *      * pure (`Tree [ ("foo", `Contents "1"); ("bar", `Contents "2") ])
 *      * >>| fun _addr -> () *\)
 *   in
 * 
 *   let (), bindings = Heap.run op in
 *   dump_bindings bindings
 * *)

let suite = [ ("optics.effects", []) ]
