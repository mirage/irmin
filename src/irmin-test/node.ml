let check pos typ ~expected actual =
  let typ =
    Alcotest.testable Irmin.Type.(pp_dump typ) Irmin.Type.(unstage (equal typ))
  in
  Alcotest.(check ~pos typ) "" expected actual

module Make (Make_node : Irmin.Node.Generic_key.Maker) : sig
  val suite : unit Alcotest.test_case list
end = struct
  module Schema = Irmin.Schema.KV (Irmin.Contents.String)
  module Hash = Schema.Hash
  module Key = Irmin.Key.Of_hash (Hash)
  module X = Make_node (Hash) (Schema.Path) (Schema.Metadata) (Key) (Key)

  type key = Key.t [@@deriving irmin]

  let random_key =
    let hash_of_string = Irmin.Type.(unstage (of_bin_string Hash.t)) in
    let random_string =
      Irmin.Type.(unstage (random (string_of (`Fixed Hash.hash_size))))
    in
    fun () ->
      match hash_of_string (random_string ()) with
      | Ok x -> x
      | Error _ -> assert false

  let test_empty () =
    check __POS__ [%typ: bool] ~expected:true X.(is_empty (empty ()));
    check __POS__ [%typ: int] ~expected:0 X.(length (empty ()));
    check __POS__ [%typ: (X.step * X.value) list] ~expected:[]
      X.(list (empty ()))

  let test_add () =
    let with_binding k v t = X.add t k v in
    let k1 = random_key () and k2 = random_key () in
    let a =
      X.empty () |> with_binding "a" (`Node k1) |> with_binding "b" (`Node k2)
    in
    check __POS__ [%typ: int] ~expected:2 (X.length a)

  let test_remove () =
    (* Remove is a no-op on an empty node *)
    check __POS__ [%typ: X.t] ~expected:(X.empty ()) X.(remove (empty ()) "foo")

  let suite =
    [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "add" `Quick test_add;
      Alcotest.test_case "remove" `Quick test_remove;
    ]
end
