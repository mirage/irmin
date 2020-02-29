open Irmin_root
module Type = Irmin_type.Type
module Lens = Irmin_optics.Lens
module ELens = Irmin_optics.Effectful.Lens
module Prism = Irmin_optics.Prism
module EPrism = Irmin_optics.Effectful.Prism

type my_record = { name : string; flag : bool; count : int }

let get_name x = x.name

let get_flag x = x.flag

let get_count x = x.count

let test_stock_lens_view () =
  let x = (1, ("2", 3)) in
  let () =
    x
    |> Lens.(view snd)
    |> Lens.(view id)
    |> Lens.(view fst)
    |> Alcotest.(check string) "stock lenses can view independently" "2"
  in
  let () =
    x
    |> Lens.(view (snd >> id >> snd))
    |> Alcotest.(check int) "stock lenses can view when composed" 3
  in
  ()

let test_stock_lens_modify () =
  let x = (1, ("2", 3)) in
  let () =
    x
    |> Lens.(modify fst (( + ) 10))
    |> fst
    |> Alcotest.(check int) "stock lenses can update independently" 11
  in
  let () =
    x
    |> Lens.(update (snd >> id >> fst)) "20"
    |> (fun (_, (s, _)) -> s)
    |> Alcotest.(check string) "stock lenses can update when composed" "20"
  in
  ()

let (_my_record : my_record Type.t), ELens.[ name; flag; count ] =
  let open Type in
  record "my_record" (fun name flag count -> { name; flag; count })
  |+ field "name" string ~set:(fun s name -> { s with name }) (fun s -> s.name)
  |+ field "flag" bool ~set:(fun s flag -> { s with flag }) (fun s -> s.flag)
  |+ field "count" int ~set:(fun s count -> { s with count }) (fun s -> s.count)
  |> sealr_with_optics
  |> Pair.second (fun x -> x Identity.v)

let name, flag, count = (Lens.prj name, Lens.prj flag, Lens.prj count)

let test_generic_lens_view () =
  let r = { name = "foo"; flag = true; count = 2 } in
  r |> Lens.view name |> Alcotest.(check string) "name field viewable" "foo";
  r |> Lens.view flag |> Alcotest.(check bool) "flag field viewable" true;
  r |> Lens.view count |> Alcotest.(check int) "count field viewable" 2;
  ()

let test_generic_lens_modify () =
  let r = { name = "foo"; flag = true; count = 2 } in
  let () =
    r
    |> Lens.update name "bar"
    |> get_name
    |> Alcotest.(check string) "name field updatable" "bar"
  in
  let () =
    r
    |> Lens.update flag false
    |> get_flag
    |> Alcotest.(check bool) "flag field updatable" false
  in
  let () =
    r
    |> Lens.update count 3
    |> get_count
    |> Alcotest.(check int) "count field updatable" 3;
    ()
  in
  ()

let ( |>! ) x f =
  match x with Some x -> f x | None -> Alcotest.fail "Unexpected case: None"

let test_stock_prism_preview () =
  let x = Some (Ok [ Error 5; Ok (Some "2"); Ok None ]) in
  let () =
    x
    |> Prism.(preview some)
    |>! Prism.(preview ok)
    |>! Prism.(preview head)
    |>! Prism.(preview error)
    |>! Alcotest.(check int) "stock prisms can preview independently" 5
  in
  let () =
    x
    |> Prism.(preview (some >> ok >> tail >> head >> ok >> some))
    |>! Alcotest.(check string) "stock prisms can preview when composed" "2"
  in
  ()

let test_stock_prism_review () =
  let () =
    Prism.(review none ())
    |> Prism.(review error)
    |> Prism.(review head)
    |> Prism.(review some)
    |> Alcotest.(check (option (list (result int (option int)))))
         "stock prisms can review independently"
         (Some [ Error None ])
  in
  let () =
    Prism.(review (some >> head >> ok >> error) 5)
    |> Alcotest.(check (option (list (result (result string int) string))))
         "stock prisms can review when composed"
         (Some [ Ok (Error 5) ])
  in
  ()

type my_variant = A of string | B of int | C | D of my_variant

let (_my_variant : my_variant Type.t), EPrism.[ a; b; c; d ] =
  let open Type in
  mu_optics (fun v ->
      variant "my_record" (fun a b c d ->
        function A x -> a x | B x -> b x | C -> c | D x -> d x)
      |~ case1 "a" string (fun x -> A x)
      |~ case1 "b" int (fun x -> B x)
      |~ case0 "c" C
      |~ case1 "d" v (fun x -> D x)
      |> sealv_with_optics)
  |> Pair.second (fun x -> x Identity.v)

let a, b, c, d = (Prism.prj a, Prism.prj b, Prism.prj c, Prism.prj d)

let test_generic_prism_preview () =
  let () =
    D (D (D (B 2)))
    |> Prism.(preview d)
    |>! Prism.(preview d)
    |>! Prism.(preview d)
    |>! Prism.(preview b)
    |>! Alcotest.(check int) "generic prisms can preview independently" 2
  in
  let () =
    D (D (A "foo"))
    |> Prism.(preview (d >> d >> a))
    |>! Alcotest.(check string) "generic prisms can preview when composed" "foo"
  in
  ()

let test_generic_prism_review () =
  let () =
    (Prism.(review c ()) |> Prism.(review d) |> Prism.(review d)) |> function
    | D (D C) -> ()
    | _ -> Alcotest.fail "generic prisms can review independently"
  in
  let () =
    Prism.(review (d >> d >> a) "foo") |> function
    | D (D (A "foo")) -> ()
    | _ -> Alcotest.fail "generic prisms can review when composed"
  in
  ()

let suite =
  [
    ( "optics.lenses",
      [
        ("stock (view)", `Quick, test_stock_lens_view);
        ("stock (modify)", `Quick, test_stock_lens_modify);
        ("generic (view)", `Quick, test_generic_lens_view);
        ("generic (modify)", `Quick, test_generic_lens_modify);
      ] );
    ( "optics.prisms",
      [
        ("stock prisms (preview)", `Quick, test_stock_prism_preview);
        ("stock prisms (review)", `Quick, test_stock_prism_review);
        ("generic prisms (preview)", `Quick, test_generic_prism_preview);
        ("generic prisms (review)", `Quick, test_generic_prism_review);
      ] );
  ]
