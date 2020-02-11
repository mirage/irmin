module Type = Irmin_type.Type
module Lens = Irmin_type.Optics.Lens
module ELens = Irmin_type.Optics.Effectful.Lens
module Prism = Irmin_type.Optics.Prism

let test_stock_lens_view () =
  let x = (1, ("2", 3)) in
  x
  |> Lens.(view snd)
  |> Lens.(view id)
  |> Lens.(view fst)
  |> Alcotest.(check string) "stock lenses work independently" "2";
  x
  |> Lens.(view (snd >> id >> snd))
  |> Alcotest.(check int) "stock lenses work when composed" 3

type my_record = { name : string; flag : bool; count : int }

let get_name x = x.name

let get_flag x = x.flag

let get_count x = x.count

let (_my_record : my_record Type.t), ELens.[ name; flag; count ] =
  let open Type in
  record "my_record" (fun name flag count -> { name; flag; count })
  |+ field "name" string ~set:(fun s name -> { s with name }) (fun s -> s.name)
  |+ field "flag" bool ~set:(fun s flag -> { s with flag }) (fun s -> s.flag)
  |+ field "count" int ~set:(fun s count -> { s with count }) (fun s -> s.count)
  |> sealr_with_optics

let name, flag, count = (Lens.prj name, Lens.prj flag, Lens.prj count)

let test_generic_lens_view () =
  let r = { name = "foo"; flag = true; count = 2 } in
  r |> Lens.view name |> Alcotest.(check string) "name field viewable" "foo";
  r |> Lens.view flag |> Alcotest.(check bool) "flag field viewable" true;
  r |> Lens.view count |> Alcotest.(check int) "count field viewable" 2;
  ()

let test_generic_lens_update () =
  let r = { name = "foo"; flag = true; count = 2 } in
  r
  |> Lens.update name "bar"
  |> get_name
  |> Alcotest.(check string) "name field updatable" "bar";
  r
  |> Lens.update flag false
  |> get_flag
  |> Alcotest.(check bool) "flag field updatable" false;
  r
  |> Lens.update count 3
  |> get_count
  |> Alcotest.(check int) "count field updatable" 3;
  ()

(* type my_variant =
 *   | A of string
 *   | B of int
 *   | C
 *   | D of bool * my_variant *)

let suite =
  [
    ( "optics",
      [
        ("stock lenses (view)", `Quick, test_stock_lens_view);
        ("generic lenses (view)", `Quick, test_generic_lens_view);
        ("generic lenses (update)", `Quick, test_generic_lens_update);
      ] );
  ]

let () = Alcotest.run "irmin.type" suite
