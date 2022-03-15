open! Import
module String_set = Irmin_data.Fixed_size_string_set

let hash : string -> int = Hashtbl.hash

let hash_substring : Bigstringaf.t -> off:int -> len:int -> int =
 fun t ~off ~len -> hash (Bigstringaf.substring t ~off ~len)

let test_add_and_mem () =
  let set =
    String_set.create ~elt_length:1 ~initial_slots:0 ~hash ~hash_substring ()
  in
  String_set.mem set "a" |> check_bool __POS__ ~expected:false;
  String_set.add_exn set "a";
  String_set.mem set "a" |> check_bool __POS__ ~expected:true;
  String_set.add_exn set "b";
  String_set.add_exn set "c";

  String_set.mem set "a" |> check_bool __POS__ ~expected:true;
  String_set.mem set "b" |> check_bool __POS__ ~expected:true;
  String_set.mem set "c" |> check_bool __POS__ ~expected:true;

  String_set.add set "a" |> check_ok_or_duplicate __POS__ ~expected:`Duplicate;
  String_set.add set "b" |> check_ok_or_duplicate __POS__ ~expected:`Duplicate;
  String_set.add set "c" |> check_ok_or_duplicate __POS__ ~expected:`Duplicate;
  String_set.add set "d" |> check_ok_or_duplicate __POS__ ~expected:`Ok

let test_random () =
  let elt_length = 8 in
  let set =
    String_set.create ~elt_length ~initial_slots:31 ~hash ~hash_substring ()
  in
  let reference_tbl = Stdlib.Hashtbl.create 0 in
  let reference_vector = Vector.create ~dummy:"" in
  let random_string () =
    String.init elt_length (fun _ -> char_of_int (Random.int 256))
  in
  for i = 0 to 10_000 do
    (* Add a new element: *)
    let new_elt = random_string () in
    String_set.add_exn set new_elt;
    Stdlib.Hashtbl.add reference_tbl new_elt ();
    Vector.push reference_vector new_elt;

    (* Pick a random existing element and check [mem] is true: *)
    let elt = Vector.get reference_vector (Random.int (i + 1)) in
    assert (Stdlib.Hashtbl.mem reference_tbl elt);
    String_set.mem set elt |> check_bool __POS__ ~expected:true;

    (* Pick a random non-existing element and check [mem] is false: *)
    let non_elt = random_string () in
    assert (not (Stdlib.Hashtbl.mem reference_tbl non_elt));
    String_set.mem set non_elt |> check_bool __POS__ ~expected:false;

    (* Check that the internal invariants hold, and that all internal elements
       are also contained in the reference: *)
    if i mod 1_000 = 0 then
      String_set.invariant
        (fun elt ->
          check_bool __POS__ ~expected:true
            (Stdlib.Hashtbl.mem reference_tbl elt))
        set
  done

let test_invalid_argument () =
  (* [create] *)
  let () =
    (* Must have a positive [elt_length]: *)
    check_invalid_arg __POS__ (fun () -> String_set.create ~elt_length:0 ());

    (* Cannot pass [hash] without passing [hash_substring] (and vice versa): *)
    check_invalid_arg __POS__ (fun () ->
        String_set.create ~elt_length:1 ~hash:(fun _ -> assert false) ());
    check_invalid_arg __POS__ (fun () ->
        String_set.create ~elt_length:1
          ~hash_substring:(fun _ -> assert false)
          ())
  in

  (* [add] *)
  let () =
    let null = "0" in
    let t = String_set.create ~elt_length:1 ~null () in

    (* Element must have the correct length: *)
    check_invalid_arg __POS__ (fun () -> String_set.add_exn t "");

    (* Cannot add the null element: *)
    check_invalid_arg __POS__ (fun () -> String_set.add_exn t null);

    (* An exception is raised on adding a duplicate element: *)
    String_set.add_exn t "a";
    check_invalid_arg __POS__ (fun () -> String_set.add_exn t "a")
  in

  ()

let tests =
  let test name fn = Alcotest.test_case name `Quick fn in
  [
    test "add_and_mem" test_add_and_mem;
    test "random" test_random;
    test "invalid_argument" test_invalid_argument;
  ]
