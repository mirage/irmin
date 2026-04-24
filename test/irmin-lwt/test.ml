(* Level-1 smoke test for irmin-lwt: exercise the minimal Repo/Store
   lifecycle through the Lwt-wrapped API against the in-memory backend. *)

module Backend = Irmin_mem.KV.Make (Irmin.Contents.String)
module Store = Irmin_lwt.Make (Backend)

let info message () = Backend.Info.v ~author:"irmin-lwt-test" ~message 0L
let contents = Alcotest.option Alcotest.string

let test_set_then_find _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "set foo") [ "foo" ] "bar" in
  let* v = Store.find t [ "foo" ] in
  Alcotest.check contents "foo -> bar" (Some "bar") v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_remove _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* () = Store.set_exn t ~info:(info "set foo") [ "foo" ] "bar" in
  let* () = Store.remove_exn t ~info:(info "remove foo") [ "foo" ] in
  let* v = Store.find t [ "foo" ] in
  Alcotest.check contents "foo is gone" None v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let test_missing_path _switch () =
  let open Lwt.Syntax in
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* t = Store.main repo in
  let* v = Store.find t [ "unset" ] in
  Alcotest.check contents "missing path" None v;
  let* () = Store.Repo.close repo in
  Lwt.return_unit

let () =
  Irmin_lwt.run @@ fun () ->
  Alcotest_lwt.run "irmin-lwt"
    [
      ( "smoke",
        [
          Alcotest_lwt.test_case "set then find" `Quick test_set_then_find;
          Alcotest_lwt.test_case "remove" `Quick test_remove;
          Alcotest_lwt.test_case "missing path" `Quick test_missing_path;
        ] );
    ]
