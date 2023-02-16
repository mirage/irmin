open Lwt.Syntax
module Store = Irmin_mem.KV.Make (Irmin.Contents.String)
module Server = Irmin_server_unix.Make (Store)

let info () = Irmin.Info.Default.empty

let init () =
  let* repo = Store.Repo.v (Irmin_mem.config ()) in
  let* main = Store.main repo in
  let+ () = Store.set_exn ~info main [ "foo" ] "bar" in
  ()

let main () =
  let uri = Uri.of_string Sys.argv.(1) in
  let config = Irmin_mem.config () in
  let* server = Server.v ~uri config in
  let () = Format.printf "Listening on %a@." Uri.pp uri in
  Server.serve server

let () =
  Lwt_main.run
  @@ let* () = init () in
     main ()
