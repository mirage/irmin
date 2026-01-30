module Store = Irmin_fs_unix.KV.Make (Irmin.Contents.String)

let info () = Irmin.Info.Default.v ~message:"Commit" (Mtime_clock.now_ns ())

let main env =
  let conf =
    Irmin_fs_unix.config
      ~root:Eio.Path.(env#cwd / "irmin_example")
      ~clock:env#clock
  in
  let repo = Store.Repo.v conf in
  let main = Store.main repo in
  Store.set_exn ~info main [ "a" ] "hello";
  let b1 = Store.of_branch repo "exp1" in
  Store.set_exn ~info b1 [ "a" ] "world";
  let branches = Store.Branch.list repo in
  Eio.traceln "Branches: [%a]\n%!"
    Fmt.(list ~sep:(Fmt.any ", ") string)
    branches

let () = Eio_main.run main
