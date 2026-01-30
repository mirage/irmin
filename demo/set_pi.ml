module Fs_store = Irmin_fs_unix.KV.Make(Irmin.Contents.Json_value)
let commit branch message path contents =
  let info () = Fs_store.Info.v ~author:"jane doe" ~message 0L in
  Fs_store.set ~info branch path contents

let set_pi env =
  let root = Eio.Path.(env#fs / "math") in
  let conf = Irmin_fs_unix.config ~root ~clock:env#clock in
  let repo = Fs_store.Repo.v conf in
  let main = Fs_store.main repo in
  let pi = `O ["val", `Float 3.1416] in
  let result = commit main "set pi" ["pi"] pi in 
  assert (Result.is_ok result)

let _ = Eio_main.run set_pi
