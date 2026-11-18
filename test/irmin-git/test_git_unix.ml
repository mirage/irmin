include Irmin.Export_for_backends

let stats watcher () =
  let stats = Irmin_watcher.stats watcher in
  (stats.Irmin_watcher.watchdogs, Irmin.Backend.Watch.workers ())

let test_db = Test_git.test_db

let init ~sw ~fs ~watcher ~config =
  let test_db =
    Irmin.Backend.Conf.find_root config |> Option.value ~default:test_db
  in
  assert (test_db <> ".git");
  let () =
    if Sys.file_exists test_db then
      Lwt_eio.run_lwt @@ fun () ->
      let open Lwt.Infix in
      Git_unix.Store.v (Fpath.v test_db) >>= function
      | Ok t -> Git_unix.Store.reset t >|= fun _ -> ()
      | Error _ -> Lwt.return_unit
  in
  Irmin.Backend.Watch.set_listen_dir_eio_hook ~sw fs
    (Irmin_watcher.hook watcher)

module S = struct
  module G = Git_unix.Store
  include Irmin_git_unix.FS.KV (Irmin.Contents.String)
end

let store ~sw ~fs ~watcher =
  let module S = struct
    include S

    let init = init ~sw ~fs ~watcher
  end in
  (module S : Test_git.G)

let clean ~config:_ = Irmin.Backend.Watch.(set_listen_dir_hook none)

let config =
  let head = Git.Reference.v "refs/heads/test" in
  Irmin_git.config ~head ~bare:true test_db

let suite ~sw ~fs ~watcher =
  let store = (module S : Irmin_test.S) in
  Irmin_test.Suite.create ~name:"GIT.UNIX" ~init:(init ~sw ~fs ~watcher) ~store
    ~config ~clean ~stats:(stats watcher) ()

let test_non_bare ~sw ~fs ~watcher () =
  let config = Irmin_git.config ~bare:false test_db in
  init ~sw ~fs ~watcher ~config;
  let info = Irmin_git_unix.info in
  let repo = S.Repo.v config in
  let t = S.main repo in
  S.set_exn t ~info:(info "fst one") [ "fst" ] "ok";
  S.set_exn t ~info:(info "snd one") [ "fst"; "snd" ] "maybe?";
  S.set_exn t ~info:(info "fst one") [ "fst" ] "hoho"

let misc ~sw ~fs ~watcher : unit Alcotest.test_case list =
  [ ("non-bare", `Quick, test_non_bare ~sw ~fs ~watcher) ]
