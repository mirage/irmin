(* Races the shared [Eio_pool.t] instances ([mkdir_pool], [openfile_pool])
   in src/irmin-fs/unix/irmin_fs_unix.ml:75,87.

   A single FS-backed store is shared across N domains; each writes a
   distinct key. Store.set_exn goes through [with_write_file] which uses
   [openfile_pool]; creating the root directory uses [mkdir_pool]. The
   pool's internal state (queue, counter) is shared across domains. *)

module Store = Irmin_fs_unix.KV.Make (Irmin.Contents.String)

let run ~env ~iter =
  Eio.Switch.run @@ fun _sw ->
  let clock = Eio.Stdenv.clock env in
  let dmgr = Eio.Stdenv.domain_mgr env in
  let root = Stress_common.scratch_path ~env "stress_fs_pool" in
  let cfg = Irmin_fs_unix.config ~root ~clock in
  let repo = Store.Repo.v cfg in
  let main = Store.main repo in
  let info () = Store.Info.v 0L in
  let worker id () =
    for i = 1 to iter do
      Store.set_exn ~info main [ Printf.sprintf "k-%d-%d" id i ] "v"
    done
  in
  let fns = List.init 2 (fun id -> worker id) in
  Stress_common.domains_run ~dmgr fns;
  Store.Repo.close repo
