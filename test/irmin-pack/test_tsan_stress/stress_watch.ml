(* Races the global [listen_dir_hook] / [watch_switch] refs in
   src/irmin/watch.ml:28-29. N domains concurrently overwrite the
   shared hook with [set_listen_dir_hook]; the assignment to the
   [ref] is unsynchronised. *)

let dummy_hook _ _ _ _ = ()

let run ~env ~iter =
  let dmgr = Eio.Stdenv.domain_mgr env in
  let worker () =
    for _ = 1 to iter do
      Irmin.Backend.Watch.set_listen_dir_hook dummy_hook
    done
  in
  Stress_common.domains_spawn ~dmgr ~nb:4 worker
