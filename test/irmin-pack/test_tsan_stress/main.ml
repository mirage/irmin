(* TSan stress suite — dispatcher.

   Each scenario targets a mutable-state hotspot that the standard test
   suite does not exercise across domains. Iteration count is driven by
   [IRMIN_TSAN_STRESS_ITER] (default 100).

   Scenarios and expected outcomes under TSan:

   - mem:   clean data-race warning at irmin_mem.ml:51 (Hashtbl.add in
            the global cache) and on the shared KMap mutable.
   - watch: clean data-race warning at watch.ml:33 (listen_dir_hook
            ref assignment).
   - ao:    SEGV — concurrent Buffer.add_string corrupts the buffer
            fast enough that TSan's signal handler fires before the
            race warning is written. The SEGV itself is evidence of
            the race; a clean warning would need finer-grained access.
   - dict:  SEGV — same pattern as ao, via the two unguarded Hashtbl.t
            caches plus the append path through Ao.
   - fs:    TSan "nested bug, aborting" — the race interacts with Eio
            scheduler/pool internals in a way the sanitizer can't
            unwind. Surfaces the problem but not a specific site.

   Because ao/dict/fs crash, each scenario is run in its own process by
   the @tsan-stress dune alias so one crash does not hide the others.

   Usage:
     main.exe                 run all scenarios
     main.exe all             same
     main.exe <name>          run one scenario (ao|dict|mem|watch|fs) *)

let iter_count =
  match Sys.getenv_opt "IRMIN_TSAN_STRESS_ITER" with
  | Some s -> int_of_string s
  | None -> 100

type env = Eio_unix.Stdenv.base

let scenarios : (string * (env:env -> iter:int -> unit)) list =
  [
    ("ao", Stress_ao_buf.run);
    ("dict", Stress_dict.run);
    ("mem", Stress_mem_cache.run);
    ("watch", Stress_watch.run);
    ("fs", Stress_fs_pool.run);
  ]

let run_one ~env (name, fn) =
  Printf.printf "tsan-stress: %s (iter=%d)\n%!" name iter_count;
  fn ~env ~iter:iter_count

let () =
  let which =
    match Sys.argv with
    | [| _ |] | [| _; "all" |] -> `All
    | [| _; name |] -> `One name
    | _ ->
        prerr_endline "usage: main.exe [all|ao|dict|mem|watch|fs]";
        exit 2
  in
  Eio_main.run @@ fun env ->
  match which with
  | `All -> List.iter (run_one ~env) scenarios
  | `One name -> (
      match List.assoc_opt name scenarios with
      | Some fn -> run_one ~env (name, fn)
      | None ->
          Printf.eprintf "tsan-stress: unknown scenario %S\n%!" name;
          exit 2)
