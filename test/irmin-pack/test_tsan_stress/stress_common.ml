(* Barrier-synchronised domain spawning, mirroring the idiom in
   test/irmin-pack/test_multicore.ml so the workers start roughly
   together and the scheduler has to interleave them. *)

(* Clean scratch directory for scenarios that need on-disk state.
   Rooted at [/tmp/irmin-tsan-stress/<name>] to avoid clashing with
   a possibly missing or read-only [_build] under the process cwd. *)
let scratch_path ~env name =
  let fs = Eio.Stdenv.fs env in
  let base = Eio.Path.(fs / "tmp" / "irmin-tsan-stress") in
  let p = Eio.Path.(base / name) in
  (try Eio.Path.rmtree p with _ -> ());
  (try Eio.Path.mkdirs ~perm:0o755 base with _ -> ());
  (try Eio.Path.mkdir ~perm:0o755 p with _ -> ());
  p

(* Like [scratch_path] but returns a path to a file (non-existing)
   whose parent directory has been freshly created. *)
let scratch_file ~env name =
  let fs = Eio.Stdenv.fs env in
  let base = Eio.Path.(fs / "tmp" / "irmin-tsan-stress") in
  (try Eio.Path.mkdirs ~perm:0o755 base with _ -> ());
  let p = Eio.Path.(base / name) in
  (try Eio.Path.unlink p with _ -> ());
  p

let domains_run ~dmgr fns =
  let count = Atomic.make (List.length fns) in
  let fibers =
    List.map
      (fun fn () ->
        Eio.Domain_manager.run dmgr (fun () ->
            Atomic.decr count;
            while Atomic.get count > 0 do
              Domain.cpu_relax ()
            done;
            fn ()))
      fns
  in
  Eio.Fiber.all fibers

let domains_spawn ~dmgr ?(nb = 4) fn =
  domains_run ~dmgr (List.init nb (fun _ -> fn))
