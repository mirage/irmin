module S = Irmin_tezos.Store
module Tree = S.Tree

let make_tree_of_paths paths =
  Array.fold_left
    (fun tree (path, contents) -> Tree.add tree path contents)
    (Tree.empty ()) paths

let goto_project_root () =
  let cwd = Fpath.v (Sys.getcwd ()) in
  match cwd |> Fpath.segs |> List.rev with
  | "bench_multicore" :: "irmin-pack" :: "test" :: "default" :: "_build" :: root
  | "bench_multicore" :: "irmin-pack" :: "test" :: root ->
      Unix.chdir @@ String.concat Fpath.dir_sep @@ List.rev root
  | _ -> ()

let root = Filename.concat "_build" "bench-multicore"

let reset_test_env () =
  goto_project_root ();
  Common.rm_dir root

let info () = S.Info.empty

let open_repo ~fresh ~readonly () =
  let conf = Irmin_pack.Conf.init ~fresh ~readonly root in
  S.Repo.v conf

let apply_op tree = function
  | Gen.Find path ->
      let _ = Tree.find tree path in
      tree
  | Add (path, contents) -> Tree.add tree path contents
  | Rem path -> Tree.remove tree path

let half_task tree task =
  let _ = Array.fold_left apply_op tree task in
  ()

let full_task i tree_at task =
  let path = [ string_of_int i ] in
  let tree = Atomic.get tree_at in
  let new_tree = Array.fold_left apply_op tree task in
  let new_subtree = Option.get @@ Tree.find_tree new_tree path in
  let rec update () =
    let current_tree = Atomic.get tree_at in
    let new_tree = Tree.add_tree current_tree path new_subtree in
    if not (Atomic.compare_and_set tree_at current_tree new_tree) then update ()
  in
  update ()

let warmup_task tree task =
  Array.iter
    (function
      | Gen.Find path | Add (path, _) | Rem path ->
          ignore @@ Tree.find tree path)
    task

let analyze_bench timers =
  let n = Array.length timers in
  Array.sort Float.compare timers;
  let mean = timers.(n / 2) in
  (timers.(0), mean, timers.(n - 1))

let bench ?(samples = 5) fn =
  let timers =
    Array.init samples (fun _ ->
        let t0 = Unix.gettimeofday () in
        fn ();
        let t1 = Unix.gettimeofday () in
        let sequential = 1000.0 *. (t1 -. t0) in
        sequential)
  in
  analyze_bench timers

let get_tree repo = S.main repo |> S.Head.get |> S.Commit.tree

let get_tree ~config repo tasks =
  if not config.Gen.warm then fun () -> get_tree repo
  else
    let tree = get_tree repo in
    Array.iter (warmup_task tree) tasks;
    fun () -> tree

let setup_tree ~sw ~readonly paths =
  let tree = make_tree_of_paths paths in
  reset_test_env ();
  let repo = open_repo ~sw ~fresh:true ~readonly:false () in
  let () = S.set_tree_exn ~info (S.main repo) [] tree in
  S.Repo.close repo;
  let repo = open_repo ~sw ~fresh:false ~readonly () in
  Format.printf
    "# domains,min_time,median_time,max_time,min_ratio,median_ratio,max_ratio@.";
  repo

let half ~d_mgr ~(config : Gen.config) =
  Eio.Switch.run @@ fun sw ->
  let paths, tasks = Gen.make ~config in
  let repo = setup_tree ~sw ~readonly:true paths in
  let get_tree = get_tree ~config repo tasks in

  let _, sequential, _ =
    bench ~samples:config.nb_runs @@ fun () ->
    let tree = get_tree () in
    Array.iter (half_task tree) tasks
  in

  for nb_domains = 1 to Domain.recommended_domain_count () do
    let elapsed = ref [] in
    for _ = 1 to config.nb_runs do
      let tree = get_tree () in
      let tasks = Array.map (fun task () -> half_task tree task) tasks in
      let dt = Workers.run ~d_mgr ~nb:nb_domains tasks in
      elapsed := dt :: !elapsed
    done;
    let min, median, max = analyze_bench @@ Array.of_list !elapsed in
    Format.printf "%i,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f@." nb_domains min median max
      (sequential /. max) (sequential /. median) (sequential /. min)
  done;
  S.Repo.close repo

let full ~d_mgr ~(config : Gen.config) =
  Eio.Switch.run @@ fun sw ->
  let paths, tasks = Gen.make_full ~config in
  let repo = setup_tree ~sw ~readonly:false paths in
  let get_tree = get_tree ~config repo tasks in
  let parents = [ S.Commit.key @@ S.Head.get @@ S.main repo ] in

  let commit tree_at () =
    let new_tree = Atomic.get tree_at in
    let _ = S.Commit.v repo ~parents ~info:S.Info.empty new_tree in
    ()
  in

  let _, sequential, _ =
    bench ~samples:config.nb_runs @@ fun () ->
    let tree_at = Atomic.make (get_tree ()) in
    Array.iteri (fun i task -> full_task i tree_at task) tasks;
    commit tree_at ()
  in

  for nb_domains = 1 to Domain.recommended_domain_count () do
    let elapsed = ref [] in
    for _ = 1 to config.nb_runs do
      let tree = get_tree () in
      let tree_at = Atomic.make tree in
      let tasks =
        Array.mapi (fun i task () -> full_task i tree_at task) tasks
      in
      let dt =
        Workers.run ~d_mgr ~nb:nb_domains ~finally:(commit tree_at) tasks
      in
      elapsed := dt :: !elapsed
    done;
    let min, median, max = analyze_bench @@ Array.of_list !elapsed in
    Format.printf "%i,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f@." nb_domains min median max
      (sequential /. max) (sequential /. median) (sequential /. min)
  done;
  S.Repo.close repo
