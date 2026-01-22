(** Inline contents benchmark *)

module S = Irmin_tezos.Store

type config = {
  n_contents : int;  (* Number of contents to create *)
  distribution : Distribution.t;
  inline_contents : bool;
  inline_threshold : int;  (* Max serialized bytes for inlining *)
  n_reads : int;  (* Number of read operations for latency measurement *)
  runs : int;  (* Number of benchmark runs *)
}

type metrics = {
  store_size_bytes : int;
  write_time_ms : float;
  read_latencies_us : float array;  (* Individual read latencies in microseconds *)
  inlined_count : int;
  non_inlined_count : int;
  content_sizes : int array;  (* Actual sizes generated *)
}

let default_config = {
  n_contents = 10_000;
  distribution = Distribution.around_threshold;
  inline_contents = false;
  inline_threshold = 48;
  n_reads = 1_000;
  runs = 5;
}

let root fs = Eio.Path.(fs / "_build" / "bench-inline")

let reset_env ~fs () =
  Eio.Path.rmtree ~missing_ok:true (root fs)

let info () = S.Info.empty

let open_repo ~sw ~fs ~fresh ~readonly ~inline_contents () =
  let conf = Irmin_pack.Conf.init ~sw ~fs ~fresh ~readonly ~inline_contents (root fs) in
  S.Repo.v conf

(* Generate random content of given size *)
let make_content size =
  Bytes.init size (fun _ -> Char.chr (Char.code 'a' + Random.int 26))

(* Generate paths and contents based on distribution *)
let generate ~config =
  let sizes = Distribution.sample ~n:config.n_contents config.distribution in
  let contents = Array.mapi (fun i size ->
    let path = [ Printf.sprintf "content_%06d" i ] in
    let value = make_content size in
    (path, value, size)
  ) sizes in
  contents

(* Measure directory size recursively *)
let rec dir_size path =
  if Sys.is_directory path then
    let entries = Sys.readdir path in
    Array.fold_left (fun acc entry ->
      acc + dir_size (Filename.concat path entry)
    ) 0 entries
  else
    (Unix.stat path).Unix.st_size

(* Count inlined vs non-inlined contents by examining the backend *)
let count_inlined repo =
  (* Access the node store to check structure *)
  let inlined = ref 0 in
  let non_inlined = ref 0 in
  let main = S.main repo in
  let head = S.Head.get main in
  let tree = S.Commit.tree head in
  let root_node = match S.Tree.destruct tree with
    | `Node (n, _) -> S.to_backend_node n
    | `Contents _ -> failwith "Expected root to be a node"
  in
  let entries = S.Backend.Node.Val.list root_node in
  List.iter (fun (_step, value) ->
    match value with
    | `Contents_inlined _ -> incr inlined
    | `Contents _ -> incr non_inlined
    | `Node _ -> ()
  ) entries;
  (!inlined, !non_inlined)

(* Run a single benchmark iteration *)
let run_one ~sw ~fs ~config =
  reset_env ~fs ();
  let contents = generate ~config in
  let content_sizes = Array.map (fun (_, _, size) -> size) contents in

  (* Set the inlining threshold before opening repo *)
  Irmin.Tree.set_inline_contents_max_bytes config.inline_threshold;

  (* Write phase *)
  let repo = open_repo ~sw ~fs ~fresh:true ~readonly:false
      ~inline_contents:config.inline_contents () in
  let main = S.main repo in

  let t0 = Unix.gettimeofday () in
  let tree = Array.fold_left (fun tree (path, value, _size) ->
    S.Tree.add tree path value
  ) (S.Tree.empty ()) contents in
  S.set_tree_exn ~info main [] tree;
  let t1 = Unix.gettimeofday () in
  let write_time_ms = 1000.0 *. (t1 -. t0) in

  (* Count inlined *)
  let inlined_count, non_inlined_count = count_inlined repo in

  (* Measure store size *)
  S.Repo.close repo;
  let store_path = Eio.Path.native_exn (root fs) in
  let store_size_bytes = dir_size store_path in

  (* Read phase - reopen in readonly mode *)
  let repo = open_repo ~sw ~fs ~fresh:false ~readonly:true
      ~inline_contents:config.inline_contents () in
  let main = S.main repo in
  let head = S.Head.get main in
  let tree = S.Commit.tree head in

  (* Sample random reads and measure latency *)
  let n_contents = Array.length contents in
  let read_latencies_us = Array.init config.n_reads (fun _ ->
    let idx = Random.int n_contents in
    let path, expected_value, _ = contents.(idx) in
    let t0 = Unix.gettimeofday () in
    let value = S.Tree.find tree path in
    let t1 = Unix.gettimeofday () in
    (match value with
     | Some v when Bytes.equal v expected_value -> ()
     | Some v ->
         Printf.eprintf "Warning: value mismatch at %s: expected %d bytes, got %d bytes\n"
           (String.concat "/" path) (Bytes.length expected_value) (Bytes.length v)
     | None ->
         Printf.eprintf "Warning: missing value at %s\n" (String.concat "/" path));
    1_000_000.0 *. (t1 -. t0)
  ) in

  S.Repo.close repo;

  { store_size_bytes; write_time_ms; read_latencies_us;
    inlined_count; non_inlined_count; content_sizes }

(* Compute latency percentiles *)
let latency_percentiles latencies =
  let sorted = Array.copy latencies in
  Array.sort Float.compare sorted;
  let n = Array.length sorted in
  let p idx = sorted.(Int.min (n - 1) idx) in
  (p (n / 2), p (n * 99 / 100), p (n * 999 / 1000))

(* Run benchmark with multiple iterations *)
let run ~fs ~config =
  Eio.Switch.run @@ fun sw ->

  (* Warm-up run *)
  let _ = run_one ~sw ~fs ~config in

  (* Actual runs *)
  let results = Array.init config.runs (fun _ -> run_one ~sw ~fs ~config) in

  (* Aggregate results - use median for timing metrics *)
  let write_times = Array.map (fun r -> r.write_time_ms) results in
  Array.sort Float.compare write_times;
  let median_write_time = write_times.(config.runs / 2) in

  let store_sizes = Array.map (fun r -> r.store_size_bytes) results in
  Array.sort Int.compare store_sizes;
  let median_store_size = store_sizes.(config.runs / 2) in

  (* Combine all read latencies from all runs *)
  let all_latencies = Array.concat (Array.to_list
    (Array.map (fun r -> r.read_latencies_us) results)) in
  let p50, p99, p999 = latency_percentiles all_latencies in

  (* Use first result for counts (should be same across runs) *)
  let r0 = results.(0) in

  (median_store_size, median_write_time, p50, p99, p999,
   r0.inlined_count, r0.non_inlined_count, r0.content_sizes)

(* Print results as CSV row *)
let print_csv_header () =
  Printf.printf "distribution,threshold,n_contents,store_bytes,write_ms,read_p50_us,read_p99_us,read_p999_us,inlined,non_inlined,size_stats\n"

let print_csv_row ~dist_name ~config (store_size, write_time, p50, p99, p999, inlined, non_inlined, sizes) =
  let threshold_str = if config.inline_contents then string_of_int config.inline_threshold else "off" in
  Printf.printf "%s,%s,%d,%d,%.2f,%.2f,%.2f,%.2f,%d,%d,\"%s\"\n"
    dist_name
    threshold_str
    config.n_contents
    store_size
    write_time
    p50 p99 p999
    inlined non_inlined
    (Distribution.describe sizes)
