open Bench_common
open Irmin.Export_for_backends

type key = string list [@@deriving yojson]
type hash = string [@@deriving yojson]
type message = string [@@deriving yojson]

type op =
  | Add of key * string
  | Remove of key
  | Find of key * bool
  | Mem of key * bool
  | Mem_tree of key * bool
  | Commit of hash * int64 * message * hash list
  | Checkout of hash
  | Copy of key * key
[@@deriving yojson]

type config = {
  ncommits : int;
  ncommits_trace : int;
  depth : int;
  nchain_trees : int;
  width : int;
  nlarge_trees : int;
  root : string;
  flatten : bool;
  inode_config : [ `Entries_32 | `Entries_2 ];
  store_type : [ `Pack | `Pack_layered ];
  freeze_commit : int;
  commit_data_file : string;
  results_dir : string;
}

module type STORE = sig
  include Irmin.S with type key = string list and type contents = string

  type on_commit := int -> Hash.t -> unit Lwt.t
  type pp := Format.formatter -> unit

  val create_repo : int -> config -> (Repo.t * on_commit * pp) Lwt.t
end

let pp_inode_config ppf = function
  | `Entries_2 -> Format.fprintf ppf "[2, 5]"
  | `Entries_32 -> Format.fprintf ppf "[32, 256]"

let pp_store_type ppf = function
  | `Pack -> Format.fprintf ppf "[pack store]"
  | `Pack_layered -> Format.fprintf ppf "[pack-layered store]"

module Parse_trace = struct
  let is_hex_char = function
    | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
    | _ -> false

  let is_2char_hex s =
    if String.length s <> 2 then false
    else s |> String.to_seq |> List.of_seq |> List.for_all is_hex_char

  let is_30char_hex s =
    if String.length s <> 30 then false
    else s |> String.to_seq |> List.of_seq |> List.for_all is_hex_char

  let rec flatten_key_suffix = function
    | a :: b :: c :: d :: e :: f :: tl
      when is_2char_hex a
           && is_2char_hex b
           && is_2char_hex c
           && is_2char_hex d
           && is_2char_hex e
           && is_30char_hex f ->
        (a ^ b ^ c ^ d ^ e ^ f) :: flatten_key_suffix tl
    | hd :: tl -> hd :: flatten_key_suffix tl
    | [] -> []

  (** This function flattens all the 6 step-long chunks forming 38 byte-long
      hashes to a single step.

      The paths in tezos:
      https://www.dailambda.jp/blog/2020-05-11-plebeia/#tezos-path

      A chopped hash has this form:

      {v ([0-9a-f]{2}/){5}[0-9a-f]{30} v}

      and is flattened to that form:

      {v [0-9a-f]{40} v} *)
  let flatten_key = flatten_key_suffix

  let flatten_op = function
    | Add (key, v) -> Add (flatten_key key, v)
    | Remove keys -> Remove (flatten_key keys)
    | Find (keys, b) -> Find (flatten_key keys, b)
    | Mem (keys, b) -> Mem (flatten_key keys, b)
    | Mem_tree (keys, b) -> Mem_tree (flatten_key keys, b)
    | Checkout _ as op -> op
    | Copy (from, to_) -> Copy (flatten_key from, flatten_key to_)
    | Commit _ as op -> op

  let read_commits commits ncommits flatten path =
    let parse_op op =
      if flatten then op_of_yojson op |> Result.map flatten_op
      else op_of_yojson op
    in
    let json = Yojson.Safe.stream_from_file path in
    let rec aux index_op index_commit operations =
      if index_commit >= ncommits then index_commit
      else
        match Stream.next json with
        | exception Stream.Failure ->
            Fmt.epr
              "Only %d commits available in the trace file, proceeding...\n%!"
              index_commit;
            index_commit
        | op -> (
            match parse_op op with
            | Ok (Commit _ as x) ->
                commits.(index_commit) <- List.rev (x :: operations);
                (aux [@tailcall]) (index_op + 1) (index_commit + 1) []
            | Ok x ->
                (aux [@tailcall]) (index_op + 1) index_commit (x :: operations)
            | Error s -> Fmt.failwith "error op_of_yosjon %s\n%!" s)
    in
    aux 0 0 []

  let populate_array ncommits flatten path =
    let commits = Array.init ncommits (fun _ -> []) in
    let n = read_commits commits ncommits flatten path in
    (commits, n)
end

module Generate_trees_from_trace (Store : STORE) = struct
  type t = { mutable tree : Store.tree }

  type stat_entry =
    [ `Add | `Remove | `Find | `Mem | `Mem_tree | `Checkout | `Copy | `Commit ]
  [@@deriving repr]

  let op_tags =
    [ `Add; `Remove; `Find; `Mem; `Mem_tree; `Checkout; `Copy; `Commit ]

  (** One running histogram for each trace base operartions.

      The histograms are computed using https://github.com/barko/bentov.

      [Bentov] computes dynamic histograms without the need for a priori
      information on the distributions, while maintaining a constant memory
      space and a marginal CPU footprint.

      The implementation of that library is pretty straightforward, but not
      perfect; it doesn't scale well with the number of bins. I chose 32
      randomly.

      The computed histogram depends on the order of the operations, some
      marginal unsabilities are to be expected. *)
  let histo_per_op =
    op_tags
    |> List.map (fun which -> (which, Bentov.create 32))
    |> List.to_seq
    |> Hashtbl.of_seq

  (** Find and print the largest directory. This may be useful in the future to
      make sure that we are benching on million-sized directories. *)
  let _largest_directory { tree } =
    let rec aux : _ -> Store.step list * int = function
      | `Contents _ -> ([], 0)
      | `Tree l ->
          List.fold_left
            (fun acc (step, concrete) ->
              let steps, n = aux concrete in
              if n > snd acc then (step :: steps, n) else acc)
            ([], List.length l)
            l
    in
    let+ concrete = Store.Tree.to_concrete tree in
    aux concrete

  let pp_stats ppf (as_json, flatten, inode_config, store_type) =
    let mean histo =
      if Bentov.total_count histo > 0 then Bentov.mean histo else 0.
    in
    let total =
      Hashtbl.to_seq histo_per_op
      |> Seq.map snd
      |> Seq.map (fun histo ->
             mean histo *. float_of_int (Bentov.total_count histo))
      |> Seq.fold_left ( +. ) 0.
    in
    let total = if total = 0. then 1. else total in
    let pp_stat ppf which =
      let histo = Hashtbl.find histo_per_op which in
      let n = Bentov.total_count histo in
      let el = mean histo *. float_of_int n in
      if as_json then
        let pp_bar ppf (bin : Bentov.bin) =
          Format.fprintf ppf "[%2d,%.3e]" bin.count bin.center
        in
        Format.fprintf ppf "%a:[%a]" (Repr.pp stat_entry_t) which
          Fmt.(list ~sep:(any ",") pp_bar)
          (Bentov.bins histo)
      else
        Format.fprintf ppf "%d %a %.3f sec (%.1f%%)" n (Repr.pp stat_entry_t)
          which el
          (el /. total *. 100.)
    in
    if as_json then
      Fmt.pf ppf
        "{\"revision\":\"%s\", \"flatten\":%d, \"inode_config\":\"%a\", \
         \"store_type\":\"%a\", @\n\
         \"points\":{%a}}"
        "missing"
        (if flatten then 1 else 0)
        pp_inode_config inode_config pp_store_type store_type
        Fmt.(list ~sep:(any ",@\n") pp_stat)
        op_tags
    else Fmt.pf ppf "%a" Fmt.(list ~sep:(any "@\n") pp_stat) op_tags

  let with_monitoring which f =
    let histo0 = Hashtbl.find histo_per_op which in
    let t0 = Mtime_clock.counter () in
    let+ res = f () in
    let el1 = Mtime_clock.count t0 in
    let histo1 = Bentov.add (Mtime.Span.to_s el1) histo0 in
    Hashtbl.replace histo_per_op which histo1;
    res

  let error_find op k b n_op n_c =
    Fmt.failwith
      "Cannot reproduce operation %d of commit %d %s @[k = %a@] expected %b"
      n_op n_c op
      Fmt.(list ~sep:comma string)
      k b

  let exec_add t prev_commit i key v () =
    let+ tree = Store.Tree.add t.tree key v in
    t.tree <- tree;
    (i + 1, prev_commit)

  let exec_remove t prev_commit i keys () =
    let+ tree = Store.Tree.remove t.tree keys in
    t.tree <- tree;
    (i + 1, prev_commit)

  let exec_find t prev_commit n i keys b () =
    Store.Tree.find t.tree keys >|= function
    | None when not b -> (i + 1, prev_commit)
    | Some _ when b -> (i + 1, prev_commit)
    | _ -> error_find "find" keys b i n

  let exec_mem t prev_commit n i keys b () =
    let+ b' = Store.Tree.mem t.tree keys in
    if b <> b' then error_find "mem" keys b i n;
    (i + 1, prev_commit)

  let exec_mem_tree t prev_commit n i keys b () =
    let+ b' = Store.Tree.mem_tree t.tree keys in
    if b <> b' then error_find "mem_tree" keys b i n;
    (i + 1, prev_commit)

  let exec_checkout t repo prev_commit i () =
    Option.get prev_commit |> Store.Commit.of_hash repo >|= function
    | None -> Fmt.failwith "prev commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        t.tree <- tree;
        (i + 1, prev_commit)

  let exec_copy t prev_commit i from to_ () =
    Store.Tree.find_tree t.tree from >>= function
    | None -> Lwt.return (i + 1, prev_commit)
    | Some sub_tree ->
        let+ tree = Store.Tree.add_tree t.tree to_ sub_tree in
        t.tree <- tree;
        (i + 1, prev_commit)

  let exec_commit t repo prev_commit i date message () =
    (* in tezos commits call Tree.list first for the unshallow operation *)
    let* _ = Store.Tree.list t.tree [] in
    let info = Irmin.Info.v ~date ~author:"Tezos" message in
    let parents = match prev_commit with None -> [] | Some p -> [ p ] in
    let+ commit = Store.Commit.v repo ~info ~parents t.tree in
    Store.Tree.clear t.tree;
    (i + 1, Some (Store.Commit.hash commit))

  let add_operations t repo prev_commit operations n =
    Lwt_list.fold_left_s
      (fun (i, prev_commit) (operation : op) ->
        match operation with
        | Add (key, v) -> exec_add t prev_commit i key v |> with_monitoring `Add
        | Remove keys ->
            exec_remove t prev_commit i keys |> with_monitoring `Remove
        | Find (keys, b) ->
            exec_find t prev_commit n i keys b |> with_monitoring `Find
        | Mem (keys, b) ->
            exec_mem t prev_commit n i keys b |> with_monitoring `Mem
        | Mem_tree (keys, b) ->
            exec_mem_tree t prev_commit n i keys b |> with_monitoring `Mem_tree
        | Checkout _ ->
            exec_checkout t repo prev_commit i |> with_monitoring `Checkout
        | Copy (from, to_) ->
            exec_copy t prev_commit i from to_ |> with_monitoring `Copy
        | Commit (_, date, message, _) ->
            exec_commit t repo prev_commit i date message
            |> with_monitoring `Commit)
      (0, prev_commit) operations

  let add_commits repo commits on_commit () =
    let n = Array.length commits in
    with_progress_bar ~message:"Replaying trace" ~n ~unit:"commits"
      ~sampling_interval:50
    @@ fun prog ->
    let t = { tree = Store.Tree.empty } in
    let rec array_iter_lwt prev_commit i =
      if i >= n then Lwt.return_unit
      else
        let operations = commits.(i) in
        let* _, prev_commit = add_operations t repo prev_commit operations i in
        let* () = on_commit i (Option.get prev_commit) in
        prog Int64.one;
        array_iter_lwt prev_commit (i + 1)
    in
    array_iter_lwt None 0
end

module Benchmark = struct
  type result = { time : float; size : int }

  let run config f =
    let+ time, _ = with_timer f in
    let size = FSHelper.get_size config.root in
    { time; size }

  let pp_results ppf result =
    Format.fprintf ppf "Total time: %f@\nSize on disk: %d M" result.time
      result.size
end

module Hash = Irmin.Hash.SHA1

module Bench_suite (Store : STORE) = struct
  let init_commit repo =
    Store.Commit.v repo ~info:(info ()) ~parents:[] Store.Tree.empty

  module Trees = Generate_trees (Store)
  module Trees_trace = Generate_trees_from_trace (Store)

  let checkout_and_commit repo prev_commit f =
    Store.Commit.of_hash repo prev_commit >>= function
    | None -> Lwt.fail_with "commit not found"
    | Some commit ->
        let tree = Store.Commit.tree commit in
        let* tree = f tree in
        Store.Commit.v repo ~info:(info ()) ~parents:[ prev_commit ] tree

  let add_commits ~message repo ncommits on_commit f () =
    with_progress_bar ~message ~n:ncommits ~unit:"commits" ~sampling_interval:1
    @@ fun prog ->
    let* c = init_commit repo in
    let rec aux c i =
      if i >= ncommits then Lwt.return c
      else
        let* c' = checkout_and_commit repo (Store.Commit.hash c) f in
        let* () = on_commit i (Store.Commit.hash c') in
        prog Int64.one;
        aux c' (i + 1)
    in
    let+ _ = aux c 0 in
    ()

  let run_large config =
    reset_stats ();
    let* repo, on_commit, repo_pp = Store.create_repo config.ncommits config in
    let* result =
      Trees.add_large_trees config.width config.nlarge_trees
      |> add_commits ~message:"Playing large mode" repo config.ncommits
           on_commit
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Large trees mode on inode config %a, %a: %d commits, each consisting \
         of %d large trees of %d entries\n\
         %t@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nlarge_trees config.width repo_pp
        Benchmark.pp_results result

  let run_chains config =
    reset_stats ();
    let* repo, on_commit, repo_pp = Store.create_repo config.ncommits config in
    let* result =
      Trees.add_chain_trees config.depth config.nchain_trees
      |> add_commits ~message:"Playing chain mode" repo config.ncommits
           on_commit
      |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in
    fun ppf ->
      Format.fprintf ppf
        "Chain trees mode on inode config %a, %a: %d commits, each consisting \
         of %d chains of depth %d\n\
         %t@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        config.ncommits config.nchain_trees config.depth repo_pp
        Benchmark.pp_results result

  let run_read_trace config =
    reset_stats ();
    let commits, n =
      Parse_trace.populate_array config.ncommits_trace config.flatten
        config.commit_data_file
    in
    let config = { config with ncommits_trace = n } in
    let* repo, on_commit, repo_pp = Store.create_repo n config in
    let* result =
      Trees_trace.add_commits repo commits on_commit |> Benchmark.run config
    in
    let+ () = Store.Repo.close repo in

    prepare_results_dir config.results_dir;
    let json_path =
      let ( / ) = Filename.concat in
      config.results_dir / "boostrap_trace_timings.json"
    in
    let json_channel = open_out json_path in
    Format.fprintf
      (Format.formatter_of_out_channel json_channel)
      "%a%!" Trees_trace.pp_stats
      (true, config.flatten, config.inode_config, config.store_type);
    close_out json_channel;

    fun ppf ->
      Format.fprintf ppf
        "Tezos_log mode on inode config %a, %a. @\n\
         %t@\n\
         Results: @\n\
         %a@\n\
         Stats saved to %s@\n\
         %a"
        pp_inode_config config.inode_config pp_store_type config.store_type
        repo_pp Trees_trace.pp_stats
        (false, config.flatten, config.inode_config, config.store_type)
        json_path Benchmark.pp_results result
end

module Make_store_layered (Conf : sig
  val entries : int
  val stable_hash : int
end) =
struct
  module Store =
    Irmin_pack_layered.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Hash)

  let create_repo ncommits config =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true config.root in
    let* repo = Store.Repo.v conf in
    let on_commit i commit_hash =
      let* () =
        if i = config.freeze_commit then
          let* c = Store.Commit.of_hash repo commit_hash in
          let c = Option.get c in
          Store.freeze repo ~max:[ c ] ~min_upper:[ c ]
        else Lwt.return_unit
      in
      let* () =
        if i = ncommits - 1 then Store.PrivateLayer.wait_for_freeze repo
        else Lwt.return_unit
      in
      (* Something else than pause could be used here, like an Lwt_unix.sleep
         or nothing. See #1293 *)
      Lwt.pause ()
    in
    let pp ppf =
      if Irmin_layers.Stats.get_freeze_count () = 0 then
        Format.fprintf ppf "no freeze"
      else Format.fprintf ppf "%t" Irmin_layers.Stats.pp_latest
    in
    Lwt.return (repo, on_commit, pp)

  include Store
end

module Make_store_pack (Conf : sig
  val entries : int
  val stable_hash : int
end) =
struct
  module Store =
    Irmin_pack.Make (Conf) (Irmin.Metadata.None) (Irmin.Contents.String)
      (Irmin.Path.String_list)
      (Irmin.Branch.String)
      (Hash)

  let create_repo _ config =
    let conf = Irmin_pack.config ~readonly:false ~fresh:true config.root in
    let* repo = Store.Repo.v conf in
    let on_commit _ _ = Lwt.return_unit in
    let pp _ = () in
    Lwt.return (repo, on_commit, pp)

  include Store
end

module Conf2 = struct
  let entries = 2
  let stable_hash = 5
end

module Bench_inodes_32 = Bench_suite (Make_store_pack (Conf))
module Bench_inodes_2 = Bench_suite (Make_store_pack (Conf2))
module Bench_inodes_32_layered = Bench_suite (Make_store_layered (Conf))
module Bench_inodes_2_layered = Bench_suite (Make_store_layered (Conf2))

type suite_elt = {
  mode : [ `Read_trace | `Chains | `Large ];
  speed : [ `Quick | `Slow | `Custom ];
  run : config -> (Format.formatter -> unit) Lwt.t;
}

let suite : suite_elt list =
  [
    {
      mode = `Read_trace;
      speed = `Quick;
      run =
        (fun config ->
          Bench_inodes_32.run_read_trace
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Read_trace;
      speed = `Slow;
      run =
        (fun config ->
          Bench_inodes_32.run_read_trace
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Chains;
      speed = `Quick;
      run =
        (fun config ->
          Bench_inodes_32.run_chains
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Chains;
      speed = `Slow;
      run =
        (fun config ->
          Bench_inodes_2.run_chains
            { config with inode_config = `Entries_2; store_type = `Pack });
    };
    {
      mode = `Large;
      speed = `Quick;
      run =
        (fun config ->
          Bench_inodes_32.run_large
            { config with inode_config = `Entries_32; store_type = `Pack });
    };
    {
      mode = `Large;
      speed = `Slow;
      run =
        (fun config ->
          Bench_inodes_2.run_large
            { config with inode_config = `Entries_2; store_type = `Pack });
    };
    {
      mode = `Read_trace;
      speed = `Custom;
      run =
        (fun config ->
          match (config.inode_config, config.store_type) with
          | `Entries_2, `Pack -> Bench_inodes_2.run_read_trace config
          | `Entries_32, `Pack -> Bench_inodes_32.run_read_trace config
          | `Entries_2, `Pack_layered ->
              Bench_inodes_2_layered.run_read_trace config
          | `Entries_32, `Pack_layered ->
              Bench_inodes_32_layered.run_read_trace config);
    };
    {
      mode = `Chains;
      speed = `Custom;
      run =
        (fun config ->
          match (config.inode_config, config.store_type) with
          | `Entries_2, `Pack -> Bench_inodes_2.run_chains config
          | `Entries_32, `Pack -> Bench_inodes_32.run_chains config
          | `Entries_2, `Pack_layered ->
              Bench_inodes_2_layered.run_chains config
          | `Entries_32, `Pack_layered ->
              Bench_inodes_32_layered.run_chains config);
    };
    {
      mode = `Large;
      speed = `Custom;
      run =
        (fun config ->
          match (config.inode_config, config.store_type) with
          | `Entries_2, `Pack -> Bench_inodes_2.run_read_trace config
          | `Entries_32, `Pack -> Bench_inodes_32.run_read_trace config
          | `Entries_2, `Pack_layered ->
              Bench_inodes_2_layered.run_read_trace config
          | `Entries_32, `Pack_layered ->
              Bench_inodes_32_layered.run_read_trace config);
    };
  ]

let get_suite suite_filter =
  List.filter
    (fun { mode; speed; _ } ->
      match (suite_filter, speed, mode) with
      | `Slow, `Slow, `Read_trace ->
          (* The suite contains several `Read_trace benchmarks, let's keep the
             slow one only *)
          true
      | `Slow, _, `Read_trace -> false
      | `Slow, (`Slow | `Quick), _ -> true
      | `Quick, `Quick, _ -> true
      | `Custom_trace, `Custom, `Read_trace -> true
      | `Custom_chains, `Custom, `Chains -> true
      | `Custom_large, `Custom, `Large -> true
      | (`Slow | `Quick | `Custom_trace | `Custom_chains | `Custom_large), _, _
        ->
          false)
    suite

let main ncommits ncommits_trace suite_filter inode_config store_type
    freeze_commit flatten depth width nchain_trees nlarge_trees commit_data_file
    results_dir =
  let default = match suite_filter with `Quick -> 10000 | _ -> 13315 in
  let ncommits_trace = Option.value ~default ncommits_trace in
  let config =
    {
      ncommits;
      ncommits_trace;
      root = "test-bench";
      flatten;
      depth;
      width;
      nchain_trees;
      nlarge_trees;
      commit_data_file;
      inode_config;
      store_type;
      freeze_commit;
      results_dir;
    }
  in
  Printexc.record_backtrace true;
  Random.self_init ();
  FSHelper.rm_dir config.root;
  let suite = get_suite suite_filter in
  let run_benchmarks () = Lwt_list.map_s (fun b -> b.run config) suite in
  let results = Lwt_main.run (run_benchmarks ()) in
  Fmt.pr "%a@." Fmt.(list ~sep:(any "@\n@\n") (fun ppf f -> f ppf)) results

open Cmdliner

let mode =
  let mode =
    [
      ("slow", `Slow);
      ("quick", `Quick);
      ("trace", `Custom_trace);
      ("chains", `Custom_chains);
      ("large", `Custom_large);
    ]
  in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "mode" ] in
  Arg.(value @@ opt (Arg.enum mode) `Slow doc)

let inode_config =
  let mode = [ ("2", `Entries_2); ("32", `Entries_32) ] in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "inode-config" ] in
  Arg.(value @@ opt (Arg.enum mode) `Entries_32 doc)

let store_type =
  let mode = [ ("pack", `Pack); ("pack-layered", `Pack_layered) ] in
  let doc = Arg.info ~doc:(Arg.doc_alts_enum mode) [ "store-type" ] in
  Arg.(value @@ opt (Arg.enum mode) `Pack doc)

let freeze_commit =
  let doc =
    Arg.info
      ~doc:"Index of the commit after which to start the layered store freeze."
      [ "freeze-commit" ]
  in
  Arg.(value @@ opt int 1664 doc)

let flatten =
  let doc =
    Arg.info ~doc:"Flatten the paths in the trace benchmarks" [ "flatten" ]
  in
  Arg.(value @@ flag doc)

let ncommits =
  let doc =
    Arg.info ~doc:"Number of commits for the large and chain modes."
      [ "n"; "ncommits" ]
  in
  Arg.(value @@ opt int 2 doc)

let ncommits_trace =
  let doc =
    Arg.info ~doc:"Number of commits to read from trace." [ "ncommits_trace" ]
  in
  Arg.(value @@ opt (some int) None doc)

let depth =
  let doc =
    Arg.info ~doc:"Depth of a commit's tree in chains-mode." [ "d"; "depth" ]
  in
  Arg.(value @@ opt int 1000 doc)

let nchain_trees =
  let doc =
    Arg.info ~doc:"Number of chain trees per commit in chains-mode."
      [ "c"; "nchain" ]
  in
  Arg.(value @@ opt int 1 doc)

let width =
  let doc =
    Arg.info ~doc:"Width of a commit's tree in large-mode." [ "w"; "width" ]
  in
  Arg.(value @@ opt int 1000000 doc)

let nlarge_trees =
  let doc =
    Arg.info ~doc:"Number of large trees per commit in large-mode."
      [ "l"; "nlarge" ]
  in
  Arg.(value @@ opt int 1 doc)

let commit_data_file =
  let doc =
    Arg.info ~docv:"PATH"
      ~doc:
        "Path to the JSON-encoded commit data to use for the benchmark run. An \
         example of this data is available at\n\
         https://github.com/icristescu/dataset"
      []
  in
  Arg.(required @@ pos 0 (some string) None doc)

let results_dir =
  let doc =
    Arg.info ~docv:"PATH" ~doc:"Destination of the bench artefacts."
      [ "results" ]
  in
  Arg.(value @@ opt string default_results_dir doc)

let main_term =
  Term.(
    const main
    $ ncommits
    $ ncommits_trace
    $ mode
    $ inode_config
    $ store_type
    $ freeze_commit
    $ flatten
    $ depth
    $ width
    $ nchain_trees
    $ nlarge_trees
    $ commit_data_file
    $ results_dir)

let () =
  let info = Term.info "Benchmarks for tree operations" in
  Term.exit @@ Term.eval (main_term, info)
