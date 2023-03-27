(*
 * Copyright (c) 2018-2022 Tarides <contact@tarides.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open! Import
include Checks_intf

let setup_log =
  let init style_renderer level =
    let format_reporter =
      let report _src level ~over k msgf =
        let k _ =
          over ();
          k ()
        in
        msgf @@ fun ?header:_ ?tags:_ fmt ->
        match level with
        | Logs.App ->
            Fmt.kpf k Fmt.stderr
              ("@[<v 0>%a" ^^ fmt ^^ "@]@.")
              Fmt.(styled `Bold (styled (`Fg `Cyan) string))
              ">> "
        | _ -> Fmt.kpf k Fmt.stdout ("@[<v 0>" ^^ fmt ^^ "@]@.")
      in
      { Logs.report }
    in
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter format_reporter
  in
  Cmdliner.Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())

let path =
  let open Cmdliner.Arg in
  required
  @@ pos 0 (some string) None
  @@ info ~doc:"Path to the Irmin store on disk" ~docv:"PATH" []

let deprecated_info = (Cmdliner.Term.info [@alert "-deprecated"])

let ppf_or_null ppf =
  let null =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> "/dev/null"
    | "Win32" -> "NUL"
    | _ -> invalid_arg "invalid os type"
  in
  match ppf with
  | Some p -> p
  | None -> open_out null |> Format.formatter_of_out_channel

module Make (Store : Store) = struct
  module Hash = Store.Hash
  module Index = Pack_index.Make (Hash)

  (** Read basic metrics from an existing store. *)
  module Stat = struct
    type size = Bytes of int [@@deriving irmin]

    type io = { size : size; offset : int63; version : Version.t }
    [@@deriving irmin]

    type objects = { nb_commits : int; nb_nodes : int; nb_contents : int }
    [@@deriving irmin]

    type t = { hash_size : size; log_size : int; objects : objects }
    [@@deriving irmin]

    let traverse_index ~root log_size =
      let index = Index.v_exn ~readonly:true ~fresh:false ~log_size root in
      let ppf = Format.err_formatter in
      let bar, (progress_contents, progress_nodes, progress_commits) =
        Utils.Object_counter.start ppf
      in
      let f _ (_, _, (kind : Pack_value.Kind.t)) =
        match kind with
        | Contents -> progress_contents ()
        | Inode_v1_stable | Inode_v1_unstable | Inode_v2_root | Inode_v2_nonroot
          ->
            progress_nodes ()
        | Commit_v1 | Commit_v2 -> progress_commits ()
        | Dangling_parent_commit -> assert false
      in
      Index.iter f index;
      let nb_contents, nb_nodes, nb_commits =
        Utils.Object_counter.finalise_with_stats bar
      in
      { nb_contents; nb_nodes; nb_commits }

    let conf root = Conf.init ~readonly:true ~fresh:false ~no_migrate:true root

    let run ~root =
      [%logs.app "Getting statistics for store: `%s'@," root];
      let log_size = conf root |> Conf.index_log_size in
      let objects = traverse_index ~root log_size in
      { hash_size = Bytes Hash.hash_size; log_size; objects }
      |> Irmin.Type.pp_json ~minify:false t Fmt.stdout;
      Lwt.return_unit

    let term_internal =
      Cmdliner.Term.(const (fun root () -> Lwt_main.run (run ~root)) $ path)

    let term =
      let doc = "Print high-level statistics about the store." in
      Cmdliner.Term.(term_internal $ setup_log, deprecated_info ~doc "stat")
  end

  module Reconstruct_index = struct
    let conf ~index_log_size root =
      Conf.init ~readonly:false ~fresh:false ?index_log_size ~no_migrate:true
        root

    let dest =
      let open Cmdliner.Arg in
      value
      & pos 1 (some string) None
        @@ info ~doc:"Path to the new index file" ~docv:"DEST" []

    let index_log_size =
      let open Cmdliner.Arg in
      value
      & opt (some int) None
        @@ info ~doc:"Size of the index log file" [ "index-log-size" ]

    let run ~root ~output ?index_log_size () =
      let conf = conf ~index_log_size root in
      match output with
      | None -> Store.traverse_pack_file (`Reconstruct_index `In_place) conf
      | Some p -> Store.traverse_pack_file (`Reconstruct_index (`Output p)) conf

    let term_internal =
      Cmdliner.Term.(
        const (fun root output index_log_size () ->
            run ~root ~output ?index_log_size ())
        $ path
        $ dest
        $ index_log_size)

    let term =
      let doc = "Reconstruct index from an existing pack file." in
      Cmdliner.Term.
        (term_internal $ setup_log, deprecated_info ~doc "reconstruct-index")
  end

  module Integrity_check_index = struct
    let conf root = Conf.init ~readonly:true ~fresh:false ~no_migrate:true root

    let run ~root ~auto_repair () =
      let conf = conf root in
      if auto_repair then Store.traverse_pack_file `Check_and_fix_index conf
      else Store.traverse_pack_file `Check_index conf

    let auto_repair =
      let open Cmdliner.Arg in
      value
      & (flag @@ info ~doc:"Add missing entries in index" [ "auto-repair" ])

    let term_internal =
      Cmdliner.Term.(
        const (fun root auto_repair () -> run ~root ~auto_repair ())
        $ path
        $ auto_repair)

    let term =
      let doc = "Check index integrity." in
      Cmdliner.Term.
        (term_internal $ setup_log, deprecated_info ~doc "integrity-check-index")
  end

  module Integrity_check = struct
    let conf root always =
      let indexing_strategy =
        if always then Irmin_pack.Indexing_strategy.always
        else Irmin_pack.Indexing_strategy.minimal
      in
      Conf.init ~readonly:false ~fresh:false ~no_migrate:true ~indexing_strategy
        root

    let handle_result ?ppf ?name res =
      let ppf = ppf_or_null ppf in
      let name = match name with Some x -> x ^ ": " | None -> "" in
      match res with
      | Ok (`Fixed n) -> Fmt.pf ppf "%sOk -- fixed %d\n%!" name n
      | Ok `No_error -> Fmt.pf ppf "%sOk\n%!" name
      | Error (`Cannot_fix x) ->
          Printf.eprintf "%sError -- cannot fix: %s\n%!" name x
      | Error (`Corrupted x) ->
          Printf.eprintf "%sError -- corrupted: %d\n%!" name x

    let run ?ppf ~root ~auto_repair ~always ~heads () =
      let conf = conf root always in
      let* repo = Store.Repo.v conf in
      let* heads =
        match heads with
        | None -> Store.Repo.heads repo
        | Some heads ->
            Lwt_list.filter_map_s
              (fun x ->
                match Repr.of_string Store.Hash.t x with
                | Ok x -> Store.Commit.of_hash repo x
                | Error (`Msg m) -> Fmt.kstr Lwt.fail_with "Invalid hash %S" m)
              heads
      in
      let* result = Store.integrity_check ?ppf ~auto_repair ~heads repo in
      let+ () = Store.Repo.close repo in
      handle_result ?ppf ?name:None result

    let heads =
      let open Cmdliner.Arg in
      value
      & opt (some (list ~sep:',' string)) None
      & info [ "heads" ] ~doc:"List of head commit hashes" ~docv:"HEADS"

    let auto_repair =
      let open Cmdliner.Arg in
      value & (flag @@ info ~doc:"Automatically repair issues" [ "auto-repair" ])

    let always =
      let open Cmdliner.Arg in
      value & (flag @@ info ~doc:"Use always indexing strategy" [ "always" ])

    let term_internal =
      Cmdliner.Term.(
        const (fun root auto_repair always heads () ->
            Lwt_main.run
              (run ~ppf:Format.err_formatter ~root ~auto_repair ~always ~heads
                 ()))
        $ path
        $ auto_repair
        $ always
        $ heads)

    let term =
      let doc = "Check integrity of an existing store." in
      Cmdliner.Term.
        (term_internal $ setup_log, deprecated_info ~doc "integrity-check")
  end

  module Integrity_check_inodes = struct
    let conf root = Conf.init ~readonly:true ~fresh:false ~no_migrate:true root

    let heads =
      let open Cmdliner.Arg in
      value
      & opt (some (list ~sep:',' string)) None
      & info [ "heads" ] ~doc:"List of head commit hashes" ~docv:"HEADS"

    let run ~root ~heads =
      let conf = conf root in
      let* repo = Store.Repo.v conf in
      let* heads =
        match heads with
        | None -> Store.Repo.heads repo
        | Some heads ->
            Lwt_list.filter_map_s
              (fun x ->
                match Repr.of_string Store.Hash.t x with
                | Ok x -> Store.Commit.of_hash repo x
                | Error (`Msg m) -> Fmt.kstr Lwt.fail_with "Invalid hash %S" m)
              heads
      in
      let* () =
        Store.integrity_check_inodes ~heads repo >|= function
        | Ok `No_error -> [%logs.app "Ok"]
        | Error (`Cannot_fix msg) -> Fmt.failwith "Error: %s" msg
      in
      Store.Repo.close repo

    let term_internal =
      Cmdliner.Term.(
        const (fun root heads () -> Lwt_main.run (run ~root ~heads))
        $ path
        $ heads)

    let term =
      let doc = "Check integrity of inodes in an existing store." in
      Cmdliner.Term.
        ( term_internal $ setup_log,
          deprecated_info ~doc "integrity-check-inodes" )
  end

  module Stats_commit = struct
    let conf root = Conf.init ~readonly:true ~fresh:false ~no_migrate:true root

    let commit =
      let open Cmdliner.Arg in
      value
      & opt (some string) None
      & info [ "commit" ] ~doc:"The commit whose underlying tree is traversed."
          ~docv:"COMMIT"

    let dump_blob_paths_to =
      let open Cmdliner.Arg in
      value
      & opt (some string) None
      & info [ "dump_blob_paths_to" ]
          ~doc:"Print all paths to a blob in the tree in a file."

    let run ~root ~commit ~dump_blob_paths_to () =
      let conf = conf root in
      let* repo = Store.Repo.v conf in
      let* commit =
        match commit with
        | None -> (
            let* heads = Store.Repo.heads repo in
            match heads with
            | [] -> Lwt.fail_with "No heads found"
            | [ head ] -> Lwt.return head
            | ls ->
                Fmt.kstr Lwt.fail_with
                  "Several heads found, please specify one. Heads = %a"
                  Fmt.(list ~sep:comma Store.Commit.pp_hash)
                  ls)
        | Some hash -> (
            match Repr.of_string Store.Hash.t hash with
            | Ok x -> (
                Store.Commit.of_hash repo x >>= function
                | None ->
                    Fmt.kstr Lwt.fail_with "Commit with hash %s not found" hash
                | Some x -> Lwt.return x)
            | Error (`Msg m) -> Fmt.kstr Lwt.fail_with "Invalid hash %S" m)
      in
      let* () = Store.stats ~dump_blob_paths_to ~commit repo in
      Store.Repo.close repo

    let term_internal =
      Cmdliner.Term.(
        const (fun root commit dump_blob_paths_to () ->
            Lwt_main.run (run ~root ~commit ~dump_blob_paths_to ()))
        $ path
        $ commit
        $ dump_blob_paths_to)

    let term =
      let doc =
        "Traverse one commit, specified with the --commit argument, in the \
         store for stats. If no commit is specified the current head is used."
      in
      Cmdliner.Term.
        (term_internal $ setup_log, deprecated_info ~doc "stat-store")
  end

  module Cli = struct
    open Cmdliner

    let main
        ?(terms =
          [
            Stat.term;
            Reconstruct_index.term;
            Integrity_check.term;
            Integrity_check_inodes.term;
            Integrity_check_index.term;
            Stats_commit.term;
          ]) () : empty =
      let default =
        let default_info =
          let doc = "Check Irmin data-stores." in
          deprecated_info ~doc "irmin-fsck"
        in
        Term.(ret (const (`Help (`Auto, None))), default_info)
      in
      let deprecated_eval_choice = (Term.eval_choice [@alert "-deprecated"]) in
      let deprecated_exit = (Term.exit [@alert "-deprecated"]) in
      deprecated_eval_choice default terms |> deprecated_exit;
      assert false
  end

  let cli = Cli.main
end

module Integrity_checks
    (XKey : Pack_key.S)
    (X : Irmin.Backend.S
           with type Commit.key = XKey.t
            and type Node.key = XKey.t
            and type Schema.Hash.t = XKey.hash)
    (Index : Pack_index.S) =
struct
  let check_always ?ppf ~auto_repair ~check index =
    let ppf = ppf_or_null ppf in
    Fmt.pf ppf "Running the integrity_check.\n%!";
    let nb_absent = ref 0 in
    let nb_corrupted = ref 0 in
    let exception Cannot_fix in
    let counter, (progress_contents, progress_nodes, progress_commits) =
      Utils.Object_counter.start ppf
    in
    let f (k, (offset, length, (kind : Pack_value.Kind.t))) =
      match kind with
      | Contents ->
          progress_contents ();
          check ~kind:`Contents ~offset ~length k
      | Inode_v1_stable | Inode_v1_unstable | Inode_v2_root | Inode_v2_nonroot
        ->
          progress_nodes ();
          check ~kind:`Node ~offset ~length k
      | Commit_v1 | Commit_v2 ->
          progress_commits ();
          check ~kind:`Commit ~offset ~length k
      | Dangling_parent_commit -> assert false
    in
    let result =
      if auto_repair then
        try
          Index.filter index (fun binding ->
              match f binding with
              | Ok () -> true
              | Error `Wrong_hash -> raise Cannot_fix
              | Error `Absent_value ->
                  incr nb_absent;
                  false);
          if !nb_absent = 0 then Ok `No_error else Ok (`Fixed !nb_absent)
        with Cannot_fix -> Error (`Cannot_fix "Not implemented")
      else (
        Index.iter
          (fun k v ->
            match f (k, v) with
            | Ok () -> ()
            | Error `Wrong_hash -> incr nb_corrupted
            | Error `Absent_value -> incr nb_absent)
          index;
        if !nb_absent = 0 && !nb_corrupted = 0 then Ok `No_error
        else Error (`Corrupted (!nb_corrupted + !nb_absent)))
    in
    Utils.Object_counter.finalise counter;
    result

  let check_minimal ?ppf ~pred ~iter ~check ~recompute_hash t =
    let ppf = ppf_or_null ppf in
    Fmt.pf ppf "Running the integrity_check.\n%!";
    let errors = ref [] in
    let counter, (progress_contents, progress_nodes, progress_commits) =
      Utils.Object_counter.start ppf
    in
    let pp_hash = Irmin.Type.pp X.Hash.t in
    let equal_hash = Irmin.Type.(unstage (equal X.Hash.t)) in
    let add_error err hash =
      let msg =
        match err with
        | `Wrong_hash -> Fmt.str "Wrong_hash %a" pp_hash hash
        | `Absent_value -> Fmt.str "Absent_value for hash %a" pp_hash hash
      in
      errors := msg :: !errors
    in
    let check_contents key =
      match Pack_key.inspect key with
      | Indexed _hash ->
          (* TODO: The goal here is to check a "one commit" store, generated
             by a gc, in which indexed keys cannot occur. We might want to
             extends this to stores that have both indexed and direct keys. *)
          Lwt.fail_with
            "Not supported for stores which have entries obtained with irmin < \
             3.0. If all entries were added with irmin < 3.0, please use \
             [integrity_check] instead."
      | Direct { offset; length; hash; _ } -> (
          let result = check ~offset ~length hash in
          match result with
          | Ok () -> Lwt.return_unit
          | Error err ->
              add_error err hash;
              Lwt.return_unit)
    in
    (* Commits are read from disk and checked by the [find] function in [pred].
       We need to explicitly check the contents and the nodes. *)
    let contents key =
      progress_contents ();
      check_contents key
    in
    let pred_node repo key =
      try
        X.Node.find (X.Repo.node_t repo) key >|= function
        | None ->
            Fmt.failwith "node with hash %a not found" pp_hash
              (XKey.to_hash key)
        | Some v ->
            let preds = pred v in
            List.rev_map
              (function
                | s, `Inode x ->
                    assert (s = None);
                    `Node x
                | _, `Node x -> `Node x
                | _, `Contents x -> `Contents x)
              preds
      with _exn ->
        add_error `Wrong_hash (XKey.to_hash key);
        Lwt.return []
    in
    let check_nodes key =
      X.Node.find (X.Repo.node_t t) key >|= function
      | None ->
          Fmt.failwith "node with hash %a not found" pp_hash (XKey.to_hash key)
      | Some v ->
          let h = XKey.to_hash key in
          let h' = recompute_hash v in
          if not (equal_hash h h') then add_error `Wrong_hash h
    in
    let node key =
      progress_nodes ();
      check_nodes key
    in
    (* Only visit the nodes of the commits and not the parents of the commit. *)
    let pred_commit repo k =
      try
        progress_commits ();
        X.Commit.find (X.Repo.commit_t repo) k >|= function
        | None -> []
        | Some c ->
            let node = X.Commit.Val.node c in
            [ `Node node ]
      with _exn ->
        add_error `Wrong_hash (XKey.to_hash k);
        Lwt.return []
    in

    let+ () = iter ~contents ~node ~pred_node ~pred_commit t in
    Utils.Object_counter.finalise counter;
    if !errors = [] then Ok `No_error
    else
      Fmt.kstr
        (fun x -> Error (`Cannot_fix x))
        "Inconsistencies found: %a"
        Fmt.(list ~sep:comma string)
        !errors

  let check_inodes ?ppf ~iter ~pred ~check t =
    let ppf = ppf_or_null ppf in
    Fmt.pf ppf "Check integrity for inodes.\n%!";
    let counter, (_, progress_nodes, progress_commits) =
      Utils.Object_counter.start ppf
    in
    let errors = ref [] in
    let pred_node repo key =
      Lwt.catch
        (fun () -> pred repo key)
        (fun _ ->
          errors := "Error in repo iter" :: !errors;
          Lwt.return [])
    in
    let node k =
      progress_nodes ();
      check k >|= function Ok () -> () | Error msg -> errors := msg :: !errors
    in
    let commit _ =
      progress_commits ();
      Lwt.return_unit
    in
    let+ () = iter ~pred_node ~node ~commit t in
    Utils.Object_counter.finalise counter;
    if !errors = [] then Ok `No_error
    else
      Fmt.kstr
        (fun x -> Error (`Cannot_fix x))
        "Inconsistent inodes found %a"
        Fmt.(list ~sep:comma string)
        !errors
end

module Stats (S : sig
  type step

  val step_t : step Irmin.Type.t

  module Hash : Irmin.Hash.S
end) =
struct
  type step = Node of S.step | Inode
  type path = step list

  module Metrics : sig
    type max
    type node

    val max_length : node -> int
    val all_paths : node -> path list
    val mp : node -> max
    val maximum : max -> int
    val maximal_count : max -> int
    val representative : max -> path

    val v :
      ?maximal_count:int -> maximum:int -> representative:path -> unit -> max

    val empty_root_node : node
    val empty_node : node
    val empty_max : max
    val update_node : node -> node -> step -> int -> node
    val update_width : node -> int -> max -> max
    val pp : max Fmt.t
    val pp_all_paths : node Fmt.t
  end = struct
    type max = { maximum : int; maximal_count : int; representative : path }

    type node = {
      all_paths : path list;
      (* All paths to a node. *)
      max_length : int;
      (* The max length of a path to a node. *)
      mp : max;
          (* The maximum size of a membership proof: the number of siblings at
             every level along the path. *)
    }

    let max_length { max_length; _ } = max_length
    let all_paths { all_paths; _ } = all_paths
    let mp { mp; _ } = mp
    let maximum { maximum; _ } = maximum
    let representative { representative; _ } = representative
    let maximal_count { maximal_count; _ } = maximal_count

    let v ?(maximal_count = 1) ~maximum ~representative () =
      { maximum; maximal_count; representative }

    let empty_max = { maximum = 0; maximal_count = 0; representative = [] }

    let empty_root_node =
      let mp = empty_max in
      { all_paths = [ [] ]; max_length = 0; mp }

    let empty_node =
      let mp = empty_max in
      { all_paths = []; max_length = 0; mp }

    let incr ({ maximal_count; _ } as t) =
      { t with maximal_count = maximal_count + 1 }

    let update_mp stat_k stat_pred step nb_siblings =
      let mp = stat_k.maximum + nb_siblings in
      if stat_pred.maximum > mp then stat_pred
      else if stat_pred.maximum = mp && not (mp = 0) then incr stat_pred
      else
        let path_to_k = stat_k.representative in
        let new_path_to_pred = step :: path_to_k in
        v ~maximum:mp ~representative:new_path_to_pred ()

    let update_width stat_k width_k max_width =
      if max_width.maximum > width_k then max_width
      else if max_width.maximum = width_k then incr max_width
      else
        let representative = List.hd stat_k.all_paths in
        v ~maximum:width_k ~representative ()

    let update_path paths_to_k step_k_to_n paths_to_n =
      let new_paths_to_n =
        List.rev_map (fun rev_path -> step_k_to_n :: rev_path) paths_to_k
      in
      List.rev_append new_paths_to_n paths_to_n

    let update_node stat_k stat_pred step_k_to_pred nb_siblings =
      let all_paths, max_length =
        match step_k_to_pred with
        | Inode ->
            (* Do not update if pred is an inode. *)
            (stat_k.all_paths, stat_k.max_length)
        | Node _ ->
            let paths_to_pred =
              update_path stat_k.all_paths step_k_to_pred stat_pred.all_paths
            in
            let length =
              (* The new current length to pred. *)
              let lk = stat_k.max_length + 1 in
              (* The previous max length to pred. *)
              let ln = stat_pred.max_length in
              max lk ln
            in
            (paths_to_pred, length)
      in
      let mp = update_mp stat_k.mp stat_pred.mp step_k_to_pred nb_siblings in
      let stat_pred' = { all_paths; max_length; mp } in
      stat_pred'

    let pp_step ppf = function
      | Inode -> Fmt.pf ppf "-"
      | Node x -> Fmt.pf ppf "%a" (Irmin.Type.pp S.step_t) x

    let pp_path = Fmt.list ~sep:(Fmt.any "/") pp_step

    let pp_all_paths fmt stats =
      List.iter
        (fun l -> Fmt.pf fmt "%a\n" pp_path (List.rev l))
        stats.all_paths

    let pp =
      let open Fmt.Dump in
      record
        [
          field "maximum" (fun t -> t.maximum) Fmt.int;
          field "maximal_count" (fun t -> t.maximal_count) Fmt.int;
          field "representative" (fun t -> List.rev t.representative) pp_path;
        ]
  end

  type t = {
    visited : (S.Hash.t, Metrics.node) Hashtbl.t;
    mutable max_width : Metrics.max;
    mutable max_mp : int;
    mutable max_length : int;
  }

  let v () =
    let visited = Hashtbl.create 100 in
    let max_width = Metrics.empty_max in
    { visited; max_width; max_length = 0; max_mp = 0 }

  let get t k =
    try Hashtbl.find t.visited k with Not_found -> Metrics.empty_node

  let visit_node t k preds ~nb_children ~width =
    let preds =
      List.map
        (function None, x -> (Inode, x) | Some s, x -> (Node s, x))
        preds
    in
    let stat_k = get t k in
    let visit step pred =
      let stat_pred = get t pred in
      let nb_siblings = nb_children - 1 in
      let stat_pred' = Metrics.update_node stat_k stat_pred step nb_siblings in
      Hashtbl.replace t.visited pred stat_pred'
    in
    let () =
      List.iter
        (function
          | Inode, `Inode x -> visit Inode x
          | Node s, `Node x -> visit (Node s) x
          | Node s, `Contents x -> visit (Node s) x
          | _ -> assert false)
        preds
    in
    (* Once we updated its preds we can remove the node from the
       table. If its a max width, we update the max_width stats. *)
    Hashtbl.remove t.visited k;
    t.max_width <- Metrics.update_width stat_k width t.max_width

  let visit_commit t root_node =
    let stat = Metrics.empty_root_node in
    Hashtbl.replace t.visited root_node stat

  (* Update the max length and max_mp while traversing the contents. *)
  let visit_contents t k =
    let stat = get t k in
    let max_length = Metrics.max_length stat in
    if max_length > t.max_length then t.max_length <- max_length;
    let maximum = Metrics.mp stat |> Metrics.maximum in
    if maximum > t.max_mp then t.max_mp <- maximum

  let pp_results ~dump_blob_paths_to t =
    [%log.app "Max width = %a" Metrics.pp t.max_width];
    let maximal_count, representative =
      Hashtbl.fold
        (fun _ (stat : Metrics.node) ((counter, _) as acc) ->
          let maximum = Metrics.mp stat |> Metrics.maximum in
          if maximum = t.max_mp then
            let maximal_count = Metrics.mp stat |> Metrics.maximal_count in
            let counter' = counter + maximal_count in
            let repr = Metrics.mp stat |> Metrics.representative in
            (counter', repr)
          else acc)
        t.visited (0, [])
    in
    let max_mp =
      Metrics.v ~maximal_count ~representative ~maximum:t.max_mp ()
    in
    [%log.app "Max number of path-adjacent nodes = %a" Metrics.pp max_mp];
    (* Count all paths that have max length. *)
    let maximal_count, representative =
      Hashtbl.fold
        (fun _ (stat : Metrics.node) acc ->
          if Metrics.max_length stat = t.max_length then
            List.fold_left
              (fun ((counter, _) as acc) l ->
                if List.length l = t.max_length then (counter + 1, l) else acc)
              acc (Metrics.all_paths stat)
          else acc)
        t.visited (0, [])
    in
    let max_length =
      Metrics.v ~maximal_count ~representative ~maximum:t.max_length ()
    in
    [%log.app "Max length = %a" Metrics.pp max_length];
    match dump_blob_paths_to with
    | None -> ()
    | Some filename ->
        let chan = open_out filename in
        let fmt = Format.formatter_of_out_channel chan in
        Hashtbl.iter (fun _ stats -> Metrics.pp_all_paths fmt stats) t.visited;
        Fmt.flush fmt ();
        close_out chan
end
