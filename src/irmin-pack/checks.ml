(*
 * Copyright (c) 2018-2021 Tarides <contact@tarides.com>
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
module IO = IO.Unix

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

module Make (M : Maker) = struct
  module Store_V1 = M (Version.V1)
  module Store_V2 = M (Version.V2)
  module Hash = Store_V1.Hash
  module Index = Pack_index.Make (Hash)

  let current_version = `V1

  (** Read basic metrics from an existing store. *)
  module Stat = struct
    type size = Bytes of int [@@deriving irmin]
    type version = [ `V1 | `V2 ] [@@deriving irmin]

    type io = {
      size : size;
      offset : int63;
      generation : int63;
      version : version;
    }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    type objects = { nb_commits : int; nb_nodes : int; nb_contents : int }
    [@@deriving irmin]

    type t = {
      hash_size : size;
      log_size : int;
      files : files;
      objects : objects;
    }
    [@@deriving irmin]

    let with_io : type a. Version.t -> string -> (IO.t -> a) -> a option =
     fun version path f ->
      match IO.exists path with
      | false -> None
      | true ->
          let io =
            IO.v ~fresh:false ~readonly:true ~version:(Some version) path
          in
          Fun.protect ~finally:(fun () -> IO.close io) (fun () -> Some (f io))

    let detect_version ~root =
      try
        let path = Layout.pack ~root in
        match with_io current_version path Fun.id with
        | None -> Fmt.failwith "cannot read pack file"
        | Some _ -> current_version
      with Version.Invalid { expected = _; found } -> found

    let io ~version path =
      with_io version path @@ fun io ->
      let offset = IO.offset io in
      let generation = IO.generation io in
      let size = Bytes (IO.size io) in
      let version = IO.version io in
      { size; offset; generation; version }

    let v ~root ~version =
      let pack = Layout.pack ~root |> io ~version in
      let branch = Layout.branch ~root |> io ~version in
      let dict = Layout.dict ~root |> io ~version in
      { pack; branch; dict }

    let traverse_index ~root log_size =
      let index = Index.v ~readonly:true ~fresh:false ~log_size root in
      let bar, (progress_contents, progress_nodes, progress_commits) =
        Utils.Progress.increment ~ppf:Format.err_formatter ()
      in
      let f _ (_, _, (kind : Pack_value.Kind.t)) =
        match kind with
        | Contents -> progress_contents ()
        | Node | Inode -> progress_nodes ()
        | Commit -> progress_commits ()
      in
      Index.iter f index;
      let nb_commits, nb_nodes, nb_contents =
        Utils.Progress.finalise_with_stats bar
      in
      { nb_commits; nb_nodes; nb_contents }

    let conf root = Conf.v ~readonly:true ~fresh:false root

    let run_versioned_store ~root version =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      let log_size = conf root |> Conf.index_log_size in
      let objects = traverse_index ~root log_size in
      let files = v ~root ~version in
      { hash_size = Bytes Hash.hash_size; log_size; files; objects }
      |> Irmin.Type.pp_json ~minify:false t Fmt.stdout;
      Lwt.return_unit

    let run ~root = detect_version ~root |> run_versioned_store ~root

    let term_internal =
      Cmdliner.Term.(const (fun root () -> Lwt_main.run (run ~root)) $ path)

    let term =
      let doc = "Print high-level statistics about the store." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "stat")
  end

  module Reconstruct_index = struct
    let conf ~index_log_size root =
      Conf.v ~readonly:false ~fresh:false ?index_log_size root

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
      let (module Store : Versioned_store) =
        match Stat.detect_version ~root with
        | `V1 -> (module Store_V1)
        | `V2 -> (module Store_V2)
      in
      let conf = conf ~index_log_size root in
      Store.reconstruct_index ?output conf

    let term_internal =
      Cmdliner.Term.(
        const (fun root output index_log_size () ->
            run ~root ~output ?index_log_size ())
        $ path
        $ dest
        $ index_log_size)

    let term =
      let doc = "Reconstruct index from an existing pack file." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "reconstruct-index")
  end

  module Integrity_check = struct
    let conf root = Conf.v ~readonly:false ~fresh:false root

    let handle_result ?name res =
      let name = match name with Some x -> x ^ ": " | None -> "" in
      match res with
      | Ok (`Fixed n) -> Printf.printf "%sOk -- fixed %d\n%!" name n
      | Ok `No_error -> Printf.printf "%sOk\n%!" name
      | Error (`Cannot_fix x) ->
          Printf.eprintf "%sError -- cannot fix: %s\n%!" name x
      | Error (`Corrupted x) ->
          Printf.eprintf "%sError -- corrupted: %d\n%!" name x

    let run_versioned_store ~root ~auto_repair (module Store : Versioned_store)
        =
      let conf = conf root in
      let+ repo = Store.Repo.v conf in
      Store.integrity_check ~ppf:Format.err_formatter ~auto_repair repo
      |> handle_result ?name:None

    let run ~root ~auto_repair =
      match Stat.detect_version ~root with
      | `V1 -> run_versioned_store ~root ~auto_repair (module Store_V1)
      | `V2 -> run_versioned_store ~root ~auto_repair (module Store_V2)

    let term_internal =
      let auto_repair =
        let open Cmdliner.Arg in
        value
        & (flag @@ info ~doc:"Automatically repair issues" [ "auto-repair" ])
      in
      Cmdliner.Term.(
        const (fun root auto_repair () -> Lwt_main.run (run ~root ~auto_repair))
        $ path
        $ auto_repair)

    let term =
      let doc = "Check integrity of an existing store." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "integrity-check")
  end

  module Integrity_check_inodes = struct
    let conf root = Conf.v ~readonly:true ~fresh:false root

    let heads =
      let open Cmdliner.Arg in
      value
      & opt (some (list ~sep:',' string)) None
      & info [ "heads" ] ~doc:"List of head commit hashes" ~docv:"HEADS"

    let run_versioned_store ~root ~heads (module Store : Versioned_store) =
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
                | _ -> Lwt.return None)
              heads
      in
      let* () =
        Store.integrity_check_inodes ~heads repo >|= function
        | Ok (`Msg msg) -> Logs.app (fun l -> l "Ok -- %s" msg)
        | Error (`Msg msg) -> Logs.err (fun l -> l "Error -- %s" msg)
      in
      Store.Repo.close repo

    let run ~root ~heads =
      match Stat.detect_version ~root with
      | `V1 -> run_versioned_store ~root ~heads (module Store_V1)
      | `V2 -> run_versioned_store ~root ~heads (module Store_V2)

    let term_internal =
      Cmdliner.Term.(
        const (fun root heads () -> Lwt_main.run (run ~root ~heads))
        $ path
        $ heads)

    let term =
      let doc = "Check integrity of inodes in an existing store." in
      Cmdliner.Term.
        (term_internal $ setup_log, info ~doc "integrity-check-inodes")
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
          ]) () : empty =
      let default =
        let default_info =
          let doc = "Check Irmin data-stores." in
          Term.info ~doc "irmin-fsck"
        in
        Term.(ret (const (`Help (`Auto, None))), default_info)
      in
      Term.(eval_choice default terms |> (exit : unit result -> _));
      assert false
  end

  let cli = Cli.main
end

module Index (Index : Pack_index.S) = struct
  let null =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> "/dev/null"
    | "Win32" -> "NUL"
    | _ -> invalid_arg "invalid os type"

  let integrity_check ?ppf ~auto_repair ~check index =
    let ppf =
      match ppf with
      | Some p -> p
      | None -> open_out null |> Format.formatter_of_out_channel
    in
    Fmt.pf ppf "Running the integrity_check.\n%!";
    let nb_absent = ref 0 in
    let nb_corrupted = ref 0 in
    let exception Cannot_fix in
    let bar, (progress_contents, progress_nodes, progress_commits) =
      Utils.Progress.increment ()
    in
    let f (k, (offset, length, (kind : Pack_value.Kind.t))) =
      match kind with
      | Contents ->
          progress_contents ();
          check ~kind:`Contents ~offset ~length k
      | Node | Inode ->
          progress_nodes ();
          check ~kind:`Node ~offset ~length k
      | Commit ->
          progress_commits ();
          check ~kind:`Commit ~offset ~length k
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
    Utils.Progress.finalise bar;
    result
end
