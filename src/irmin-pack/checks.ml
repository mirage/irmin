include Checks_intf
module T = Irmin.Type
module I = IO
module IO = IO.Unix
open! Import

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

module Make (M : MAKER) = struct
  module V1 = struct
    let io_version = `V1
  end

  module V2 = struct
    let io_version = `V2
  end

  module Store_V1 = M (V1)
  module Store_V2 = M (V2)
  module Hash = Store_V1.Hash
  module Index = Pack_index.Make (Hash)

  let current_version = `V1

  (** Read basic metrics from an existing store. *)
  module Stat = struct
    type size = Bytes of int [@@deriving irmin]
    type version = [ `V1 | `V2 ] [@@deriving irmin]

    type io = {
      size : size;
      offset : int64;
      generation : int64;
      version : version;
    }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    type t = { hash_size : size; files : files } [@@deriving irmin]

    let with_io : type a. I.version -> string -> (IO.t -> a) -> a option =
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
      with I.Invalid_version { expected = _; found } -> found

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

    let run_versioned_store ~root version =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      let files = v ~root ~version in
      { hash_size = Bytes Hash.hash_size; files }
      |> T.pp_json ~minify:false t Fmt.stdout;
      Lwt.return_unit

    let run ~root = detect_version ~root |> run_versioned_store ~root

    let term_internal =
      Cmdliner.Term.(const (fun root () -> Lwt_main.run (run ~root)) $ path)

    let term =
      let doc = "Print high-level statistics about the store." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "stat")
  end

  module Reconstruct_index = struct
    let conf root = Config.v ~readonly:false ~fresh:false root

    let dest =
      let open Cmdliner.Arg in
      value
      & pos 1 (some string) None
        @@ info ~doc:"Path to the new index file" ~docv:"DEST" []

    let run_versioned_store ~root ~output (module Store : Versioned_store) =
      let conf = conf root in
      Store.reconstruct_index ?output conf

    let run ~root ~output =
      match Stat.detect_version ~root with
      | `V1 -> run_versioned_store ~root ~output (module Store_V1)
      | `V2 -> run_versioned_store ~root ~output (module Store_V2)

    let term_internal =
      Cmdliner.Term.(
        const (fun root output () -> run ~root ~output) $ path $ dest)

    let term =
      let doc = "Reconstruct index from an existing pack file." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "reconstruct-index")
  end

  module Integrity_check = struct
    let conf root = Config.v ~readonly:false ~fresh:false root

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
    let conf root = Config.v ~readonly:true ~fresh:false root

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
