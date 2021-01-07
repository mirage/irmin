include Checks_intf
module T = Irmin.Type
module IO = IO.Unix
open Lwt.Infix

let current_version = `V2

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

module Make (Args : Make_args) = struct
  module Hash = Args.Hash
  module Store = Args.Store
  module Index = Pack_index.Make (Hash)

  (** Read basic metrics from an existing store. *)
  module Stat = struct
    type size = Bytes of int [@@deriving irmin]

    type io = { size : size; offset : int64; generation : int64 }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    type t = { hash_size : size; files : files } [@@deriving irmin]

    let with_io : type a. string -> (IO.t -> a) -> a option =
     fun path f ->
      match IO.exists path with
      | false -> None
      | true ->
          let io =
            IO.v ~fresh:false ~readonly:true ~version:(Some current_version)
              path
          in
          Fun.protect ~finally:(fun () -> IO.close io) (fun () -> Some (f io))

    let io path =
      with_io path @@ fun io ->
      let offset = IO.offset io in
      let generation = IO.generation io in
      let size = Bytes (IO.size io) in
      { size; offset; generation }

    let v ~root =
      let pack = Layout.pack ~root |> io in
      let branch = Layout.branch ~root |> io in
      let dict = Layout.dict ~root |> io in
      { pack; branch; dict }

    let run ~root =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      let files = v ~root in
      { hash_size = Bytes Hash.hash_size; files }
      |> T.pp_json ~minify:false t Fmt.stdout;
      Lwt.return_unit

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

    let run ~root ~output =
      let conf = conf root in
      Store.reconstruct_index ?output conf

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

    let run ~root ~auto_repair =
      let conf = conf root in
      Store.Repo.v conf >|= fun repo ->
      Store.integrity_check ~auto_repair repo |> handle_result ?name:None

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

  module Cli = struct
    open Cmdliner

    let main
        ?(terms = [ Stat.term; Reconstruct_index.term; Integrity_check.term ])
        () : empty =
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
