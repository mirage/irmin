include Checks_intf
module T = Irmin.Type
module IO = IO.Unix
open Lwt.Infix

let ( // ) = Filename.concat

let current_version = `V2

(** Only works for layered stores that use the default names for layers. *)
module Layout = struct
  let pack root = root // "store.pack"

  let branch root = root // "store.branches"

  let dict root = root // "store.dict"

  let flip root = root // "flip"

  let lower root = root // "lower"

  let upper1 root = root // "upper1"

  let upper0 root = root // "upper0"

  let toplevel root = [ flip root; lower root; upper1 root; upper0 root ]
end

module Make
    (Conf : Config.S)
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S
              with type metadata = M.t
               and type hash = H.t
               and type step = P.step)
    (Commit : Irmin.Private.Commit.S with type hash = H.t) =
struct
  module Index = Pack_index.Make (H)

  type size = Bytes of int [@@deriving irmin]

  let detect_layered_store ~root =
    root |> Layout.toplevel |> List.exists (fun path -> IO.exists path)

  let detect_pack_layer ~layer_root = Layout.dict layer_root |> IO.exists

  let read_flip ~root =
    let path = Layout.flip root in
    match IO.exists path with
    | false -> Lwt.return_none
    | true ->
        IO_layers.IO.v path >>= fun t ->
        (IO_layers.IO.read_flip t >|= function
         | true -> `Upper1
         | false -> `Upper0)
        >>= fun a ->
        IO_layers.IO.close t >|= fun () -> Some a

  (** Read basic metrics from an existing store. *)
  module Quick_stats = struct
    type io = { size : size; offset : int64; generation : int64 }
    [@@deriving irmin]

    type files = { pack : io option; branch : io option; dict : io option }
    [@@deriving irmin]

    type files_layer = {
      flip : [ `Upper1 | `Upper0 ] option;
      lower : files;
      upper1 : files;
      upper0 : files;
    }
    [@@deriving irmin]

    type files_store = Layered of files_layer | Simple of files
    [@@deriving irmin]

    type t = { hash_size : size; files : files_store } [@@deriving irmin]

    let with_io : type a. string -> (IO.t -> a) -> a option =
     fun path f ->
      match IO.exists path with
      | false -> None
      | true ->
          let io =
            IO.v ~fresh:false ~readonly:true ~version:(Some current_version)
              path
          in
          let a = f io in
          IO.close io;
          Some a

    let io path =
      with_io path @@ fun io ->
      let offset = IO.offset io in
      let generation = IO.generation io in
      let size = Bytes (IO.size io) in
      { size; offset; generation }

    let v_simple ~root =
      let pack = Layout.pack root |> io in
      let branch = Layout.branch root |> io in
      let dict = Layout.dict root |> io in
      { pack; branch; dict }

    let v_layered ~root =
      match detect_layered_store ~root with
      | false -> Lwt.return (Simple (v_simple ~root))
      | true -> (
          Logs.app (fun f -> f "Layered store detected");
          read_flip ~root >|= fun flip ->
          Layout.toplevel root
          |> List.tl
          |> List.map (fun root -> v_simple ~root)
          |> function
          | [ lower; upper1; upper0 ] -> Layered { flip; lower; upper1; upper0 }
          | _ -> assert false)

    let v ~root =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      v_layered ~root >>= fun files ->
      { hash_size = Bytes H.hash_size; files }
      |> T.pp_json ~minify:false t Fmt.stdout;
      Lwt.return_unit
  end

  module Check_containment = struct
    module Store =
      Irmin_pack_layers.Make_ext (Conf) (M) (C) (P) (B) (H) (Node) (Commit)

    let conf root =
      let conf = Config.v ~readonly:true root in
      Irmin_pack_layers.config_layers ~conf ~with_lower:false ()

    let check_store ~root (module S : Irmin_pack_layers.S) =
      S.Repo.v (conf root) >>= fun repo ->
      (S.check_self_contained repo >|= function
       | Ok (`Msg msg) -> Logs.app (fun l -> l "Ok -- %s" msg)
       | Error (`Msg msg) -> Logs.err (fun l -> l "Error -- %s" msg))
      >>= fun () -> S.Repo.close repo

    let v ~root =
      if not (detect_layered_store ~root) then
        Fmt.failwith "%s is not a layered store." root;
      (read_flip ~root >|= function
       | None | Some `Upper1 -> Layout.upper1 root
       | Some `Upper0 -> Layout.upper0 root)
      >>= fun upper ->
      if detect_pack_layer ~layer_root:upper then
        check_store ~root (module Store)
      else Fmt.failwith "To fix"
  end

  module Cli = struct
    open Cmdliner

    let path =
      let open Arg in
      required
      @@ pos 0 (some string) None
      @@ info ~doc:"Path to the Irmin store on disk" ~docv:"PATH" []

    let quick_stats =
      let run ~root = Lwt_main.run (Quick_stats.v ~root) in
      Term.(const (fun root () -> run ~root) $ path)

    let check_containment =
      let run ~root = Lwt_main.run (Check_containment.v ~root) in
      Term.(const (fun root () -> run ~root) $ path)

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
      Term.(const init $ Fmt_cli.style_renderer () $ Logs_cli.level ())

    let main () : empty =
      let default =
        let default_info =
          let doc = "Check Irmin data-stores." in
          Term.info ~doc "irmin-fsck"
        in
        Term.(ret (const (`Help (`Auto, None))), default_info)
      in
      Term.(
        eval_choice default
          [
            ( quick_stats $ setup_log,
              Term.info ~doc:"Print high-level statistics about the store."
                "stat" );
            ( check_containment $ setup_log,
              Term.info
                ~doc:
                  "Check that the upper layer of the store is self contained."
                "check" );
          ]
        |> (exit : unit result -> _));
      assert false
  end

  let cli = Cli.main
end
