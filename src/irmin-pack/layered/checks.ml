open Lwt.Infix
open Irmin_pack.Checks
module IO = Irmin_pack.Private.IO.Unix

let ( let+ ) x f = Lwt.map f x
let ( let* ) = Lwt.bind

module type S = sig
  include S

  module Check_self_contained : sig
    val run : root:string -> heads:string list option -> unit Lwt.t
    (** Ensure that the upper layer of the store is self-contained.*)

    val term : (unit -> unit) Cmdliner.Term.t
    (** A pre-packaged [Cmdliner] term for executing {!run}. *)
  end

  val cli : unit -> empty
  (** Run a [Cmdliner] binary containing tools for running offline checks. *)
end

module Layout = struct
  include Layout

  (** Only works for layered stores that use the default names for layers. *)
  let lower, upper0, upper1 =
    let of_id id ~root =
      Filename.concat root (Irmin_layers.Layer_id.to_string id)
    in
    (of_id `Lower, of_id `Upper0, of_id `Upper1)

  let toplevel root =
    [ Layout.flip ~root; lower ~root; upper1 ~root; upper0 ~root ]
end

module Make (Args : sig
  module Hash : Irmin.Hash.S
  module Store : S.STORE with type hash = Hash.t
end) =
struct
  module Hash = Args.Hash
  module Store = Args.Store

  module Simple = Make (struct
    include Args

    module Store = struct
      include Store

      (* Quick hack to shim functionality that we don't intend to use. *)
      let integrity_check ?ppf:_ ~auto_repair:_ _ = assert false
      let reconstruct_index ?output:_ _ = assert false
    end
  end)

  let read_flip ~root =
    let path = Layout.flip ~root in
    match IO.exists path with
    | false -> Lwt.return_none
    | true ->
        let* t = IO_layers.IO.v path in
        let* a =
          IO_layers.IO.read_flip t >|= function
          | true -> `Upper1
          | false -> `Upper0
        in
        let+ () = IO_layers.IO.close t in
        Some a

  module Stat = struct
    module Layer_stat = Simple.Stat

    type files_layer = {
      flip : [ `Upper1 | `Upper0 ] option;
      lower : Layer_stat.files;
      upper1 : Layer_stat.files;
      upper0 : Layer_stat.files;
    }
    [@@deriving irmin]

    type t = { hash_size : Layer_stat.size; files : files_layer }
    [@@deriving irmin]

    let v ~root =
      read_flip ~root >|= fun flip ->
      let lower = Layer_stat.v ~root:(Layout.lower ~root)
      and upper1 = Layer_stat.v ~root:(Layout.upper1 ~root)
      and upper0 = Layer_stat.v ~root:(Layout.upper0 ~root) in
      { flip; lower; upper1; upper0 }

    let run ~root =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      let+ files = v ~root in
      { hash_size = Bytes Hash.hash_size; files }
      |> Irmin.Type.pp_json ~minify:false t Fmt.stdout

    let term_internal =
      Cmdliner.Term.(const (fun root () -> Lwt_main.run (run ~root)) $ path)

    let term =
      let doc = "Print high-level statistics about the store." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "stat")
  end

  module Integrity_check = struct
    let conf root = Irmin_pack.Config.v ~readonly:false ~fresh:false root

    let run ~root ~auto_repair =
      let conf = conf root in
      let lower_root = Layout.lower ~root in
      let upper_root1 = Layout.upper1 ~root in
      let upper_root0 = Layout.upper0 ~root in
      let conf = Config.v ~conf ~lower_root ~upper_root1 ~upper_root0 () in
      let+ repo = Store.Repo.v conf in
      let res = Store.integrity_check ~auto_repair repo in
      List.iter
        (fun (r, id) ->
          Simple.Integrity_check.handle_result
            ~name:(Irmin_layers.Layer_id.to_string id)
            r)
        res

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

  module Check_self_contained = struct
    let conf root =
      let conf = Irmin_pack.Config.v ~readonly:true root in
      Config.v ~conf ~with_lower:false ()

    let heads =
      let open Cmdliner.Arg in
      value
      & opt (some (list ~sep:',' string)) None
      & info [ "heads" ] ~doc:"List of head commit hashes" ~docv:"HEADS"

    let check_store ~root ~heads (module S : S.STORE) =
      let* repo = S.Repo.v (conf root) in
      let* heads =
        match heads with
        | None -> S.Repo.heads repo
        | Some heads ->
            Lwt_list.filter_map_s
              (fun x ->
                match Repr.of_string S.Hash.t x with
                | Ok x -> S.Commit.of_hash repo x
                | _ -> Lwt.return None)
              heads
      in

      let* () =
        S.check_self_contained ~heads repo >|= function
        | Ok (`Msg msg) -> Logs.app (fun l -> l "Ok -- %s" msg)
        | Error (`Msg msg) -> Logs.err (fun l -> l "Error -- %s" msg)
      in
      S.Repo.close repo

    let run ~root ~heads = check_store ~root ~heads (module Store)

    let term_internal =
      Cmdliner.Term.(
        const (fun root heads () -> Lwt_main.run (run ~root ~heads))
        $ path
        $ heads)

    let term =
      let doc = "Check that the upper layer of the store is self contained." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "check-self-contained")
  end

  let cli () =
    Simple.cli
      ~terms:[ Stat.term; Integrity_check.term; Check_self_contained.term ]
      ()
end
