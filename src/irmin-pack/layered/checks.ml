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
open Irmin_pack.Checks
module IO = Irmin_pack.IO.Unix

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

module Make (M : Maker) (Store : S.Store) = struct
  module Simple = Make (M)
  module Hash = Store.Hash

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
        IO_layers.IO.close t >|= fun () -> Some a

  module Stat = struct
    module Layer_stat = Simple.Stat

    type files_layer = {
      flip : [ `Upper1 | `Upper0 ] option;
      lower : Layer_stat.files;
      upper1 : Layer_stat.files;
      upper0 : Layer_stat.files;
    }
    [@@deriving irmin]

    type objects_layer = {
      lower : Layer_stat.objects;
      upper1 : Layer_stat.objects;
      upper0 : Layer_stat.objects;
    }
    [@@deriving irmin]

    type t = {
      hash_size : Layer_stat.size;
      log_size : int;
      files : files_layer;
      objects : objects_layer;
    }
    [@@deriving irmin]

    let v = Layer_stat.v ~version:`V2

    let v ~root =
      read_flip ~root >|= fun flip ->
      let lower = v ~root:(Layout.lower ~root)
      and upper1 = v ~root:(Layout.upper1 ~root)
      and upper0 = v ~root:(Layout.upper0 ~root) in
      { flip; lower; upper1; upper0 }

    let conf root = Irmin_pack.config ~readonly:false ~fresh:false root

    let traverse_indexes ~root log_size =
      let lower = Layer_stat.traverse_index ~root:(Layout.lower ~root) log_size
      and upper1 =
        Layer_stat.traverse_index ~root:(Layout.upper1 ~root) log_size
      and upper0 =
        Layer_stat.traverse_index ~root:(Layout.upper0 ~root) log_size
      in
      { lower; upper1; upper0 }

    let run ~root =
      Logs.app (fun f -> f "Getting statistics for store: `%s'@," root);
      let log_size = conf root |> Irmin_pack.Conf.index_log_size in
      let objects = traverse_indexes ~root log_size in
      let+ files = v ~root in
      { hash_size = Bytes Hash.hash_size; log_size; files; objects }
      |> Irmin.Type.pp_json ~minify:false t Fmt.stdout

    let term_internal =
      Cmdliner.Term.(const (fun root () -> Lwt_main.run (run ~root)) $ path)

    let term =
      let doc = "Print high-level statistics about the store." in
      Cmdliner.Term.(term_internal $ setup_log, info ~doc "stat")
  end

  module Integrity_check = struct
    let conf root = Irmin_pack.config ~readonly:false ~fresh:false root

    let run ~root ~auto_repair =
      let conf = conf root in
      let lower_root = Layout.lower ~root in
      let upper_root1 = Layout.upper1 ~root in
      let upper_root0 = Layout.upper0 ~root in
      let conf = Conf.init ~lower_root ~upper_root1 ~upper_root0 conf in
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
      let conf = Irmin_pack.config ~readonly:true root in
      Conf.init ~with_lower:false conf

    let heads =
      let open Cmdliner.Arg in
      value
      & opt (some (list ~sep:',' string)) None
      & info [ "heads" ] ~doc:"List of head commit hashes" ~docv:"HEADS"

    let check_store ~root ~heads (module S : S.Store) =
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
