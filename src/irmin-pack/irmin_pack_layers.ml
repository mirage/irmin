(*
 * Copyright (c) 2013-2020 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESIrmin. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "irmin.pack.layers" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = `V2

let pp_version = IO.pp_version

let ( // ) = Filename.concat

exception RO_Not_Allowed = IO.Unix.RO_Not_Allowed

exception Unsupported_version of IO.version

module I = IO
module IO = IO.Unix
open Lwt.Infix
module Atomic_write = Store.Atomic_write
module Pack_config = Config

module Default = struct
  let lower_root = "lower"

  let upper_root1 = "upper1"
end

module Conf = Irmin.Private.Conf

let lower_root_key =
  Conf.key ~doc:"The root directory for the lower layer." "root_lower"
    Conf.string Default.lower_root

let lower_root conf = Conf.get conf lower_root_key

let upper_root1_key =
  Conf.key ~doc:"The root directory for the upper layer." "root_upper"
    Conf.string Default.upper_root1

let upper_root1 conf = Conf.get conf upper_root1_key

let config_layers ?(conf = Conf.empty) ?(lower_root = Default.lower_root)
    ?(upper_root1 = Default.upper_root1) () =
  let config = Conf.add conf lower_root_key lower_root in
  let config = Conf.add config upper_root1_key upper_root1 in
  config

module Make_ext
    (Config : Config.S)
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
  module Pack = Pack.File (Index) (H)

  type store_handle =
    | Commit_t : H.t -> store_handle
    | Node_t : H.t -> store_handle
    | Content_t : H.t -> store_handle

  module X = struct
    module Hash = H

    type 'a value = { magic : char; hash : H.t; v : 'a }

    let value a =
      let open Irmin.Type in
      record "value" (fun hash magic v -> { magic; hash; v })
      |+ field "hash" H.t (fun v -> v.hash)
      |+ field "magic" char (fun v -> v.magic)
      |+ field "v" a (fun v -> v.v)
      |> sealr

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let magic = 'B'

          let value = value Val.t

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, t = decode_value s off in
            t.v

          let magic _ = magic
        end)

        module CA = Closeable.Content_addressable (CA_Pack)
        include Layered.Content_addressable (Index) (CA)
      end

      include Irmin.Contents.Store (CA)
    end

    module Node = struct
      module Pa = Layered.Pack_Maker (H) (Index) (Pack)
      module CA = Inode_layers.Make (Config) (H) (Pa) (Node)
      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        module CA_Pack = Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let value = value Val.t

          let magic = 'C'

          let encode_value = Irmin.Type.(unstage (encode_bin value))

          let decode_value = Irmin.Type.(unstage (decode_bin value))

          let encode_bin ~dict:_ ~offset:_ v hash =
            encode_value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, v = decode_value s off in
            v.v

          let magic _ = magic
        end)

        module CA = Closeable.Content_addressable (CA_Pack)
        include Layered.Content_addressable (Index) (CA)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      module AW = Atomic_write (Key) (Val)
      module Closeable_AW = Closeable.Atomic_write (AW)
      include Layered.Atomic_write (Closeable_AW)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type upper_layer = {
        contents : [ `Read ] Contents.CA.U.t;
        node : [ `Read ] Node.CA.U.t;
        commit : [ `Read ] Commit.CA.U.t;
        branch : Branch.U.t;
        index : Index.t;
      }

      type lower_layer = {
        contents : [ `Read ] Contents.CA.L.t;
        node : [ `Read ] Node.CA.L.t;
        commit : [ `Read ] Commit.CA.L.t;
        branch : Branch.L.t;
        index : Index.t;
      }

      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.CA.t;
        node : [ `Read ] Node.CA.t;
        branch : Branch.t;
        commit : [ `Read ] Commit.CA.t;
        lower_index : Index.t;
        upper_index : Index.t;
      }

      let contents_t t = t.contents

      let node_t t = (contents_t t, t.node)

      let commit_t t = (node_t t, t.commit)

      let branch_t t = t.branch

      let batch t f =
        Contents.CA.batch t.contents (fun contents ->
            Node.CA.batch t.node (fun node ->
                Commit.CA.batch t.commit (fun commit ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit)))

      let unsafe_v_upper root config =
        let fresh = Pack_config.fresh config in
        let lru_size = Pack_config.lru_size config in
        let readonly = Pack_config.readonly config in
        let log_size = Pack_config.index_log_size config in
        let throttle = Pack_config.index_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
              (* backpatching to add pack flush before an index flush *)
            ~fresh ~readonly ~throttle ~log_size root
        in
        Contents.CA.U.v ~fresh ~readonly ~lru_size ~index root
        >>= fun contents ->
        Node.CA.U.v ~fresh ~readonly ~lru_size ~index root >>= fun node ->
        Commit.CA.U.v ~fresh ~readonly ~lru_size ~index root >>= fun commit ->
        Branch.U.v ~fresh ~readonly root >|= fun branch ->
        (f := fun () -> Contents.CA.U.flush ~index:false contents);
        ({ index; contents; node; commit; branch } : upper_layer)

      let unsafe_v_lower root config =
        let fresh = Pack_config.fresh config in
        let lru_size = Pack_config.lru_size config in
        let readonly = Pack_config.readonly config in
        let log_size = Pack_config.index_log_size config in
        let throttle = Pack_config.index_throttle config in
        let f = ref (fun () -> ()) in
        let index =
          Index.v
            ~flush_callback:(fun () -> !f ())
              (* backpatching to add pack flush before an index flush *)
            ~fresh ~readonly ~throttle ~log_size root
        in
        Contents.CA.L.v ~fresh ~readonly ~lru_size ~index root
        >>= fun contents ->
        Node.CA.L.v ~fresh ~readonly ~lru_size ~index root >>= fun node ->
        Commit.CA.L.v ~fresh ~readonly ~lru_size ~index root >>= fun commit ->
        Branch.L.v ~fresh ~readonly root >|= fun branch ->
        (f := fun () -> Contents.CA.L.flush ~index:false contents);
        ({ index; contents; node; commit; branch } : lower_layer)

      let v_layer ~v root config =
        Lwt.catch
          (fun () -> v root config)
          (function
            | I.Invalid_version { expected; found }
              when expected = current_version ->
                Log.err (fun m ->
                    m "[%s] Attempted to open store of unsupported version %a"
                      root pp_version found);
                Lwt.fail (Unsupported_version found)
            | e -> Lwt.fail e)

      let v config =
        let root = Pack_config.root config in
        let upper_root = Filename.concat root (upper_root1 config) in
        v_layer ~v:unsafe_v_upper upper_root config >>= fun upper ->
        let lower_root = Filename.concat root (lower_root config) in
        v_layer ~v:unsafe_v_lower lower_root config >|= fun lower ->
        let contents = Contents.CA.v upper.contents lower.contents in
        let node = Node.CA.v upper.node lower.node in
        let commit = Commit.CA.v upper.commit lower.commit in
        let branch = Branch.v upper.branch lower.branch in
        {
          contents;
          node;
          commit;
          branch;
          config;
          lower_index = lower.index;
          upper_index = upper.index;
        }

      let close t =
        Index.close t.lower_index;
        Index.close t.upper_index;
        Contents.CA.close t.contents >>= fun () ->
        Node.CA.close t.node >>= fun () ->
        Commit.CA.close t.commit >>= fun () -> Branch.close t.branch

      let sync t =
        let on_generation_change () =
          Node.CA.clear_caches t.node;
          Commit.CA.clear_caches t.commit
        in
        Contents.CA.sync ~on_generation_change t.contents

      let clear t = Contents.CA.clear t.contents

      (** migrate can be called on a layered store where only one layer exists
          on disk. As migration fails on an empty store, we check which layer is
          in the wrong version. *)
      let migrate config =
        if Pack_config.readonly config then raise RO_Not_Allowed;
        let root = Pack_config.root config in
        [ upper_root1; lower_root ]
        |> List.map (fun name ->
               let root = Filename.concat root (name config) in
               let config = Conf.add config Pack_config.root_key (Some root) in
               try
                 let io =
                   IO.v ~version:(Some current_version) ~fresh:false
                     ~readonly:true (root // "store.pack")
                 in
                 (config, Some io)
               with
               | I.Invalid_version _ -> (config, None)
               | e -> raise e)
        |> List.fold_left
             (fun to_migrate (config, io) ->
               match io with
               | None -> config :: to_migrate
               | Some io ->
                   IO.close io;
                   to_migrate)
             []
        |> List.iter (fun config -> Store.migrate config)

      let layer_id t store_handler =
        match store_handler with
        | Commit_t k -> Commit.CA.layer_id t.commit k
        | Node_t k -> Node.CA.layer_id t.node k
        | Content_t k -> Contents.CA.layer_id t.contents k

      let flush t =
        Contents.CA.flush t.contents;
        Branch.flush t.branch
    end
  end

  let null =
    match Sys.os_type with
    | "Unix" | "Cygwin" -> "/dev/null"
    | "Win32" -> "NUL"
    | _ -> invalid_arg "invalid os type"

  let integrity_check ?ppf ~auto_repair t =
    let ppf =
      match ppf with
      | Some p -> p
      | None -> open_out null |> Format.formatter_of_out_channel
    in
    Fmt.pf ppf "Running the integrity_check.\n%!";
    let nb_commits = ref 0 in
    let nb_nodes = ref 0 in
    let nb_contents = ref 0 in
    let nb_absent = ref 0 in
    let nb_corrupted = ref 0 in
    let exception Cannot_fix in
    let contents = X.Repo.contents_t t in
    let nodes = X.Repo.node_t t |> snd in
    let commits = X.Repo.commit_t t |> snd in
    let pp_stats () =
      Fmt.pf ppf "\t%dk contents / %dk nodes / %dk commits\n%!"
        (!nb_contents / 1000) (!nb_nodes / 1000) (!nb_commits / 1000)
    in
    let count_increment count =
      incr count;
      if !count mod 1000 = 0 then pp_stats ()
    in
    let f (k, (offset, length, m)) =
      match m with
      | 'B' ->
          count_increment nb_contents;
          X.Contents.CA.integrity_check ~offset ~length k contents
      | 'N' | 'I' ->
          count_increment nb_nodes;
          X.Node.CA.integrity_check ~offset ~length k nodes
      | 'C' ->
          count_increment nb_commits;
          X.Commit.CA.integrity_check ~offset ~length k commits
      | _ -> invalid_arg "unknown content type"
    in
    if auto_repair then
      try
        Index.filter t.lower_index (fun binding ->
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
        t.lower_index;
      if !nb_absent = 0 && !nb_corrupted = 0 then Ok `No_error
      else Error (`Corrupted (!nb_corrupted + !nb_absent)))

  include Irmin.Of_private (X)

  let sync = X.Repo.sync

  let clear = X.Repo.clear

  let migrate = X.Repo.migrate

  let flush = X.Repo.flush

  let freeze ?min:_ ?max:_ ?squash:_ _repo = Lwt.return_unit

  let layer_id t = X.Repo.layer_id t
end
