(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Core_kernel.Std
open Lwt
open IrminMerge.OP

type origin = IrminOrigin.t

module Log = Log.Make(struct let section = "SNAPSHOT" end)

module type STORE = sig
  include IrminStore.RO with type key = IrminPath.t
  type db
  val create: db -> t Lwt.t
  val revert: db -> t -> unit Lwt.t
  val merge: db -> ?origin:IrminOrigin.t -> t -> unit IrminMerge.result Lwt.t
  val merge_exn: db -> ?origin:IrminOrigin.t -> t -> unit Lwt.t
  val watch: db -> key -> (key * t) Lwt_stream.t
  type state
  val of_state: db -> state -> t
  val to_state: t -> state
  include IrminIdent.S with type t := state
end

module Make (S: IrminBranch.INTERNAL) = struct

  module Tag = S.Tag
  module Node = S.Block.Node
  module Commit = S.Block.Commit

  module K = S.Block.Key

  type db = S.t

  type t = (S.t * S.Block.key)
  type key = S.key
  type value = S.value

  let create t =
    Tag.read_exn (S.tag_t t) (S.branch t) >>= fun k ->
    return (t, k)

  let root_node (t, c) =
    Commit.read (S.commit_t t) c >>= function
    | None   -> return IrminNode.empty
    | Some c ->
      match Commit.node (S.commit_t t) c with
      | None   -> return IrminNode.empty
      | Some n -> n

  let map (t, c) path ~f =
    root_node (t, c) >>= fun node ->
    f (S.node_t t) node path

  let read (t:t) path =
    map t path ~f:Node.find

  let read_exn t path =
    read t path >>= function
    | None   -> fail Not_found
    | Some x -> return x

  let mem t path =
    map t path ~f:Node.valid

  (* XXX: code duplication with IrminBranch.list *)
  let list (t, c) paths =
    Log.debugf "list";
    let one path =
      root_node (t, c) >>= fun n ->
      Node.sub (S.node_t t) n path >>= function
      | None      -> return_nil
      | Some node ->
        let c = Node.succ (S.node_t t) node in
        let c = Map.keys c in
        let paths = List.map ~f:(fun c -> path @ [c]) c in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = IrminPath.Set.of_list paths in
        return (IrminPath.Set.union set paths)
      ) IrminPath.Set.empty paths
    >>= fun paths ->
    return (IrminPath.Set.to_list paths)

  let dump (t, c) =
    failwith "TODO"

  let revert t (_, c) =
    Tag.update (S.tag_t t) (S.branch t) c

  let merge t ?origin (_, c) =
    let origin = match origin with
      | None   -> IrminOrigin.create "Merge snapshot %s" (K.to_string c)
      | Some o -> o in
    S.merge_commit t ~origin c

  let merge_exn t ?origin c =
    merge t ?origin c >>=
    IrminMerge.exn

  let watch db path =
    let stream = S.watch_node db path in
    Lwt_stream.map (fun (path, c) -> path, (db ,c)) stream

  type state = S.Block.key

  let of_state t s = (t, s)

  let to_state (_, s) = s

  include (S.Block.Key: IrminIdent.S with type t := state)

end
