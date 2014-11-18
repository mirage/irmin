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

open Lwt
open Ir_misc.OP

module Log = Log.Make(struct let section = "SNAPSHOT" end)

module type OF_STORE = sig
  include Ir_ro.STORE
  type db
  val create: db -> origin -> t Lwt.t
  val revert: db -> origin -> t -> unit Lwt.t
  val merge: db -> origin -> t -> unit Ir_merge.result Lwt.t
  val watch: db -> origin -> key -> (key * t) Lwt_stream.t
  type state
  val of_state: db -> state -> t
  val to_state: t -> state
end

module Of_store (S: Ir_bc.STORE_EXT) = struct

  module B = S.Block
  module N = B.Node
  module C = B.Commit
  module K = B.Commit.Key
  module T = S.Tag

  type origin = S.origin
  type db = S.t
  type state = N.key

  module Path = Ir_path.Make(N.Step)
  module PathSet = Ir_misc.Set(Path)
  module StepMap = Ir_misc.Map(N.Step)

  (* XXX: add a path in the tuple to handle snapshot of sub-trees. *)
  type t = (db * state)
  type key = S.key
  type value = S.value

  let db: t -> db = function (db, _) -> db
  let state: t -> state = function (_,key) -> key

  let create db origin =
    S.head db origin >>= function
    | None   -> fail Not_found
    | Some c ->
      C.read (S.commit_t db) origin c >>= function
      | None   -> fail Not_found
      | Some c -> match C.Value.node c with
        | None   -> fail Not_found
        | Some k -> return (db, (k: state))

  let root_node (db, n) origin =
    N.read (S.node_t db) origin n >>= function
    | None   -> return N.empty
    | Some n -> return n

  let map t origin path ~f =
    root_node t origin >>= fun node ->
    f (S.node_t (db t)) origin node path

  let read t origin path =
    map t origin path ~f:N.find

  let read_exn t origin path =
    read t origin path >>= function
    | None   -> fail Not_found
    | Some x -> return x

  let mem t origin path =
    map t origin path ~f:N.valid

  (* XXX: code duplication with Branch.list *)
  let list (t, c) origin paths =
    Log.debugf "list";
    let one path =
      root_node (t, c) origin >>= fun n ->
      N.sub (S.node_t t) origin n path >>= function
      | None      -> return_nil
      | Some node ->
        let c = N.succ (S.node_t t) origin node in
        let c = StepMap.keys c in
        let paths = List.map (fun c -> path @ [c]) c in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = PathSet.of_list paths in
        return (PathSet.union set paths)
      ) PathSet.empty paths
    >>= fun paths ->
    return (PathSet.to_list paths)

  let dump (_, _) =
    failwith "TODO"

  let pre_revert t origin s =
    begin S.head t origin >>= function
      | None   -> return_nil
      | Some h -> return [h]
    end >>= fun parents ->
    let c = C.Value.create origin ~node:(state s) ~parents in
    C.add (S.commit_t t) origin c

  let revert t origin s =
    Log.debugf "revert %a" force (show (module N.Key) (state s));
    pre_revert t origin s >>= fun k ->
    S.update_head t origin k

  let merge t origin s =
    Log.debugf "merge %a" force (show (module N.Key) (state s));
    pre_revert t origin s >>= fun k ->
    S.merge_head t origin k

  let watch db origin path =
    let stream = S.watch_node db origin path in
    Lwt_stream.map (fun (path, n) -> path, (db ,n)) stream

  let of_state t s = (t, s)

  let to_state s = state s

end
