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

module type S = sig
  include Ir_ro.STORE
  type db
  val create: db -> t Lwt.t
  val revert: db -> t -> unit Lwt.t
  val merge: db -> t -> unit Ir_merge.result Lwt.t
  val watch: db -> key -> (key * t) Lwt_stream.t
end

module Make (S: Ir_s.STORE) = struct

  module P = S.Private
  module B = Ir_bc.Make_ext(P)
  module V = Ir_contents.Make_ext(P.Contents)
  module N = Ir_node.Make_ext(P.Contents)(P.Node)
  module C = Ir_commit.Make_ext(P.Contents)(P.Node)(P.Commit)
  module K = P.Commit.Key
  module T = P.Tag

  module Path = N.Path
  module PathSet = Ir_misc.Set(Path)
  module StepMap = Ir_misc.Map(S.Key.Step)

  (* XXX: add a path in the tuple to handle snapshot of sub-trees. *)
  type db = S.t
  type key = S.key
  type value = S.value
  type t = db * N.key

  let db (t:t) = fst t

  let task t = S.task (db t)
  let config t = S.config (db t)

  let of_head db c =
    C.read (P.commit_t db) c >>= function
    | None   -> fail Not_found
    | Some c -> match C.Val.node c with
      | None   -> fail Not_found
      | Some k -> return (db, k)

  let create db =
    S.head db >>= function
    | None   -> fail Not_found
    | Some c -> of_head db c

  let root_node (db, n) =
    N.read (P.node_t db) n >>= function
    | None   -> return N.empty
    | Some n -> return n

  let map t path ~f =
    root_node t >>= fun node ->
    f (P.node_t (db t)) node path

  let read t path =
    map t path ~f:N.find

  let read_exn t path =
    read t path >>= function
    | None   -> fail Not_found
    | Some x -> return x

  let mem t path =
    map t path ~f:N.valid

  (* XXX: code duplication with Branch.list *)
  let list t paths =
    Log.debugf "list";
    let t_n = P.node_t (db t) in
    let one path =
      root_node t >>= fun n ->
      N.sub t_n n path >>= function
      | None      -> return_nil
      | Some node ->
        let steps = N.Val.steps node in
        let paths = List.map (fun c -> path @ [c]) steps in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = PathSet.of_list paths in
        return (PathSet.union set paths)
      ) PathSet.empty paths
    >>= fun paths ->
    return (PathSet.to_list paths)

  let dump _ =
    failwith "TODO"

  let pre_revert db (s:N.key) =
    begin S.head db >>= function
      | None   -> return_nil
      | Some h -> return [h]
    end >>= fun parents ->
    let c = C.Val.create (S.task db) ~node:s ~parents in
    C.add (P.commit_t db) c

  let revert db (_, s) =
    Log.debugf "revert %a" force (show (module N.Key) s);
    pre_revert db s >>= fun k ->
    S.update_head db k

  let merge db (_, s) =
    Log.debugf "merge %a" force (show (module N.Key) s);
    pre_revert db s >>= fun k ->
    S.merge_head db k

  let watch db path =
    let stream = S.watch_head db path in
    Lwt_stream.map_s (fun (path, h) ->
        of_head db h >>= fun n ->
        return (path, n)
      ) stream

end
