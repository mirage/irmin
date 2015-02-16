(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  val to_hum: t -> string
  val of_hum: db -> string -> t
  val create: db -> t Lwt.t
  val revert: db -> t -> unit Lwt.t
  val merge: db -> ?max_depth:int -> ?n:int -> t -> unit Ir_merge.result Lwt.t
  val merge_exn: db -> ?max_depth:int -> ?n:int -> t -> unit Lwt.t
  val watch: db -> key -> (key * t) Lwt_stream.t
end

module Make (S: Ir_s.STORE) = struct

  module P = S.Private
  module V = P.Contents
  module N = P.Node
  module C = P.Commit
  module K = C.Key
  module T = P.Tag

  module Graph = Ir_node.Graph(V)(N)
  module Path = S.Key
  module PathSet = Ir_misc.Set(Path)
  module StepMap = Ir_misc.Map(S.Key.Step)

  (* XXX: add a path in the tuple to handle snapshot of sub-trees. *)
  type db = S.t
  type key = S.key
  type value = S.value
  type t = db * N.key

  let db (t:t) = fst t
  let contents_t t = P.contents_t (db t)
  let task t = S.task (db t)
  let to_hum (_, k) = N.Key.to_hum k
  let of_hum db s = (db, N.Key.of_hum s)

  let err_not_found n =
    fail (Invalid_argument (Printf.sprintf "Irmin.Snapshot.%s: not found" n))

  let of_head db c =
    C.read_exn (P.commit_t db) c >>= fun c ->
    match C.Val.node c with
    | None   -> err_not_found "of_head"
    | Some k -> return (db, k)

  let create db =
    S.head db >>= function
    | None   -> err_not_found "create"
    | Some c -> of_head db c

  let graph_t db =
    P.contents_t db, P.node_t db

  let map (db, n) path ~f =
    f (graph_t db) n path

  let read t path =
    map t path ~f:Graph.read_contents >>= function
    | None   -> return_none
    | Some c -> V.read (contents_t t) c

  let read_exn t path =
    read t path >>= function
    | None   -> err_not_found "read"
    | Some x -> return x

  let mem t path =
    map t path ~f:Graph.mem_contents

  let pre_revert db (s:N.key) =
    begin S.head db >>= function
      | None   -> return_nil
      | Some h -> return [h]
    end >>= fun parents ->
    let c = C.Val.create (S.task db) ~node:s ~parents in
    C.add (P.commit_t db) c

  let revert db (_, s) =
    Log.debug "revert %a" force (show (module N.Key) s);
    pre_revert db s >>= fun k ->
    S.update_head db k

  let merge db ?max_depth ?n (_, s) =
    Log.debug "merge %a" force (show (module N.Key) s);
    pre_revert db s >>= fun k ->
    S.merge_head db ?max_depth ?n k

  let merge_exn t ?max_depth ?n s = merge t ?max_depth ?n s >>= Ir_merge.exn

  let watch db path =
    let stream = S.watch_head db path in
    Lwt_stream.filter_map_s (fun (path, h) ->
        match h with
        | None   -> Lwt.return_none
        | Some h ->
          of_head db h >>= fun n ->
          return (Some (path, n))
      ) stream

end
