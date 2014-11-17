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

module Log = Log.Make(struct let section = "VALUE" end)

module Unit = struct
  include Tc.U
  let master = ()
end

module type STORE = sig

  type origin
  type step
  type contents
  type node
  type commit
  type head

  module Contents: Ir_contents.STORE
    with type value = contents
     and type origin = origin

  module Node: Ir_node.STORE
    with type value = node
     and type contents = contents
     and type step = step
     and type origin = origin
     and module Contents = Contents

  module Commit: Ir_commit.STORE
    with type key = head
     and type value = commit
     and type node = node
     and type origin = origin
     and module Node = Node

end

module type MAKER =
  functor (K: Ir_uid.S) ->
  functor (S: Ir_path.STEP) ->
  functor (O: Ir_origin.S) ->
  functor (C: Ir_contents.S) ->
    STORE with type contents = C.t
           and type origin = O.t
           and type step = S.t

module Make
    (Contents: Ir_ao.MAKER)
    (Node: Ir_ao.MAKER)
    (Commit: Ir_ao.MAKER)

end

module Mux
  (K: Ir_uid.S)
  (XC: Ir_contents.S)
  (XContents: Ir_ao.S with type key = K.t and type value = XC.t)
  (XNode: Ir_ao.S with type key = K.t and type value = K.t Ir_node.t)
  (XCommit: Ir_ao.S with type key = K.t and type value = (Ir_origin.t, K.t) Ir_commit.t)
= struct

  type contents = XC.t
  type key = K.t
  type origin = Ir_origin.t
  type value = (origin, K.t, XC.t) t
  module Key = K
  module C = Ir_contents.Make(K)(XC)(XContents)
  module N = Ir_node.Make(K)(XC)(C)(XNode)
  module R = Ir_commit.Make(K)(N)(XCommit)
  module V = S(K)(XC)
  type commit = R.value
  type node = N.value
  type t = {
    contents : C.t;
    node     : N.t;
    commit   : R.t;
  }

  let contents_t t = t.contents
  let node_t t = t.node
  let commit_t t = t.commit

  let create () =
    R.create () >>= fun  ((contents, _ as node), _ as commit) ->
    return { contents; node; commit }

  (* XXX: ugly and slow *)
  let read t key =
    Log.debugf "read %a" force (show (module K) key);
    C.read t.contents key >>= function
    | Some b -> return (Some (Contents b))
    | None   ->
      N.read t.node key >>= function
      | Some t -> return (Some (Node t))
      | None   ->
        R.read t.commit key >>= function
        | Some c -> return (Some (Commit c))
        | None   -> return_none

  let read_exn t key =
    Log.debugf "read_exn %a" force (show (module K) key);
    read t key >>= function
    | Some v -> return v
    | None   -> fail Not_found

  let mem t key =
    read t key >>= function
    | None   -> return false
    | Some _ -> return true

  let add t = function
    | Contents b -> C.add t.contents b
    | Node tr    -> N.add t.node tr
    | Commit c   -> R.add t.commit c

  module Graph = Ir_graph.Make(K)(Unit)
  module Origin = Ir_origin

  let dump t =
    Log.debugf "dump";
    C.dump t.contents >>= fun contents ->
    N.dump t.node     >>= fun nodes ->
    R.dump t.commit   >>= fun commits ->
    let all =
      List.map (fun (k, b) -> k, Contents b) contents
      @ List.map (fun (k, t) -> k, Node t) nodes
      @ List.map (fun (k, c) -> k, Commit c) commits in
    return all

  (* XXX: code repetition ... *)

  let merge t =
    Ir_merge.seq [
      Ir_merge.default (module Key);
      C.merge (contents_t t);
      N.merge (node_t t);
      R.merge (commit_t t);
    ]

  let list t ?depth keys =
    Log.debugf "list %a" force (shows (module K) keys);
    (* start by loading the bounded history. *)
    let pred = function
      | `Commit k ->
        begin R.read t.commit k >>= function
          | None   -> return_nil
          | Some c -> return (Ir_commit.edges c)
        end
      | _ -> return_nil in
    let max = Ir_graph.of_commits keys in
    Graph.closure ?depth ~pred max >>= fun g ->
    let keys = Ir_graph.to_commits (Graph.vertex g) in
    (* then load the rest *)
    let pred = function
      | `Commit _ -> failwith "Irmin.Block.list: commit"
      | `Node k   ->
        begin N.read t.node k >>= function
          | None   -> return_nil
          | Some n -> return (Ir_node.edges n)
        end
      | _ -> return_nil  in
    let max = Ir_graph.of_commits keys in
    Graph.closure ~pred max >>= fun g ->
    let keys = Ir_graph.to_keys (Graph.vertex g) in
    return keys

  module Node = N
  module Commit = R
  module Contents = C
  module Value = V

end

module type CASTABLE = sig
  type t
  type cast
  val proj: t -> cast option
  val inj: cast -> t
end

module Cast (S: Ir_ao.S) (C: CASTABLE with type t = S.value) = struct

  type t = S.t
  type key = S.key
  type value = C.cast

  let create = S.create

  let read t key =
    S.read t key >>= function
    | None   -> return_none
    | Some v ->
      match C.proj v with
      | None   -> return_none
      | Some x -> return (Some x)

  let read_exn t key =
    read t key >>= function
    | Some b -> return b
    | None   -> fail Not_found

  let mem t key =
    read t key >>= function
    | Some _ -> return true
    | None   -> return false

  let list = S.list

  let dump t =
    S.dump t >>= fun cs ->
    let cs = Ir_misc.list_filter_map (fun (k,v) ->
        match C.proj v with
        | Some x -> Some (k, x)
        | None   -> None
      ) cs in
    return cs

  let add t x =
    S.add t (C.inj x)

end

module Make
  (K: Ir_uid.S)
  (XC: Ir_contents.S)
  (Store: Ir_ao.S with type key = K.t and type value = (Ir_origin.t, K.t, XC.t) t)
= struct

  module BS = Cast(Store)(struct
      type t = Store.value
      type cast = XC.t
      let proj = function
        | Contents b -> Some b
        | _          -> None
      let inj b = Contents b
    end)

  module TS = Cast(Store)(struct
      type t = Store.value
      type cast = K.t Ir_node.t
      let proj = function
        | Node t -> Some t
        | _      -> None
      let inj t = Node t
    end)

  module CS = Cast(Store)(struct
      type t = Store.value
      type cast = (Ir_origin.t, K.t) Ir_commit.t
      let proj = function
        | Commit c -> Some c
        | _        -> None
      let inj c = Commit c
    end)

  module C = Ir_contents.Make(K)(XC)(BS)
  module N = Ir_node.Make(K)(XC)(C)(TS)
  module R = Ir_commit.Make(K)(N)(CS)

  type commit = R.value
  type node = N.value
  type origin = Ir_origin.t

  include Store
  type contents = C.value
  module Key = K
  module Value = S(K)(XC)

  module Graph = Ir_graph.Make(K)(Unit)

  let contents_t t = t
  let node_t t = (t, t)
  let commit_t t = ((t, t), t)

  (* XXX: code repetition ... *)

  let merge t =
    Ir_merge.seq [
      Ir_merge.default (module Key);
      C.merge (contents_t t);
      N.merge (node_t t);
      R.merge (commit_t t);
    ]

  let list t ?depth keys =
    Log.debugf "list %a" force (shows (module K) keys);
    (* start by loading the bounded history. *)
    let pred = function
      | `Commit k ->
        begin R.read (commit_t t) k >>= function
          | None   -> return_nil
          | Some c -> return (Ir_commit.edges c)
        end
      | _ -> return_nil in
    let max = Ir_graph.of_commits keys in
    Graph.closure ?depth ~pred max >>= fun g ->
    let keys = Ir_graph.to_commits (Graph.vertex g) in
    (* then load the rest *)
    let pred = function
      | `Commit _ -> failwith "Irmin.Block.list: commit"
      | `Node k   ->
        begin N.read (node_t t) k >>= function
          | None   -> return_nil
          | Some n -> return (Ir_node.edges n)
        end
      | _ -> return_nil  in
    let max = Ir_graph.of_commits keys in
    Graph.closure ~pred max >>= fun g ->
    let keys = Ir_graph.to_keys (Graph.vertex g) in
    return keys

  module Node = N
  module Commit = R
  module Contents = C
  module Origin = Ir_origin
end

module Rec (S: STORE) = struct
  include S.Key
  let merge =
    let merge ~origin ~old k1 k2 =
      S.create ()  >>= fun t  ->
      Ir_merge.merge (S.merge t) ~origin ~old k1 k2
    in
    Ir_merge.create' (module S.Key) merge
end
