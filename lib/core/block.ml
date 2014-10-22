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
open Merge.OP
open Misc.OP

module Log = Log.Make(struct let section = "VALUE" end)

module T3 = struct
  type ('origin, 'key, 'contents) t =
    | Contents of 'contents
    | Node of 'key Node.t
    | Commit of ('origin, 'key) Commit.t
  with bin_io, compare, sexp
end
include T3
module T = Tc.I3(T3)

type origin = Origin.t

module type S = sig
  type key
  type contents
  include Sig.Contents with type t = (origin, key, contents) t
end

module S (K: Sig.Uid) (C: Sig.Contents) = struct
  type key = K.t
  type contents = C.t
  module S = Tc.App3(T)(Origin)(K)(C)
  include S
  module Key = K
  module Contents = C
  module Node = Node.S(K)
  module Commit = Commit.S(K)
  let merge = Merge.default (module S)
end

module String = S(Uid.SHA1)(Contents.String)

module JSON = S(Uid.SHA1)(Contents.JSON)

module Unit = struct
  include Tc.U
  let master = ()
  let compute_from_string _ = ()
  let compute_from_cstruct _ = ()
  let to_raw () = ""
  let of_raw _ = ()
  let pretty () = ""
end

module type STORE = sig
  type key
  type contents
  type node = key Node.t
  type commit = (origin, key) Commit.t
  include Sig.AO with type key := key and type value = (origin, key, contents) t
  val list: t -> ?depth:int -> key list -> key list Lwt.t
  module Contents: Contents.STORE with type key = key and type value = contents
  module Node: Node.STORE with type key = key and type contents = contents
  module Commit: Commit.STORE with type key = key
  val contents_t: t -> Contents.t
  val node_t: t -> Node.t
  val commit_t: t -> Commit.t
  val merge: t -> key Merge.t
  module Key: Sig.Uid with type t = key
  module Value: S with type key = key and type contents = contents
  module Graph: Digraph.S with type V.t = (key, unit) Digraph.vertex
end

module Mux
  (K: Sig.Uid)
  (XC: Sig.Contents)
  (XContents: Sig.AO with type key = K.t and type value = XC.t)
  (XNode: Sig.AO with type key = K.t and type value = K.t Node.t)
  (XCommit: Sig.AO with type key = K.t and type value = (origin, K.t) Commit.t)
= struct

  type contents = XC.t
  type key = K.t
  type value = (origin, K.t, XC.t) t
  module Key = K
  module C = Contents.Make(K)(XC)(XContents)
  module N = Node.Make(K)(XC)(C)(XNode)
  module R = Commit.Make(K)(N)(XCommit)
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

  module Graph = Digraph.Make(K)(Unit)

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
    Merge.seq [
      Merge.default (module Key);
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
          | Some c -> return (Commit.edges c)
        end
      | _ -> return_nil in
    let max = Digraph.of_commits keys in
    Graph.closure ?depth ~pred max >>= fun g ->
    let keys = Digraph.to_commits (Graph.vertex g) in
    (* then load the rest *)
    let pred = function
      | `Commit k -> failwith "Irmin.Block.list: commit"
      | `Node k   ->
        begin N.read t.node k >>= function
          | None   -> return_nil
          | Some n -> return (Node.edges n)
        end
      | _ -> return_nil  in
    let max = Digraph.of_commits keys in
    Graph.closure ~pred max >>= fun g ->
    let keys = Digraph.to_keys (Graph.vertex g) in
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

module Cast (S: Sig.AO) (C: CASTABLE with type t = S.value) = struct

  open Lwt

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
    let cs = Misc.list_filter_map (fun (k,v) ->
        match C.proj v with
        | Some x -> Some (k, x)
        | None   -> None
      ) cs in
    return cs

  let add t x =
    S.add t (C.inj x)

end

module Make
  (K: Sig.Uid)
  (XC: Sig.Contents)
  (Store: Sig.AO with type key = K.t and type value = (origin, K.t, XC.t) t)
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
      type cast = K.t Node.t
      let proj = function
        | Node t -> Some t
        | _      -> None
      let inj t = Node t
    end)

  module CS = Cast(Store)(struct
      type t = Store.value
      type cast = (origin, K.t) Commit.t
      let proj = function
        | Commit c -> Some c
        | _        -> None
      let inj c = Commit c
    end)

  module C = Contents.Make(K)(XC)(BS)
  module N = Node.Make(K)(XC)(C)(TS)
  module R = Commit.Make(K)(N)(CS)

  type commit = R.value
  type node = N.value

  include Store
  type contents = C.value
  module Key = K
  module Value = S(K)(XC)

  module Graph = Digraph.Make(K)(Unit)

  let contents_t t = t
  let node_t t = (t, t)
  let commit_t t = ((t, t), t)

  (* XXX: code repetition ... *)

  let merge t =
    Merge.seq [
      Merge.default (module Key);
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
          | Some c -> return (Commit.edges c)
        end
      | _ -> return_nil in
    let max = Digraph.of_commits keys in
    Graph.closure ?depth ~pred max >>= fun g ->
    let keys = Digraph.to_commits (Graph.vertex g) in
    (* then load the rest *)
    let pred = function
      | `Commit k -> failwith "Irmin.Block.list: commit"
      | `Node k   ->
        begin N.read (node_t t) k >>= function
          | None   -> return_nil
          | Some n -> return (Node.edges n)
        end
      | _ -> return_nil  in
    let max = Digraph.of_commits keys in
    Graph.closure ~pred max >>= fun g ->
    let keys = Digraph.to_keys (Graph.vertex g) in
    return keys

  module Node = N
  module Commit = R
  module Contents = C

end

module Rec (S: STORE) = struct
  include S.Key
  let merge =
    let merge ~origin ~old k1 k2 =
      S.create ()  >>= fun t  ->
      Merge.merge (S.merge t) ~origin ~old k1 k2
    in
    Merge.create' (module S.Key) merge
end
