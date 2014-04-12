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

type ('key, 'contents) t =
  | Contents of 'contents
  | Node of 'key IrminNode.t
  | Commit of 'key IrminCommit.t
with bin_io, compare, sexp

let to_json json_of_key json_of_contents = function
  | Contents b   -> `O [ "contents"  , json_of_contents b ]
  | Node t   -> `O [ "node"  , IrminNode.to_json json_of_key t ]
  | Commit c -> `O [ "commit", IrminCommit.to_json json_of_key c]

let of_json key_of_json contents_of_json json =
  match Ezjsonm.get_dict json with
  | [ "contents"  , b ] -> Contents (contents_of_json b)
  | [ "node"  , t ] -> Node (IrminNode.of_json key_of_json t)
  | [ "commit", c ] -> Commit (IrminCommit.of_json key_of_json c)
  | _ -> failwith ("error: Value.of_json " ^ Ezjsonm.to_string json)

module type S = sig
  type key
  type contents
  include IrminContents.S with type t = (key, contents) t
end

module S (K: IrminKey.S) (C: IrminContents.S) = struct

  type key = K.t
  type contents = C.t

  module L = Log.Make(struct let section = "VALUE" end)

  module S = IrminMisc.Identifiable(struct
      type nonrec t = (K.t, C.t) t
      with bin_io, compare, sexp
    end)
  include S

  module Key = K

  module Contents = C

  module Node = IrminNode.S(K)

  module Commit = IrminCommit.S(K)

  let of_json =
    of_json K.of_json C.of_json

  let to_json =
    to_json K.to_json C.to_json

  let merge =
    IrminMerge.default (module S)

end

module String = S(IrminKey.SHA1)(IrminContents.String)

module JSON = S(IrminKey.SHA1)(IrminContents.JSON)

module type STORE = sig
  type key
  type contents
  include IrminStore.AO with type key := key
                         and type value = (key, contents) t
  module Contents: IrminContents.STORE
    with type key = key
     and type value = contents
  module Node: IrminNode.STORE
    with type key = key
     and type contents = contents
  module Commit: IrminCommit.STORE
    with type key = key
  val contents: t -> Contents.t
  val node: t -> Node.t
  val commit: t -> Commit.t
  module Key: IrminKey.S with type t = key
  module Value: S with type key = key and type contents = contents
end

module Mux
  (K: IrminKey.S)
  (C: IrminContents.S)
  (XContents: IrminStore.AO with type key = K.t and type value = C.t)
  (XNode    : IrminStore.AO with type key = K.t and type value = K.t IrminNode.t)
  (XCommit  : IrminStore.AO with type key = K.t and type value = K.t IrminCommit.t)
= struct

  type contents = C.t
  type key = K.t
  type value = (K.t, C.t) t
  module Key = K
  module Contents = IrminContents.Make(K)(C)(XContents)
  module Node = IrminNode.Make(K)(C)(Contents)(XNode)
  module Commit = IrminCommit.Make(K)(Node)(XCommit)
  module Value = S(K)(C)

  type t = {
    contents : Contents.t;
    node     : Node.t;
    commit   : Commit.t;
  }

  let contents t = t.contents
  let node t = t.node
  let commit t = t.commit

  open Lwt

  let create () =
    Commit.create () >>= fun ((contents, _ as node), _ as commit) ->
    return { contents; node; commit }

  (* XXX: ugly *)
  let read t key =
    Log.debugf "read %s" (K.to_string key);
    Contents.read t.contents key >>= function
    | Some b -> return (Some (Contents b))
    | None   ->
      Node.read t.node key >>= function
      | Some t -> return (Some (Node t))
      | None   ->
        Commit.read t.commit key >>= function
        | Some c -> return (Some (Commit c))
        | None   -> return_none

  let read_exn t key =
    Log.debugf "read_exn %s" (K.to_string key);
    read t key >>= function
    | Some v -> return v
    | None   -> fail Not_found

  let mem t key =
    read t key >>= function
    | None   -> return false
    | Some _ -> return true

  let add t = function
    | Contents b   -> Contents.add t.contents b
    | Node tr  -> Node.add t.node tr
    | Commit c -> Commit.add t.commit c

  let list t key =
    Log.debugf "list %s" (K.to_string key);
    Commit.list t.commit key

  let dump t =
    Log.debugf "dump";
    Contents.dump t.contents >>= fun contents ->
    Node.dump t.node         >>= fun nodes ->
    Commit.dump t.commit     >>= fun commits ->
    let all =
      List.map contents ~f:(fun (k, b) -> k, Contents b)
      @ List.map nodes ~f:(fun (k, t) -> k, Node t)
      @ List.map commits ~f:(fun (k, c) -> k, Commit c) in
    return all

end


module type CASTABLE = sig
  type t
  type cast
  val proj: t -> cast option
  val inj: cast -> t
end

module Cast (S: IrminStore.AO) (C: CASTABLE with type t = S.value) = struct

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
    let cs = List.filter_map cs ~f:(fun (k,v) ->
        match C.proj v with
        | Some x -> Some (k, x)
        | None   -> None
      ) in
    return cs

  let add t x =
    S.add t (C.inj x)

end

module Make
  (K: IrminKey.S)
  (C: IrminContents.S)
  (Store: IrminStore.AO with type key = K.t and type value = (K.t, C.t) t)
= struct

  module BS = Cast(Store)(struct
      type t = Store.value
      type cast = C.t
      let proj = function
        | Contents b -> Some b
        | _          -> None
      let inj b = Contents b
    end)

  module TS = Cast(Store)(struct
      type t = Store.value
      type cast = K.t IrminNode.t
      let proj = function
        | Node t -> Some t
        | _      -> None
      let inj t = Node t
    end)

  module CS = Cast(Store)(struct
      type t = Store.value
      type cast = K.t IrminCommit.t
      let proj = function
        | Commit c -> Some c
        | _        -> None
      let inj c = Commit c
    end)

  module XContents = IrminContents.Make(K)(C)(BS)
  module XNode = IrminNode.Make(K)(C)(XContents)(TS)
  module XCommit = IrminCommit.Make(K)(XNode)(CS)

  include Mux(K)(C)(XContents)(XNode)(XCommit)

end
