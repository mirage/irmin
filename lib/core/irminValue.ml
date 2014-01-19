(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type ('key, 'blob) t =
  | Blob of 'blob
  | Tree of 'key IrminTree.t
  | Commit of 'key IrminCommit.t
with bin_io, compare, sexp

let to_json json_of_key json_of_blob = function
  | Blob b   -> `O [ "blob"  , json_of_blob b ]
  | Tree t   -> `O [ "tree"  , IrminTree.to_json json_of_key t ]
  | Commit c -> `O [ "commit", IrminCommit.to_json json_of_key c]

let of_json key_of_json blob_of_json json =
  match Ezjsonm.get_dict json with
  | [ "blob"  , b ] -> Blob (blob_of_json b)
  | [ "tree"  , t ] -> Tree (IrminTree.of_json key_of_json t)
  | [ "commit", c ] -> Commit (IrminCommit.of_json key_of_json c)
  | _ -> failwith ("error: Value.of_json " ^ Ezjsonm.to_string json)

module type S = sig
  type key
  type blob
  include IrminBlob.S with type t = (key, blob) t
end

module S (K: IrminKey.S) (B: IrminBlob.S) = struct

  type key = K.t
  type blob = B.t

  module L = Log.Make(struct let section = "VALUE" end)

  module M = struct
    type nonrec t = (K.t, B.t) t
    with bin_io, compare, sexp
    let hash (t : t) = Hashtbl.hash t
    include Sexpable.To_stringable (struct type nonrec t = t with sexp end)
    let module_name = "Value"
    let name = "value"
  end
  include M
  include Identifiable.Make (M)

  module Key = K

  module Blob = B

  module Tree = IrminTree.S(K)

  module Commit = IrminCommit.S(K)

  let of_json =
    of_json K.of_json B.of_json

  let to_json =
    to_json K.to_json B.to_json

  let merge ~old:_ _ _ =
    failwith "Value.merge: TODO"

  let of_bytes str =
    IrminMisc.read bin_t (Bigstring.of_string str)

  let of_bytes_exn str =
    match of_bytes str with
    | None   -> raise (IrminBlob.Invalid str)
    | Some b -> b

  let key t =
    K.of_bigarray (IrminMisc.write bin_t t)

end

module Simple = S(IrminKey.SHA1)(IrminBlob.Simple)

module type STORE = sig
  type key
  type blob
  include IrminStore.AO with type key := key
                         and type value = (key, blob) t
  module Blob: IrminBlob.STORE
    with type key = key
     and type value = blob
  module Tree: IrminTree.STORE
    with type key = key
     and type blob = blob
  module Commit: IrminCommit.STORE
    with type key = key
  val blob: t -> Blob.t
  val tree: t -> Tree.t
  val commit: t -> Commit.t
  module Key: IrminKey.S with type t = key
  module Value: S with type key = key and type blob = blob
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

  let contents t =
    S.contents t >>= fun cs ->
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
  (B: IrminBlob.S)
  (Store: IrminStore.AO with type key = K.t and type value = (K.t, B.t) t)
= struct

  type blob = B.t

  module Key = K
  module Value = S(K)(B)

  include Store

  module BS = Cast(Store)(struct
      type t = Store.value
      type cast = B.t
      let proj = function
        | Blob b -> Some b
        | _ -> None
      let inj b = Blob b
    end)

  module TS = Cast(Store)(struct
      type t = Store.value
      type cast = K.t IrminTree.t
      let proj = function
        | Tree t -> Some t
        | _ -> None
      let inj t = Tree t
    end)

  module CS = Cast(Store)(struct
      type t = Store.value
      type cast = K.t IrminCommit.t
      let proj = function
        | Commit c -> Some c
        | _ -> None
      let inj c = Commit c
    end)

  module Blob = IrminBlob.Make(K)(B)(BS)
  module Tree = IrminTree.Make(K)(B)(BS)(TS)
  module Commit = IrminCommit.Make(K)(B)(TS)(CS)

  let blob t = t
  let tree t = (t, t)
  let commit t = (t, t)

end

module Mux
  (K: IrminKey.S)
  (B: IrminBlob.S)
  (Blob: IrminStore.AO with type key = K.t and type value = B.t)
  (Tree: IrminStore.AO with type key = K.t and type value = K.t IrminTree.t)
  (Commit: IrminStore.AO with type key = K.t and type value = K.t IrminCommit.t)
= struct

  type blob = B.t
  type key = K.t
  type value = (K.t, B.t) t
  module Key = K
  module Blob = IrminBlob.Make(K)(B)(Blob)
  module Tree = IrminTree.Make(K)(B)(Blob)(Tree)
  module Commit = IrminCommit.Make(K)(B)(Tree)(Commit)
  module Value = S(K)(B)

  type t = {
    blob     : Blob.t;
    tree     : Tree.t;
    commit   : Commit.t;
  }

  let blob t = t.blob
  let tree t = t.tree
  let commit t = t.commit

  open Lwt

  let create () =
    Blob.create () >>= fun blob ->
    Tree.create () >>= fun tree ->
    Commit.create () >>= fun commit ->
    return { blob; tree; commit }

  (* XXX: ugly *)
  let read t key =
    Blob.read t.blob key >>= function
    | Some b -> return (Some (Blob b))
    | None   ->
      Tree.read t.tree key >>= function
      | Some t -> return (Some (Tree t))
      | None   ->
        Commit.read t.commit key >>= function
        | Some c -> return (Some (Commit c))
        | None   -> return_none

  let read_exn t key =
    read t key >>= function
    | Some v -> return v
    | None   -> fail Not_found

  let mem t key =
    read t key >>= function
    | None   -> return false
    | Some _ -> return true

  let add t = function
    | Blob b   -> Blob.add t.blob b
    | Tree tr  -> Tree.add t.tree tr
    | Commit c -> Commit.add t.commit c

  let list t key =
    Commit.list t.commit key

  let contents t =
    Blob.contents t.blob     >>= fun blobs ->
    Tree.contents t.tree     >>= fun trees ->
    Commit.contents t.commit >>= fun commits ->
    let all =
      List.map blobs ~f:(fun (k, b) -> k, Blob b)
      @ List.map trees ~f:(fun (k, t) -> k, Tree t)
      @ List.map commits ~f:(fun (k, c) -> k, Commit c) in
    return all

end
