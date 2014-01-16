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

module type S = sig
  type key
  type blob
  include IrminBlob.S with type key := key and type t = (key, blob) t
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

  let pretty = function
    | Blob b   -> Blob.pretty b
    | Tree t   -> Tree.pretty t
    | Commit r -> Commit.pretty r

  let of_json = function
    | `O json ->
      let mem = List.Assoc.mem json in
      let find = List.Assoc.find_exn json in
      begin match mem "blob", mem "tree", mem "commit" with
        | true , false, false -> Blob   (Blob.of_json (find "blob"))
        | false, true , false -> Tree   (Tree.of_json (find "tree"))
        | false, false, true  -> Commit (Commit.of_json (find "commit"))
        | _ -> Mstruct.parse_error "Irmin.Dump.Value.of_json: invalid value (1)"
      end
    | _ -> Mstruct.parse_error "Irmin.VDump.Value.of_json: invalid value (2)"

  let to_json = function
    | Blob v   -> `O [ "blob"  , Blob.to_json v  ]
    | Tree t   -> `O [ "tree"  , Tree.to_json t  ]
    | Commit r -> `O [ "commit", Commit.to_json r]

  (* |----------|---------| *)
  (* | MAGIC(8) | PAYLOAD | *)
  (* |----------|---------| *)

  let header = "IRMIN00"

  let sizeof t =
    String.length header +
    match t with
    | Blob v   -> Blob.sizeof v
    | Tree t   -> Tree.sizeof t
    | Commit r -> Commit.sizeof r

  let get buf =
    L.debugf "get";
    let h = Mstruct.get_string buf (String.length header) in
    if String.(h = header) then
      (* XXX: very fragile *)
      match Mstruct.pick_string buf 1 with
      | Some "B" -> (match Blob.get buf   with None -> None | Some v -> Some (Blob v))
      | Some "T" -> (match Tree.get buf   with None -> None | Some t -> Some (Tree t))
      | Some "C" -> (match Commit.get buf with None -> None | Some r -> Some (Commit r))
      | Some x   -> L.debugf "pick: %s" x; None
      | None     -> L.debugf "pick: None"; None
    else
      None

  let set buf t =
    L.debug (lazy "set");
    Mstruct.set_string buf header;
    match t with
    | Blob v   -> Blob.set buf v
    | Tree t   -> Tree.set buf t
    | Commit r -> Commit.set buf r

  let merge ~old:_ _ _ =
    failwith "Value.merge: TODO"

  let of_bytes str =
    get (Mstruct.of_string str)

  let of_bytes_exn str =
    match of_bytes str with
    | None   -> raise (IrminBlob.Invalid str)
    | Some b -> b

  let key t =
    let n = sizeof t in
    let buf = Mstruct.create n in
    set buf t;
    K.of_bigarray (Mstruct.to_bigarray buf)

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
  (B: IrminBlob.S with type key = K.t)
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
  (B: IrminBlob.S with type key = K.t)
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
