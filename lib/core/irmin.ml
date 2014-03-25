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
open Lwt

module LogMake = Log.Make
module Log = LogMake(struct let section = "IRMIN" end)

module type S = sig
  type value
  module Internal: IrminValue.STORE with type blob = value
  include IrminStore.S with type key      = string list
                        and type value   := value
                        and type snapshot = Internal.key
                        and type dump     = (Internal.key, value) IrminDump.t
  val output: t -> string -> unit Lwt.t
  module Reference: IrminReference.STORE with type value = Internal.key
  val internal: t -> Internal.t
  val reference: t -> Reference.t
  val branch: t -> Reference.key
  module Key: IrminKey.S with type t = key
  module Value: IrminBlob.S with type t = value
  module Snapshot: IrminKey.S with type t = snapshot
  module Dump: IrminDump.S with type key = Internal.key and type blob = value
end


let date_hook =
  let c = ref 0. in
  ref (fun () -> c := Float.add !c 1.; !c)

let set_date_hook f =
  date_hook := f

let origin_hook =
  let r = string_of_int (Random.int 1024) in
  ref (fun () -> r)

let set_origin_hook f =
  origin_hook := f

module Make
    (K : IrminKey.S)
    (B : IrminBlob.S)
    (R : IrminReference.S)
    (Internal : IrminValue.STORE with type key = K.t and type blob = B.t)
    (Reference: IrminReference.STORE with type key = R.t and type value = K.t)
= struct

  module Internal = Internal
  module Reference = Reference
  module Key = IrminPath
  module Value = B
  module Blob = Internal.Blob
  module Node = Internal.Node
  module Commit = Internal.Commit
  module Dump = IrminDump.S(K)(B)
  module Snapshot = Internal.Key

  type snapshot = K.t
  type key = IrminPath.t
  type value = B.t
  type dump = Dump.t

  type watch = key * (key -> K.t -> unit)

  type t = {
    vals  : Internal.t;
    refs  : Reference.t;
    branch: R.t;
  }

  let internal t = t.vals
  let reference t = t.refs
  let branch t = t.branch

  let co = Internal.commit
  let no = Internal.node
  let bl = Internal.blob

  let create () =
    Internal.create () >>= fun vals ->
    Reference.create () >>= fun refs ->
    let branch = R.master in
    return { vals; refs; branch }

  let read_head_commit t =
    Reference.read t.refs t.branch >>= function
    | None   -> return_none
    | Some k -> Commit.read (co t.vals) k

  let read_node t = function
    | None       -> return IrminNode.empty
    | Some commit ->
      match Commit.node (co t.vals) commit with
      | None      -> return IrminNode.empty
      | Some node -> node

  let read_head_node t =
    read_head_commit t >>=
    read_node t

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let update_node t path fn =
    read_head_commit t >>= fun commit ->
    read_node t commit >>= fun old_node ->
    fn old_node >>= fun node ->
    if old_node = node then return_unit
    else (
      let parents = parents_of_commit commit in
      let date = !date_hook () in
      let origin = !origin_hook () in
      Commit.commit (co t.vals) ~date ~origin ~node ~parents >>= fun key ->
      Reference.update t.refs t.branch key
    )

  let read_node fn t path =
    read_head_node t >>= fun node ->
    fn (no t.vals) node path

  let read =
    read_node Node.find

  let update t path blob =
    read t path >>= fun old_v ->
    update_node t path (fun node ->
        Node.update (no t.vals) node path blob
      )
  let remove t path =
    update_node t path (fun node ->
        Node.remove (no t.vals) node path
      )

  let read_exn =
    read_node Node.find_exn

  let mem =
    read_node Node.valid

  let snapshot t =
    Reference.read_exn t.refs t.branch

  let revert t r =
    Reference.update t.refs t.branch r

  (* Return the subpaths. *)
  let list t path =
    read_head_node t >>= fun node ->
    Node.sub (no t.vals) node path >>= function
    | None
    | Some (IrminNode.Leaf _) -> return_nil
    | Some (IrminNode.Node c) ->
      let paths = List.map ~f:(fun (c,_) -> path @ [c]) c in
      return paths

  let contents t =
    read_head_node t >>= fun node ->
    let rec aux seen = function
      | []       -> return (List.sort compare seen)
      | path::tl ->
        list t path >>= fun childs ->
        let todo = childs @ tl in
        Node.find (no t.vals) node path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo in
    begin Node.find (no t.vals) node [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end
    >>= fun init ->
    list t [] >>= aux init

  module Graph = IrminGraph.Make(K)

  let output t name =
    Log.debugf "output %s" name;
    Blob.contents (bl t.vals)   >>= fun blobs   ->
    Node.contents (no t.vals)   >>= fun nodes   ->
    Commit.contents (co t.vals) >>= fun commits ->
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let label k =
      `Label (K.to_string k) in
    let label_of_blob k v =
      let k = K.to_string k in
      let v =
        let s = B.to_string v in
        let s =
          if String.length s <= 10 then s
          else String.sub s 0 10 in
        let s =
          if IrminMisc.is_valid_utf8 s then s
          else IrminMisc.hex_encode s in
        s in
      `Label (Printf.sprintf "%s | %s" k v) in
    List.iter ~f:(fun (k, b) ->
        add_vertex (`Blob k) [`Shape `Record; label_of_blob k b]
      ) blobs;
    List.iter ~f:(fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label k];
        match t with
        | IrminNode.Leaf v  -> add_edge (`Node k) [`Style `Dotted] (`Blob v)
        | IrminNode.Node ts ->
          List.iter ~f:(fun (l,c) ->
              add_edge (`Node k) [`Style `Solid; `Label l] (`Node c)
            ) ts
      ) nodes;
    List.iter ~f:(fun (k, r) ->
        add_vertex (`Commit k) [`Shape `Box; `Style `Bold; label k];
        List.iter ~f:(fun p ->
            add_edge (`Commit k) [`Style `Bold] (`Commit p)
          ) r.IrminCommit.parents;
        match r.IrminCommit.node with
        | None      -> ()
        | Some node -> add_edge (`Commit k) [`Style `Dashed] (`Node node)
      ) commits;
    (* XXX: this is not Xen-friendly *)
    Out_channel.with_file (name ^ ".dot") ~f:(fun oc ->
        Graph.output (Format.formatter_of_out_channel oc) !vertex !edges name;
      );
    let _ = Sys.command (Printf.sprintf "dot -Tpng %s.dot -o%s.png" name name) in
    return_unit

  let watch t path =
    Log.infof "Adding a watch on %s" (IrminPath.to_string path);
    let stream = Reference.watch t.refs t.branch in
    IrminMisc.lift_stream (
      read_node Node.sub t path >>= fun node ->
      let old_node = ref node in
      let stream = Lwt_stream.filter_map_s (fun key ->
          Log.debugf "watch: %s" (Snapshot.to_string key);
          Commit.read_exn (co t.vals) key >>= fun commit ->
          begin match Commit.node (co t.vals) commit with
            | None      -> return IrminNode.empty
            | Some node -> node
          end >>= fun node ->
          Node.sub (no t.vals) node path >>= fun node ->
          if node = !old_node then return_none
          else (
            old_node := node;
            return (Some (path, key))
          )
        ) stream in
      return stream
    )

  module Log = LogMake(struct let section ="DUMP" end)

  (* XXX: can be improved quite a lot *)
  let export t roots =
    Log.debugf "export root=%s" (IrminMisc.pretty_list K.to_string roots);
    output t "export" >>= fun () ->
    let contents = Internal.Key.Table.create () in
    let add k v = Hashtbl.add_multi contents k v in
    Reference.read t.refs t.branch >>= function
    | None        -> return_nil
    | Some commit ->
      begin
        if roots = [] then Commit.list (co t.vals) commit
        else
          let pred = function
            | `Commit k ->
              Commit.read_exn (co t.vals) k >>= fun r ->
              return (IrminGraph.of_commits r.IrminCommit.parents)
            | _ -> return_nil in
          let min = IrminGraph.of_commits roots in
          let max = IrminGraph.of_commits [commit] in
          Graph.closure pred ~min ~max >>= fun g ->
          let commits = IrminGraph.to_commits (Graph.vertex g) in
          return commits
      end
      >>= fun commits ->
      Log.debugf "export COMMITS=%s" (IrminMisc.pretty_list K.to_string commits);
      Lwt_list.fold_left_s (fun set key ->
          Commit.read_exn (co t.vals) key >>= fun commit ->
          add key (IrminValue.Commit commit);
          match commit.IrminCommit.node with
          | None      -> return set
          | Some node ->
            Node.list (no t.vals) node >>= fun nodes ->
            return (Set.union set (K.Set.of_list nodes))
        ) K.Set.empty commits
      >>= fun nodes ->
      let nodes = Set.elements nodes in
      Log.debugf "export NODES=%s" (IrminMisc.pretty_list K.to_string nodes);
      Lwt_list.fold_left_s (fun set key ->
          Node.read_exn (no t.vals) key >>= fun node ->
          add key (IrminValue.Node node);
          match node with
          | IrminNode.Node _    -> return set
          | IrminNode.Leaf blob ->
            Blob.list (bl t.vals) blob >>= fun blobs ->
            return (Set.union set (K.Set.of_list blobs))
        ) K.Set.empty nodes
      >>= fun blobs ->
      let blobs = Set.elements blobs in
      Log.debugf "export BLOBS=%s" (IrminMisc.pretty_list K.to_string blobs);
      Lwt_list.iter_p (fun key ->
          Blob.read_exn (bl t.vals) key >>= fun blob ->
          add key (IrminValue.Blob blob);
          return_unit
        ) blobs
      >>= fun () ->
      let list = Hashtbl.fold ~f:(fun ~key:k ~data init ->
          List.fold_left ~f:(fun acc v -> (k, v) :: acc) ~init data
        ) ~init:[] contents in
      return list

  exception Errors of (Internal.key * Internal.key * string) list

  let import t list =
    Log.debugf "import %s" (IrminMisc.pretty_list K.to_string (List.map ~f:fst list));
    let errors = ref [] in
    let check msg k1 k2 =
      if k1 <> k2 then errors := (k1, k2, msg) :: !errors;
      return_unit
    in
    (* Import blob first *)
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Blob x -> Blob.add (bl t.vals) x   >>= check "value" k
        | _ -> return_unit
      ) list >>= fun () ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Node x -> Node.add (no t.vals) x   >>= check "node" k
        | _ -> return_unit
      ) list >>= fun () ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Commit x -> Commit.add (co t.vals) x >>= check "commit" k
        | _ -> return_unit
      ) list >>= fun () ->
    if !errors = [] then return_unit
    else (
      let aux (expected, got, n) =
        Printf.sprintf
          "[expected %s (%s), got %s]"
          (K.to_string expected) n
          (K.to_string got) in
      Log.debugf "The following keys are invalid: %s"
        (IrminMisc.pretty_list aux !errors);
      fail (Errors !errors)
    )
end

module type SHA1 = S
  with type Internal.key = IrminKey.SHA1.t
   and type Reference.key = IrminReference.String.t

module type STRING = SHA1 with type value = IrminBlob.String.t

module String (AO: IrminStore.AO_BINARY) (RW: IrminStore.RW_BINARY) = struct

  module K = IrminKey.SHA1
  module B = IrminBlob.String
  module R = IrminReference.String
  module V = IrminValue.S(K)(B)

  module AO = IrminStore.AO_MAKER(AO)
  module RW = IrminStore.RW_MAKER(RW)

  module Val = IrminValue.Make(K)(B)(AO(K)(V))
  module Ref = IrminReference.Make(R)(K)(RW(R)(K))

  include Make (K)(B)(R)(Val)(Ref)

end

module type JSON = SHA1 with type value = IrminBlob.JSON.t
(** Signature for SHA1 to string stores. *)

module JSON (AO: IrminStore.AO_BINARY) (RW: IrminStore.RW_BINARY) = struct

  module K = IrminKey.SHA1
  module B = IrminBlob.JSON
  module R = IrminReference.String
  module V = IrminValue.S(K)(B)

  module AO = IrminStore.AO_MAKER(AO)
  module RW = IrminStore.RW_MAKER(RW)

  module Val = IrminValue.Make(K)(B)(AO(K)(V))
  module Ref = IrminReference.Make(R)(K)(RW(R)(K))

  include Make (K)(B)(R)(Val)(Ref)

end
