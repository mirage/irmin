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
  module Tree = Internal.Tree
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
  let tr = Internal.tree
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

  let read_tree t = function
    | None       -> return IrminTree.empty
    | Some commit ->
      match Commit.tree (co t.vals) commit with
      | None      -> return IrminTree.empty
      | Some tree -> tree

  let read_head_tree t =
    read_head_commit t >>=
    read_tree t

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let update_tree t path fn =
    read_head_commit t >>= fun commit ->
    read_tree t commit >>= fun old_tree ->
    fn old_tree >>= fun tree ->
    if old_tree = tree then return_unit
    else (
      let parents = parents_of_commit commit in
      Commit.commit (co t.vals) ~tree ~parents >>= fun key ->
      Reference.update t.refs t.branch key
    )

  let watches: watch list ref = ref []

  let dump_watches () =
    List.iter ~f:(fun (path, _) ->
        Log.debugf "watch: %s" (IrminPath.to_string path)
      ) !watches

  let fire path (p, _) =
    if p = path then true
    else
      let rec aux = function
        | _   , []   -> true
        | []  , _    -> false
        | a::b, x::y -> a=x && aux (b, y) in
      aux (path, p)

  let read_tree fn t path =
    read_head_tree t >>= fun tree ->
    fn (tr t.vals) tree path

  let read =
    read_tree Tree.find

  let update t path blob =
    dump_watches ();
    read t path >>= fun old_v ->
    update_tree t path (fun tree ->
        Tree.update (tr t.vals) tree path blob
      ) >>= fun () ->
    read t path >>= fun new_v ->
    let ws =
      if old_v = new_v then (
        let p = function
          | None   -> "<none>"
          | Some v -> B.to_string v in
        Log.infof "old=%s new=%s" (p old_v) (p new_v);
        []
      ) else List.filter ~f:(fire path) !watches in
    if ws = [] then return_unit
    else (
      Reference.read_exn t.refs t.branch >>= fun rev ->
      List.iter ~f:(fun (_, f) ->
          Log.infof "fire %s" (IrminPath.to_string path);
          f path rev
        ) ws;
      return_unit
    )

  let remove t path =
    update_tree t path (fun tree ->
        Tree.remove (tr t.vals) tree path
      )

  let read_exn =
    read_tree Tree.find_exn

  let mem =
    read_tree Tree.valid

  let snapshot t =
    Reference.read_exn t.refs t.branch

  let revert t r =
    Reference.update t.refs t.branch r

  (* Return the subpaths. *)
  let list t path =
    read_head_tree t >>= fun tree ->
    Tree.sub (tr t.vals) tree path >>= function
    | None
    | Some (IrminTree.Leaf _) -> return_nil
    | Some (IrminTree.Node c) ->
      let paths = List.map ~f:(fun (c,_) -> path @ [c]) c in
      return paths

  let contents t =
    read_head_tree t >>= fun tree ->
    let rec aux seen = function
      | []       -> return (List.sort compare seen)
      | path::tl ->
        list t path >>= fun childs ->
        let todo = childs @ tl in
        Tree.find (tr t.vals) tree path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo in
    begin Tree.find (tr t.vals) tree [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end
    >>= fun init ->
    list t [] >>= aux init

  module Graph = IrminGraph.Make(K)

  let output t name =
    Log.debugf "output %s" name;
    Blob.contents (bl t.vals)   >>= fun blobs   ->
    Tree.contents (tr t.vals)   >>= fun trees   ->
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
      let v = B.to_string v in
      `Label (Printf.sprintf "%s | %s" k v) in
    List.iter ~f:(fun (k, b) ->
        add_vertex k [`Shape `Record; label_of_blob k b]
      ) blobs;
    List.iter ~f:(fun (k, t) ->
        add_vertex k [`Shape `Box; `Style `Dotted; label k];
        match t with
        | IrminTree.Leaf v  -> add_edge k [`Style `Dotted] v
        | IrminTree.Node ts ->
          List.iter ~f:(fun (l,c) ->
              add_edge k [`Style `Solid; `Label l] c
            ) ts
      ) trees;
    List.iter ~f:(fun (k, r) ->
        add_vertex k [`Shape `Box; `Style `Bold; label k];
        List.iter ~f:(fun p ->
            add_edge k [`Style `Bold] p
          ) r.IrminCommit.parents;
        match r.IrminCommit.tree with
        | None      -> ()
        | Some tree -> add_edge k [`Style `Dashed] tree
      ) commits;
    (* XXX: this is not Xen-friendly *)
    Out_channel.with_file (name ^ ".dot") ~f:(fun oc ->
        Graph.output (Format.formatter_of_out_channel oc) !vertex !edges name;
      );
    let _ = Sys.command (Printf.sprintf "dot -Tpng %s.dot -o%s.png" name name) in
    return_unit

  let watch t path =
    Log.infof "Adding a watch on %s" (IrminPath.to_string path);
    let stream, push, _ = Lwt_stream.create_with_reference () in
    let callback path rev =
      push (Some (path, rev)) in
    watches := (path, callback) :: !watches;
    stream

  module Log = LogMake(struct let section ="DUMP" end)

  (* XXX: can be improved quite a lot *)
  let export t roots =
    Log.debugf "export root=%s" (IrminMisc.pretty_list K.to_string roots);
    output t "export" >>= fun () ->
    let contents = Internal.Key.Table.create () in
    let add k v =
      Hashtbl.add_multi contents k v in
    Reference.read t.refs t.branch >>= function
    | None        -> return_nil
    | Some commit ->
      begin
        if roots = [] then Commit.list (co t.vals) commit
        else
          let pred k =
            Commit.read_exn (co t.vals) k >>= fun r ->
            return r.IrminCommit.parents in
          Graph.closure pred ~min:roots ~max:[commit] >>= fun g ->
          return (Graph.vertex g)
      end
      >>= fun commits ->
      Lwt_list.fold_left_s (fun set key ->
          Commit.read_exn (co t.vals) key >>= fun commit ->
          add key (IrminValue.Commit commit);
          match commit.IrminCommit.tree with
          | None      -> return set
          | Some tree ->
            Tree.list (tr t.vals) tree >>= fun trees ->
            return (Set.union set (K.Set.of_list trees))
        ) K.Set.empty commits
      >>= fun trees ->
      let trees = Set.elements trees in
      Log.debugf "export TREES=%s" (IrminMisc.pretty_list K.to_string trees);
      Lwt_list.fold_left_s (fun set key ->
          Tree.read_exn (tr t.vals) key >>= fun tree ->
          add key (IrminValue.Tree tree);
          match tree with
          | IrminTree.Node _    -> return set
          | IrminTree.Leaf blob ->
            Blob.list (bl t.vals) blob >>= fun blobs ->
            return (Set.union set (K.Set.of_list blobs))
        ) K.Set.empty trees
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
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Blob x   -> Blob.add (bl t.vals) x   >>= check "value" k
        | IrminValue.Tree x   -> Tree.add (tr t.vals) x   >>= check "tree" k
        | IrminValue.Commit x -> Commit.add (co t.vals) x >>= check "commit" k
      ) list
    >>= fun () ->
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

module type SIMPLE = S
  with type Internal.key = IrminKey.SHA1.t
   and type value = IrminBlob.Simple.t
   and type Reference.key = IrminReference.Simple.t

module Simple (AO: IrminStore.AO_BINARY) (RW: IrminStore.RW_BINARY) = struct

  module K = IrminKey.SHA1
  module B = IrminBlob.Simple
  module R = IrminReference.Simple
  module V = IrminValue.S(K)(B)

  module AO = IrminStore.AO_MAKER(AO)
  module RW = IrminStore.RW_MAKER(RW)

  module Val = IrminValue.Make(K)(B)(AO(K)(V))
  module Ref = IrminReference.Make(R)(K)(RW(R)(K))

  include Make (K)(B)(R)(Val)(Ref)

end
