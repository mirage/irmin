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
open Lwt

module XLog = Log
module Log = XLog.Make(struct let section = "IRMIN" end)

module type S = sig
  type value
  module Internal: IrminValue.STORE with type contents = value
  module Reference: IrminReference.STORE with type value = Internal.key
  include IrminStore.S with type key      = string list
                        and type value   := value
                        and type snapshot = Internal.key
                        and type dump     = (Internal.key, value) IrminDump.t
                        and type branch   = Reference.key
  val output: t -> string -> unit Lwt.t
  val internal: t -> Internal.t
  val reference: t -> Reference.t
  val branch: t -> branch -> t Lwt.t
  val merge: t -> into:t -> unit Lwt.t
  module Key: IrminKey.S with type t = key
  module Value: IrminContents.S with type t = value
  module Snapshot: IrminKey.S with type t = snapshot
  module Dump: IrminDump.S with type key = Internal.key and type contents = value
  module View: IrminView.S with type value := value
  val updates: t -> key -> View.t -> unit Lwt.t
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
    (C : IrminContents.S)
    (R : IrminReference.S)
    (Internal : IrminValue.STORE with type key = K.t and type contents = C.t)
    (Reference: IrminReference.STORE with type key = R.t and type value = K.t)
= struct

  module Internal = Internal
  module Reference = Reference
  module Key = IrminPath
  module Value = C
  module Contents = Internal.Contents
  module Node = Internal.Node
  module Commit = Internal.Commit
  module Dump = IrminDump.S(K)(C)
  module Snapshot = Internal.Key

  type snapshot = K.t
  type key = IrminPath.t
  type value = C.t
  type dump = Dump.t

  type watch = key * (key -> K.t -> unit)

  type t = {
    vals  : Internal.t;
    refs  : Reference.t;
    branch: R.t;
  }

  let internal t = t.vals
  let reference t = t.refs

  let co = Internal.commit
  let no = Internal.node
  let bl = Internal.contents

  let create () =
    Internal.create ()  >>= fun vals ->
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

  let update_node t fn =
    read_head_commit t >>= fun commit ->
    read_node t commit >>= fun old_node ->
    fn old_node >>= fun node ->
    if IrminNode.equal K.equal old_node node then return_unit
    else (
      let parents = parents_of_commit commit in
      let date = !date_hook () in
      let origin = !origin_hook () in
      Commit.commit (co t.vals) ~date ~origin ~node ~parents >>= fun (key, _) ->
      Reference.update t.refs t.branch key
    )

  let read_node fn t path =
    read_head_node t >>= fun node ->
    fn (no t.vals) node path

  let read =
    read_node Node.find

  let update t path contents =
    Log.debugf "update %s" (IrminPath.to_string path);
    update_node t (fun node ->
        Node.update (no t.vals) node path contents
      )

  let remove t path =
    update_node t (fun node ->
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

  let string_of_set s =
    IrminMisc.pretty_list K.to_string (K.Set.to_list s)

  (* XXX: is this correct ? *)
  let find_common_ancestor t c1 c2 =
    let rec aux (seen1, todo1) (seen2, todo2) =
      Log.debugf "seen1=%s todo1=%s" (string_of_set seen1) (string_of_set todo1);
      Log.debugf "seen2=%s todo2=%s" (string_of_set seen2) (string_of_set todo2);
      let seen1' = K.Set.union seen1 todo1 in
      let seen2' = K.Set.union seen2 todo2 in
      match K.Set.to_list (K.Set.inter seen1' seen2') with
      | []  ->
        (* Compute the immediate parents *)
        let parents todo =
          let parents_of_commit seen c =
            Commit.read_exn (co t.vals) c >>= fun v ->
            let parents = K.Set.of_list v.IrminCommit.parents in
            return (K.Set.diff parents seen) in
          Lwt_list.fold_left_s parents_of_commit todo (K.Set.to_list todo)
        in
        parents todo1 >>= fun todo1' ->
        parents todo2 >>= fun todo2' ->
        aux (seen1', todo1') (seen2', todo2')
      | [r] ->
        Log.debugf "common ancestor: %s" (K.to_string r);
        return r
      | rs  ->
        Log.debugf "Error: Multiple common ancestor: %s."
          (IrminMisc.pretty_list K.to_string rs);
        fail IrminMerge.Conflict in
    aux
      (K.Set.empty, K.Set.singleton c1)
      (K.Set.empty, K.Set.singleton c2)

  let merge_snapshot t c1 c2 =
    let date = !date_hook () in
    let origin = !origin_hook () in
    find_common_ancestor t c1 c2 >>= fun old ->
    IrminMerge.merge
      (Commit.merge (co t.vals) ~date ~origin)
      ~old c1 c2

  (* Return the subpaths. *)
  let list t paths =
    Log.debugf "list";
    let one path =
      read_head_node t >>= fun node ->
      Node.sub (no t.vals) node path >>= function
      | None      -> return_nil
      | Some node ->
        let c = Node.succ (no t.vals) node in
        let c = Map.keys c in
        let paths = List.map ~f:(fun c -> path @ [c]) c in
        return paths in
    Lwt_list.fold_left_s (fun set p ->
        one p >>= fun paths ->
        let paths = IrminPath.Set.of_list paths in
        return (IrminPath.Set.union set paths)
      ) IrminPath.Set.empty paths
    >>= fun paths ->
    return (IrminPath.Set.to_list paths)

  let dump t =
    Log.debugf "dump";
    read_head_node t >>= fun node ->
    let rec aux seen = function
      | []       -> return (List.sort compare seen)
      | path::tl ->
        list t [path] >>= fun childs ->
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

  module Graph = IrminGraph.Make(K)(R)

  let output t name =
    Log.debugf "output %s" name;
    Contents.dump (bl t.vals) >>= fun contents ->
    Node.dump (no t.vals)     >>= fun nodes    ->
    Commit.dump (co t.vals)   >>= fun commits  ->
    Reference.dump t.refs     >>= fun refs     ->
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let string_of_key k =
      let s = K.to_string k in
      if String.length s <= 8 then s else String.sub s 0 8 in
    let string_of_contents s =
      let s =
        if String.length s <= 10 then s
        else String.sub s 0 10 in
      let s =
          if IrminMisc.is_valid_utf8 s then s
          else IrminMisc.hex_encode s in
      s in
    let label k =
      `Label (string_of_key k) in
    let label_of_path l =
      `Label (string_of_contents l) in
    let label_of_contents k v =
      let k = string_of_key k in
      let v = string_of_contents (C.to_string v) in
      `Label (Printf.sprintf "%s | %s" k (String.escaped v)) in
    let leafs = List.map ~f:(fun (k,_) ->
        (k, IrminNode.leaf k)
      ) contents in
    let nodes = leafs @ nodes in
    List.iter ~f:(fun (k, b) ->
        add_vertex (`Contents k) [`Shape `Record; label_of_contents k b];
      ) contents;
    List.iter ~f:(fun (k, t) ->
        add_vertex (`Node k) [`Shape `Box; `Style `Dotted; label k];
        begin match t.IrminNode.contents with
          | None    -> ()
          | Some v  -> add_edge (`Node k) [`Style `Dotted] (`Contents v)
        end;
        Map.iter ~f:(fun ~key:l ~data:c ->
            add_edge (`Node k) [`Style `Solid; label_of_path l] (`Node c)
          ) t.IrminNode.succ
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
    List.iter ~f:(fun (r,k) ->
        add_vertex (`Ref r) [`Shape `Plaintext; `Label (R.to_string r); `Style `Filled];
        let exists l = List.exists ~f:(fun (kk,_) -> kk=k) l in
        if exists commits then
          add_edge (`Ref r) [`Style `Bold] (`Commit k);
        if exists nodes then
          add_edge (`Ref r) [`Style `Bold] (`Node k);
      ) refs;
    (* XXX: this is not Xen-friendly *)
    Out_channel.with_file (name ^ ".dot") ~f:(fun oc ->
        Graph.output (Format.formatter_of_out_channel oc) !vertex !edges name;
      );
    let cmd = Printf.sprintf "dot -Tpng %s.dot -o%s.png" name name in
    let i = Sys.command cmd in
    if i <> 0 then Log.errorf "The %s.dot is corrupted" name;
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

  module Log = XLog.Make(struct let section ="DUMP" end)

  (* XXX: can be improved quite a lot *)
  let export t roots =
    Log.debugf "export root=%s" (IrminMisc.pretty_list K.to_string roots);
    let table = Internal.Key.Table.create () in
    let add k v = Hashtbl.add_multi table k v in
    Reference.read t.refs t.branch >>= function
    | None        -> return { IrminDump.head = None; store = [] }
    | Some commit ->
      let head = Some commit in
      begin
        if roots = [] then Commit.list (co t.vals) [commit]
        else
          let pred = function
            | `Commit k -> Commit.read_exn (co t.vals) k >>= fun c -> return (IrminCommit.edges c)
            | _         -> return_nil in
          let min = IrminGraph.of_commits roots in
          let max = IrminGraph.of_commits [commit] in
          Graph.closure pred ~min ~max >>= fun g ->
          let commits = IrminGraph.to_commits (Graph.vertex g) in
          return commits
      end >>= fun commits ->
      Log.debugf "export COMMITS=%s" (IrminMisc.pretty_list K.to_string commits);
      let nodes = ref K.Set.empty in
      Lwt_list.iter_p (fun key ->
          Commit.read_exn (co t.vals) key >>= fun commit ->
          add key (IrminValue.Commit commit);
          match commit.IrminCommit.node with
          | None   -> return_unit
          | Some k -> nodes := Set.add !nodes k; return_unit
        ) commits >>= fun () ->
      let nodes = !nodes in
      Node.list (no t.vals) (K.Set.to_list nodes) >>= fun nodes ->
      Log.debugf "export NODES=%s" (IrminMisc.pretty_list K.to_string nodes);
      let contents = ref K.Set.empty in
      Lwt_list.iter_p (fun key ->
          Node.read_exn (no t.vals) key >>= fun node ->
          add key (IrminValue.Node node);
          match node.IrminNode.contents with
          | None   -> return_unit
          | Some k -> contents := Set.add !contents k; return_unit
        ) nodes >>= fun () ->
      let contents = !contents in
      Contents.list (bl t.vals) (K.Set.to_list contents) >>= fun contents ->
      Log.debugf "export CONTENTS=%s" (IrminMisc.pretty_list K.to_string contents);
      Lwt_list.iter_p (fun k ->
          Contents.read_exn (bl t.vals) k >>= fun b ->
          add k (IrminValue.Contents b);
          return_unit
        ) contents >>= fun () ->
      let store = Hashtbl.fold ~f:(fun ~key:k ~data init ->
          List.fold_left ~f:(fun acc v -> (k, v) :: acc) ~init data
        ) ~init:[] table in
      return { IrminDump.head; store }

  exception Errors of (Internal.key * Internal.key * string) list

  let import t branch { IrminDump.head; store } =
    Log.debugf "import %d" (List.length store);
    let errors = ref [] in
    let check msg k1 k2 =
      if k1 <> k2 then errors := (k1, k2, msg) :: !errors;
      return_unit
    in
    (* Import contents first *)
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Contents x -> Contents.add (bl t.vals) x >>= check "value" k
        | _ -> return_unit
      ) store >>= fun () ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Node x -> Node.add (no t.vals) x >>= check "node" k
        | _ -> return_unit
      ) store >>= fun () ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminValue.Commit x -> Commit.add (co t.vals) x >>= check "commit" k
        | _ -> return_unit
      ) store >>= fun () ->
    if !errors = [] then
      match head with
      | None   -> return_unit
      | Some h -> Reference.update t.refs branch h
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

  type branch = Reference.key

  let branch t branch =
    begin Reference.read t.refs t.branch >>= function
    | None   -> Reference.remove t.refs branch
    | Some c -> Reference.update t.refs branch c
    end >>= fun () ->
    return { t with branch }

  let merge t1 ~into:t2 =
    Reference.read_exn t1.refs t1.branch  >>= fun c1  ->
    Reference.read_exn t2.refs t2.branch  >>= fun c2  ->
    merge_snapshot t1 c1 c2               >>= fun c3  ->
    Reference.update t1.refs t1.branch c3

  module View = IrminView.Make(Contents)

  let updates t path tree =
    failwith "TODO"

end

type ('key, 'value, 'ref) t =
  (module S with type Internal.key = 'key
             and type value = 'value
             and type Reference.key = 'ref)

module Binary
    (K : IrminKey.S)
    (C : IrminContents.S)
    (R : IrminReference.S)
    (AO: IrminStore.AO_BINARY)
    (RW: IrminStore.RW_BINARY) =
struct

  module V = IrminValue.S(K)(C)

  module AO = IrminStore.AO_MAKER(AO)
  module RW = IrminStore.RW_MAKER(RW)

  module Val = IrminValue.Make(K)(C)(AO(K)(V))
  module Ref = IrminReference.Make(R)(K)(RW(R)(K))

  include Make (K)(C)(R)(Val)(Ref)

end
