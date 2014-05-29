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
open IrminMerge.OP

module Log = Log.Make(struct let section ="DUMP" end)

type origin = IrminOrigin.t

module type STORE = sig
  include IrminStore.RO
  type db
  val head: t -> key option
  val with_head: t -> key option -> t
  val empty: t
  val create: db -> key list -> t Lwt.t
  val update: db -> t -> unit Lwt.t
  val merge: db -> ?origin:origin -> t -> unit IrminMerge.result Lwt.t
  val merge_exn: db -> ?origin:origin -> t -> unit Lwt.t
  val output_file: string -> db ->  unit Lwt.t
  val output_buffer: Buffer.t -> db -> unit Lwt.t
  module Key: IrminKey.S with type t = key
  module Value: IrminIdent.S with type t = value
  include IrminIdent.S with type t := t
end

module Make (Store: IrminBranch.INTERNAL) = struct

  module K = Store.Block.Key
  module C = Store.Value
  module T = Store.Tag.Key

  module Tag = Store.Tag
  module Block = Store.Block
  module Commit = Block.Commit
  module Node = Block.Node
  module Contents = Block.Contents

  module Key = K
  module Value = Block.Value

  type db = Store.t

  type dump = {
    head : K.t option;
    store: (K.t, C.t) IrminBlock.t K.Map.t;
  } with sexp, compare

  include IrminIdent.Make(struct type t = dump with sexp, compare end)

  type key = Key.t

  type value = Value.t

  let empty = {
    head  = None;
    store = K.Map.empty;
  }

  let head t = t.head

  let with_head t head = { t with head }

  (* XXX: can be improved quite a lot *)
  let create t roots =
    Log.debugf "export root=%s" (IrminMisc.pretty_list K.to_string roots);
    let store = ref K.Map.empty in
    let add k v =
      store := K.Map.add !store k v in
    Store.head t >>= function
    | None        -> return empty
    | Some commit ->
      let head = Some commit in
      begin match roots with
        | [] -> Commit.list (Store.commit_t t) [commit]
        | _  ->
          let pred = function
            | `Commit k ->
              Commit.read_exn (Store.commit_t t) k >>= fun c ->
              return (IrminCommit.edges c)
            | _ -> return_nil in
          let min = IrminGraph.of_commits roots in
          let max = IrminGraph.of_commits [commit] in
          Store.Graph.closure pred ~min ~max >>= fun g ->
          let commits = IrminGraph.to_commits (Store.Graph.vertex g) in
          return commits
      end >>= fun commits ->
      Log.debugf "export COMMITS=%s" (IrminMisc.pretty_list K.to_string commits);
      let nodes = ref K.Set.empty in
      Lwt_list.iter_p (fun key ->
          Commit.read_exn (Store.commit_t t) key >>= fun commit ->
          add key (IrminBlock.Commit commit);
          match commit.IrminCommit.node with
          | None   -> return_unit
          | Some k -> nodes := K.Set.add !nodes k; return_unit
        ) commits >>= fun () ->
      let nodes = !nodes in
      Node.list (Store.node_t t) (K.Set.to_list nodes) >>= fun nodes ->
      Log.debugf "export NODES=%s" (IrminMisc.pretty_list K.to_string nodes);
      let contents = ref K.Set.empty in
      Lwt_list.iter_p (fun key ->
          Node.read_exn (Store.node_t t) key >>= fun node ->
          add key (IrminBlock.Node node);
          match node.IrminNode.contents with
          | None   -> return_unit
          | Some k -> contents := K.Set.add !contents k; return_unit
        ) nodes >>= fun () ->
      let contents = !contents in
      Contents.list (Store.contents_t t) (K.Set.to_list contents) >>= fun contents ->
      Log.debugf "export CONTENTS=%s" (IrminMisc.pretty_list K.to_string contents);
      Lwt_list.iter_p (fun k ->
          Contents.read_exn (Store.contents_t t) k >>= fun b ->
          add k (IrminBlock.Contents b);
          return_unit
        ) contents >>= fun () ->
      return { head; store = !store }

  let read t key =
    return (K.Map.find t.store key)

  let dump t =
    return (K.Map.to_alist t.store)

  let list t _ =
    (* XXX: filter the keys *)
    return (K.Map.keys t.store)

  let mem t key =
    return (K.Map.mem t.store key)

  let read_exn t key =
    return (K.Map.find_exn t.store key)

  exception Errors of (Block.key * Block.key * string) list

  let update_aux t s =
    Log.debugf "import %d" (K.Map.length s.store);
    let errors = ref [] in
    let check msg k1 k2 =
      if K.(k1 <> k2) then errors := (k1, k2, msg) :: !errors;
      return_unit
    in
    (* Import contents first *)
    dump s >>= fun store ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminBlock.Contents x -> Contents.add (Store.contents_t t) x >>= check "value" k
        | _ -> return_unit
      ) store >>= fun () ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminBlock.Node x -> Node.add (Store.node_t t) x >>= check "node" k
        | _ -> return_unit
      ) store >>= fun () ->
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | IrminBlock.Commit x -> Commit.add (Store.commit_t t) x >>= check "commit" k
        | _ -> return_unit
      ) store >>= fun () ->
    match !errors with
    | [] -> return_unit
    | _ :: _ ->
      let aux (expected, got, n) =
        Printf.sprintf
          "[expected %s (%s), got %s]"
          (K.to_string expected) n
          (K.to_string got) in
      Log.debugf "The following keys are invalid: %s"
        (IrminMisc.pretty_list aux !errors);
      fail (Errors !errors)

  let update t dump =
    update_aux t dump >>= fun () ->
    match dump.head with
    | None   -> return_unit
    | Some h -> Store.update_commit t h

  let merge t ?origin dump =
    let origin = match origin with
      | None   -> IrminOrigin.create "Merge pulled state."
      | Some o -> o in
    update_aux t dump >>= fun () ->
    match dump.head with
    | None   -> ok ()
    | Some h -> Store.merge_commit t ~origin h

  let merge_exn t ?origin dump =
    merge t ?origin dump >>=
    IrminMerge.exn

  let fprintf name t =
    Contents.dump (Store.contents_t t) >>= fun contents ->
    Node.dump (Store.node_t t)         >>= fun nodes    ->
    Commit.dump (Store.commit_t t)     >>= fun commits  ->
    Tag.dump (Store.tag_t t)           >>= fun tags     ->
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let string_of_key k =
      let s = K.to_string k in
      if Int.(String.length s <= 8) then s else String.sub s 0 8 in
    let string_of_contents s =
      let s =
        if Int.(String.length s <= 10) then s
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
        String.Map.iter ~f:(fun ~key:l ~data:c ->
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
        add_vertex (`Tag r) [`Shape `Plaintext; `Label (T.to_string r); `Style `Filled];
        let exists l = List.exists ~f:(fun (kk,_) -> K.(kk=k)) l in
        if exists commits then
          add_edge (`Tag r) [`Style `Bold] (`Commit k);
        if exists nodes then
          add_edge (`Tag r) [`Style `Bold] (`Node k);
      ) tags;
    (* XXX: this is not Xen-friendly *)
    return (fun ppf -> Store.Graph.output ppf !vertex !edges name)

  let output_file name t =
    Log.debugf "output %s" name;
    fprintf name t >>= fun fprintf ->
    let oc = Out_channel.create (name ^ ".dot") in
    fprintf (Format.formatter_of_out_channel oc);
    Out_channel.close oc;
    let cmd = Printf.sprintf "dot -Tpng %s.dot -o%s.png" name name in
    let i = Sys.command cmd in
    if Int.(i <> 0) then Log.errorf "The %s.dot is corrupted" name;
    return_unit

  let output_buffer buf t =
    fprintf "graph" t >>= fun fprintf ->
    let ppf = Format.formatter_of_buffer buf in
    fprintf ppf;
    return_unit

end
