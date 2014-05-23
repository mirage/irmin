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

module XLog = Log
module Log = XLog.Make(struct let section = "IRMIN" end)

exception Conflict of string

module type SNAPSHOTABLE = sig
  type db
  type path
  include IrminKey.S
  val create: db -> t Lwt.t
  val update: db -> t -> unit Lwt.t
  val merge: ?origin:IrminOrigin.t -> db -> t -> unit IrminMerge.result Lwt.t
  val merge_exn: ?origin:IrminOrigin.t -> db -> t -> unit Lwt.t
  val watch: db -> path -> (path * t) Lwt_stream.t
end

module type BRANCHABLE = sig
  type db
  include IrminReference.S
  val create: db -> t -> db option Lwt.t
  val create_force: db -> t -> db Lwt.t
  val mem: db -> t -> bool Lwt.t
  val list: db -> t list Lwt.t
  val current: db -> t
  val merge: ?origin:IrminOrigin.t -> db -> t -> unit IrminMerge.result Lwt.t
  val merge_exn: ?origin:IrminOrigin.t -> db -> t -> unit Lwt.t
end

module type DUMPABLE = sig
  type db
  type snapshot
  include IrminDump.S
  val create: db -> snapshot list -> t Lwt.t
  val update: db -> t -> unit Lwt.t
  val merge: ?origin:IrminOrigin.t -> db -> t -> unit IrminMerge.result Lwt.t
  val merge_exn: ?origin:IrminOrigin.t -> db -> t -> unit Lwt.t
  val output: db -> string -> unit Lwt.t
end

module type VIEWABLE = sig
  type db
  type path
  include IrminView.S
  val of_path: db -> path -> t Lwt.t
  val update_path: ?origin:IrminOrigin.t -> db -> path -> t -> unit Lwt.t
  val merge_path: ?origin:IrminOrigin.t -> db -> path -> t -> unit IrminMerge.result Lwt.t
  val merge_path_exn: ?origin:IrminOrigin.t -> db -> path -> t -> unit Lwt.t
end


module type S = sig
  type key = string list
  type value
  include IrminStore.RW with type key   := key
                         and type value := value
  module Block: IrminValue.STORE with type contents = value
  val block: t -> Block.t
  module Tag: IrminReference.STORE with type value = Block.key
  val tag: t -> Tag.t
  module Snapshot: SNAPSHOTABLE with type db   = t
                                 and type path = key
  module Branch: BRANCHABLE with type db = t
  module Dump: DUMPABLE with type db       = t
                         and type key      = Block.key
                         and type contents = Block.contents
                         and type snapshot = Snapshot.t
  module View: VIEWABLE with type node  = Block.key
                         and type value = value
  val create: ?branch:Branch.t -> unit -> t Lwt.t
  val update: ?origin:IrminOrigin.t -> t -> key -> value -> unit Lwt.t
  val remove: ?origin:IrminOrigin.t -> t -> key -> unit Lwt.t
  module Key: IrminKey.S with type t = key
  module Value: IrminContents.S with type t = value
end

type ('key, 'value, 'ref) t =
  (module S with type Block.key = 'key
             and type value     = 'value
             and type Tag.key   = 'ref)

module Make
    (K : IrminKey.S)
    (C : IrminContents.S)
    (R : IrminReference.S)
    (Block: IrminValue.STORE with type key = K.t and type contents = C.t)
    (Tag  : IrminReference.STORE with type key = R.t and type value = K.t)
= struct

  module Block = Block
  module Tag = Tag
  module Key = IrminPath
  module Value = C
  module Contents = Block.Contents
  module Node = Block.Node
  module Commit = Block.Commit

  type key = IrminPath.t
  type value = C.t
  type t = {
    block : Block.t;
    tag   : Tag.t;
    branch: R.t;
  }

  let block t = t.block
  let tag   t = t.tag

  let commit_t   t = Block.commit t.block
  let node_t     t = Block.node t.block
  let contents_t t = Block.contents t.block

  let create ?(branch=R.master) () =
    Block.create () >>= fun block ->
    Tag.create ()   >>= fun tag ->
    return { block; tag; branch }

  let read_head_commit t =
    Tag.read t.tag t.branch >>= function
    | None   -> return_none
    | Some k -> Commit.read (commit_t t) k

  let node_of_commit t c =
    match Commit.node (commit_t t) c with
    | None   -> return IrminNode.empty
    | Some n -> n

  let node_of_opt_commit t = function
    | None   -> return IrminNode.empty
    | Some c -> node_of_commit t c

  let read_head_node t =
    read_head_commit t >>=
    node_of_opt_commit t

  let parents_of_commit = function
    | None   -> []
    | Some r -> [r]

  let update_commit ~origin t fn commit =
    node_of_opt_commit t commit >>= fun old_node ->
    fn old_node                 >>= fun node ->
    if IrminNode.equal K.equal old_node node then return_unit
    else (
      let parents = parents_of_commit commit in
      Commit.commit (commit_t t) origin ~node ~parents >>= fun (key, _) ->
      (* XXX: the head might have changed since we started the operation *)
      Tag.update t.tag t.branch key
    )

  let update_node ~origin t fn =
    read_head_commit t >>=
    update_commit t ~origin fn

  let map_node fn t path =
    read_head_node t >>= fun node ->
    fn (node_t t) node path

  let read =
    map_node Node.find

  let update ?origin t path contents =
    let origin = match origin with
      | None   -> IrminOrigin.create "Update %s." (IrminPath.to_string path)
      | Some o -> o in
    Log.debugf "update %s" (IrminPath.to_string path);
    update_node t ~origin (fun n ->
        Node.update (node_t t) n path contents
      )

  let remove ?origin t path =
    let origin = match origin with
      | None   -> IrminOrigin.create "Remove %s." (IrminPath.to_string path)
      | Some o -> o in
    update_node t ~origin (fun n ->
        Node.remove (node_t t) n path
      )

  let read_exn =
    map_node Node.find_exn

  let mem =
    map_node Node.valid

  (* Return the subpaths. *)
  let list t paths =
    Log.debugf "list";
    let one path =
      read_head_node t >>= fun n ->
      Node.sub (node_t t) n path >>= function
      | None      -> return_nil
      | Some node ->
        let c = Node.succ (node_t t) node in
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
        Node.find (node_t t) node path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo in
    begin Node.find (node_t t) node [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end
    >>= fun init ->
    list t [[]] >>= aux init

  let find_common_ancestor t c1 c2 =
    let rec aux (seen1, todo1) (seen2, todo2) =
      let seen1' = K.Set.union seen1 todo1 in
      let seen2' = K.Set.union seen2 todo2 in
      match K.Set.to_list (K.Set.inter seen1' seen2') with
      | []  ->
        (* Compute the immediate parents *)
        let parents todo =
          let parents_of_commit seen c =
            Commit.read_exn (commit_t t) c >>= fun v ->
            let parents = K.Set.of_list v.IrminCommit.parents in
            return (K.Set.diff parents seen) in
          Lwt_list.fold_left_s parents_of_commit todo (K.Set.to_list todo)
        in
        parents todo1 >>= fun todo1' ->
        parents todo2 >>= fun todo2' ->
        aux (seen1', todo1') (seen2', todo2')
      | [r] -> ok r
      | rs  -> conflict "Multiple common ancestor: %s" (IrminMisc.pretty_list K.to_string rs) in
    aux
      (K.Set.empty, K.Set.singleton c1)
      (K.Set.empty, K.Set.singleton c2)

(* Merge two commits:
  - Search for a common ancestor
  - Perform a 3-way merge *)
  let merge_commits ?origin t c1 c2 =
    let origin = match origin with
      | None   -> IrminOrigin.create "Merge commits %s and %s"
                    (K.to_string c1) (K.to_string c2)
      | Some o -> o in
    find_common_ancestor t c1 c2 >>| fun old ->
    IrminMerge.merge (Commit.merge (commit_t t) origin) ~old c1 c2

  module Snapshot = struct

    type db = t

    type path = key

    let create t =
      Tag.read_exn t.tag t.branch

    let update t r =
      Tag.update t.tag t.branch r

    let merge ?origin t c1 =
      let origin = match origin with
        | None   -> IrminOrigin.create "Merge snapshot %s"
                      (K.to_string c1)
        | Some o -> o in
      Tag.read t.tag t.branch >>= function
      | None    -> Tag.update t.tag t.branch c1 >>= ok
      | Some c2 ->
        merge_commits ~origin t c1 c2 >>| fun c3 ->
        Tag.update t.tag t.branch c3   >>=
        ok

    let merge_exn ?origin t c1 =
      merge ?origin t c1 >>=
      IrminMerge.exn (fun x -> Conflict x)

    let watch t path =
      Log.infof "Adding a watch on %s" (IrminPath.to_string path);
      let stream = Tag.watch t.tag t.branch in
      IrminMisc.lift_stream (
        map_node Node.sub t path >>= fun node ->
        let old_node = ref node in
        let stream = Lwt_stream.filter_map_s (fun key ->
            Log.debugf "watch: %s" (Block.Key.to_string key);
            Commit.read_exn (commit_t t) key >>= fun commit ->
            begin match Commit.node (commit_t t) commit with
              | None      -> return IrminNode.empty
              | Some node -> node
            end >>= fun node ->
            Node.sub (node_t t) node path >>= fun node ->
            if node = !old_node then return_none
            else (
              old_node := node;
              return (Some (path, key))
            )
          ) stream in
        return stream
      )

    include K

  end

  let watch t path =
    let stream = Snapshot.watch t path in
    Lwt_stream.filter_map_s (fun (p, k) ->
        if IrminPath.(p = path) then
          Commit.read (commit_t t) k >>= function
          | None   -> return_none
          | Some c ->
            node_of_commit t c >>= fun n ->
            Node.find (node_t t) n p
        else
          return_none
      ) stream

  module Dump = struct

    module Log = XLog.Make(struct let section ="IRMIN.DUMP" end)

    type snapshot = Snapshot.t

    module Graph = IrminGraph.Make(K)(R)

    type db = t

    include IrminDump.S(K)(C)

    (* XXX: can be improved quite a lot *)
    let create t roots =
      Log.debugf "export root=%s" (IrminMisc.pretty_list K.to_string roots);
      let table = Block.Key.Table.create () in
      let add k v = Hashtbl.add_multi table k v in
      Tag.read t.tag t.branch >>= function
      | None        -> return { IrminDump.head = None; store = [] }
      | Some commit ->
        let head = Some commit in
        begin match roots with
          | [] -> Commit.list (commit_t t) [commit]
          | _  ->
            let pred = function
              | `Commit k -> Commit.read_exn (commit_t t) k >>= fun c -> return (IrminCommit.edges c)
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
            Commit.read_exn (commit_t t) key >>= fun commit ->
            add key (IrminValue.Commit commit);
            match commit.IrminCommit.node with
            | None   -> return_unit
            | Some k -> nodes := K.Set.add !nodes k; return_unit
          ) commits >>= fun () ->
        let nodes = !nodes in
        Node.list (node_t t) (K.Set.to_list nodes) >>= fun nodes ->
        Log.debugf "export NODES=%s" (IrminMisc.pretty_list K.to_string nodes);
        let contents = ref K.Set.empty in
        Lwt_list.iter_p (fun key ->
            Node.read_exn (node_t t) key >>= fun node ->
            add key (IrminValue.Node node);
            match node.IrminNode.contents with
            | None   -> return_unit
            | Some k -> contents := K.Set.add !contents k; return_unit
          ) nodes >>= fun () ->
        let contents = !contents in
        Contents.list (contents_t t) (K.Set.to_list contents) >>= fun contents ->
        Log.debugf "export CONTENTS=%s" (IrminMisc.pretty_list K.to_string contents);
        Lwt_list.iter_p (fun k ->
            Contents.read_exn (contents_t t) k >>= fun b ->
            add k (IrminValue.Contents b);
            return_unit
          ) contents >>= fun () ->
        let store = Hashtbl.fold ~f:(fun ~key:k ~data init ->
            List.fold_left ~f:(fun acc v -> (k, v) :: acc) ~init data
          ) ~init:[] table in
        return { IrminDump.head; store }

    exception Errors of (Block.key * Block.key * string) list

    let update_aux t { IrminDump.store } =
      Log.debugf "import %d" (List.length store);
      let errors = ref [] in
      let check msg k1 k2 =
        if K.(k1 <> k2) then errors := (k1, k2, msg) :: !errors;
        return_unit
      in
      (* Import contents first *)
      Lwt_list.iter_p (fun (k,v) ->
          match v with
          | IrminValue.Contents x -> Contents.add (contents_t t) x >>= check "value" k
          | _ -> return_unit
        ) store >>= fun () ->
      Lwt_list.iter_p (fun (k,v) ->
          match v with
          | IrminValue.Node x -> Node.add (node_t t) x >>= check "node" k
          | _ -> return_unit
        ) store >>= fun () ->
      Lwt_list.iter_p (fun (k,v) ->
          match v with
          | IrminValue.Commit x -> Commit.add (commit_t t) x >>= check "commit" k
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
      match dump.IrminDump.head with
      | None   -> return_unit
      | Some h -> Tag.update t.tag t.branch h

    let merge ?origin t dump =
      let origin = match origin with
        | None   -> IrminOrigin.create "Merge pulled state."
        | Some o -> o in
      update_aux t dump >>= fun () ->
      match dump.IrminDump.head with
      | None   -> ok ()
      | Some h -> Snapshot.merge ~origin t h

    let merge_exn ?origin t dump =
      merge ?origin t dump >>=
      IrminMerge.exn (fun x -> Conflict x)

    let output t name =
      Log.debugf "output %s" name;
      Contents.dump (contents_t t) >>= fun contents ->
      Node.dump (node_t t)         >>= fun nodes    ->
      Commit.dump (commit_t t)     >>= fun commits  ->
      Tag.dump t.tag               >>= fun refs     ->
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
          add_vertex (`Ref r) [`Shape `Plaintext; `Label (R.to_string r); `Style `Filled];
          let exists l = List.exists ~f:(fun (kk,_) -> K.(kk=k)) l in
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
      if Int.(i <> 0) then Log.errorf "The %s.dot is corrupted" name;
      return_unit

  end

  module Branch = struct

    module Log = XLog.Make(struct let section ="IRMIN.BRANCH" end)

    type db = t

    include Tag.Key

    let current t = t.branch

    let create_force t branch =
      begin Tag.read t.tag t.branch >>= function
        | None   -> Tag.remove t.tag branch
        | Some c -> Tag.update t.tag branch c
      end >>= fun () ->
      return { t with branch }

    let create t branch =
      Tag.mem t.tag branch >>= function
      | true  -> return_none
      | false -> create_force t branch >>= fun t -> return (Some t)

    let merge ?origin t branch =
      let origin = match origin with
        | Some o -> o
        | None   -> IrminOrigin.create "Merge branch %s."
                      (R.to_string t.branch) in
      Tag.read_exn t.tag t.branch  >>= fun c ->
      Snapshot.merge ~origin t c

    let merge_exn ?origin t branch =
      merge ?origin t branch >>=
      IrminMerge.exn (fun x -> Conflict x)

    let list t =
      Tag.list t.tag [t.branch]

    let mem t b =
      Tag.mem t.tag b

    let update t b =
      Tag.update t.tag t.branch b

  end

  module View = struct

    module Log = XLog.Make(struct let section ="IRMIN.VIEW" end)

    type path = key
    type db = t
    include IrminView.Make(K)(C)

    let of_path t path =
      Log.debugf "read_view %s" (IrminPath.to_string path);
      let contents = Contents.read (contents_t t) in
      let node = Node.read (node_t t) in
      map_node Node.sub t path >>= function
      | None   -> create ()
      | Some n ->
        Node.add (node_t t) n >>= fun k ->
        import ~contents ~node k

    let node_of_view t view =
      let contents = Contents.add (contents_t t) in
      let node = Node.add (node_t t) in
      export ~node ~contents view >>= fun key ->
      Node.read_exn (node_t t) key

    let update_path ?origin t path view =
      Log.debugf "update_view %s" (IrminPath.to_string path);
      let origin = match origin with
        | None   -> IrminOrigin.create "Update view to %s" (IrminPath.to_string path)
        | Some o -> o in
      node_of_view t view >>= fun tree ->
      update_node t ~origin (fun node ->
          Node.map (node_t t) node path (fun _ -> tree)
        )

    let merge_path ?origin t path view =
      Log.debugf "merge_view %s" (IrminPath.to_string path);
      of_path t path >>= fun head ->
      merge view ~into:head >>| fun () ->
      let origin = match origin with
        | None   ->
          let buf = Buffer.create 1024 in
          let string_of_action = IrminView.Action.to_string (fun x -> "") in
          List.iter ~f:(fun a ->
              bprintf buf "- %s\n" (string_of_action a)
            ) (actions view);
          IrminOrigin.create "Merge view to %s\n\nActions:\n%s\n"
            (IrminPath.to_string path) (Buffer.contents buf)
        | Some o -> o in
      update_path ~origin t path head >>= fun () ->
      ok ()

    let merge_path_exn ?origin t path view =
      merge_path ?origin t path view >>=
      IrminMerge.exn (fun x -> Conflict x)

  end

end

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
