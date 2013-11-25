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

open Lwt

let debug fmt =
  IrminLog.debug "IRMIN" fmt

type ('a, 'b) store_dump =
  ('a * ('a, 'b) value_dump) list

and ('a, 'b) value_dump =
  | Value of 'b
  | Tree of ('a, 'a) IrminTree.node
  | Revision of ('a, 'a) IrminRevision.node

module Dump (A: IrminBase.S) (B: IrminBase.S) = struct

  let debug fmt =
    IrminLog.debug "DUMP" fmt

  module Value = struct

    type t = (A.t, B.t) value_dump

    module Key = A

    module Value = B

    module Tree = IrminTree.Tree(A)(A)

    module Revision = IrminRevision.Revision(A)(A)

    let name = "value"

    let compare t1 t2 =
      match t1, t2 with
      | Value v1   , Value v2    -> Value.compare v1 v2
      | Value _    , _           -> 1
      | _          , Value _     -> -1
      | Tree t1    , Tree t2     -> Tree.compare t1 t2
      | Tree _     , _           -> 1
      | _          , Tree _      -> -1
      | Revision r1, Revision r2 -> Revision.compare r1 r2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash = Hashtbl.hash

    let pretty = function
      | Value v    -> Value.pretty v
      | Tree t     -> Tree.pretty t
      | Revision r -> Revision.pretty r

    let to_string = function
      | Value v    -> Value.to_string v
      | Tree t     -> Tree.to_string t
      | Revision r -> Revision.to_string r

    let of_json = function
      | `O json ->
        let value    = List.mem_assoc "value"    json in
        let tree     = List.mem_assoc "tree"     json in
        let revision = List.mem_assoc "revision" json in
        begin match value, tree, revision with
          | true , false, false -> Value    (Value.of_json (List.assoc "value" json))
          | false, true , false -> Tree     (Tree.of_json (List.assoc "tree"  json))
          | false, false, true  -> Revision (Revision.of_json (List.assoc "revision" json))
          | _ -> IrminBuffer.parse_error "Irmin.Dump.Value.of_json: invalid value (1)"
        end
      | _ -> IrminBuffer.parse_error "Irmin.VDump.Value.of_json: invalid value (2)"

    let to_json = function
      | Value v    -> `O [ "value"   , Value.to_json v   ]
      | Tree t     -> `O [ "tree"    , Tree.to_json t    ]
      | Revision r -> `O [ "revision", Revision.to_json r]

    (* |----------|---------| *)
    (* | MAGIC(8) | PAYLOAD | *)
    (* |----------|---------| *)

    let header = "IRMIN00"

    let sizeof t =
      8 + match t with
        | Value v    -> Value.sizeof v
        | Tree t     -> Tree.sizeof t
        | Revision r -> Revision.sizeof r

    let get buf =
      debug "get";
      let h = IrminBuffer.get_string buf 8 in
      if h = header then
        (* XXX: very fragile *)
        match IrminBuffer.pick_string buf 1 with
        | Some "V" -> (match Value.get buf    with None -> None | Some v -> Some (Value v))
        | Some "T" -> (match Tree.get buf     with None -> None | Some t -> Some (Tree t))
        | Some "R" -> (match Revision.get buf with None -> None | Some r -> Some (Revision r))
        | _        -> None
      else
        None

    let set buf t =
      debug "set";
      IrminBuffer.set_string buf header;
      match t with
      | Value v    -> Value.set buf v
      | Tree t     -> Tree.set buf t
      | Revision r -> Revision.set buf r

  end

  include IrminBase.List(IrminBase.Pair(A)(Value))

  let name = "store"

  let is_empty = function
    | []  -> true
    | _   -> false

end

module type S = sig
  type key
  type value
  type tag
  module Key: IrminKey.BINARY
    with type t = key
  module Value: IrminValue.STORE
    with type key = key
     and type value = value
  module Tree: IrminTree.STORE
    with type key = key
     and type value = value
  module Revision: IrminRevision.STORE
    with type key = key
     and type tree = Tree.tree
  module Tag: IrminTag.STORE
    with type tag = tag
     and type key = key
  include IrminStore.S with type key := IrminTree.Path.t
                        and type value := value
                        and type revision := key
                        and type dump = (key, value) store_dump

  val value_store: t -> Value.t
  val tree_store: t -> Tree.t
  val revision_store: t -> Revision.t
  val tag_store: t -> Tag.t

  val output: t -> string -> unit Lwt.t
  module Dump: sig
    include IrminBase.S with type t = dump
    val is_empty: t -> bool
  end
end

module State
    (Value   : IrminValue.STORE)
    (Tree    : IrminTree.STORE)
    (Revision: IrminRevision.STORE)
    (Tag     : IrminTag.STORE) =
struct

  type t = {
    value   : Value.t;
    tree    : Tree.t;
    revision: Revision.t;
    tag     : Tag.t;
    branch  : Tag.tag;
  }

  let create () =
    Value.create () >>= fun value ->
    Tree.create () >>= fun tree ->
    Revision.create () >>= fun revision ->
    Tag.create () >>= fun tag ->
    let branch = Tag.master in
    return { value; tree; revision; tag; branch }

  let value_store t = t.value

  let tree_store t = t.tree

  let revision_store t = t.revision

  let tag_store t = t.tag

end

module type MAKER =
  functor (K: IrminKey.BINARY) ->
  functor (V: IrminValue.S)    ->
  functor (T: IrminTag.S)      ->
  functor (Value   : IrminStore.A_MAKER) ->
  functor (Tree    : IrminStore.A_MAKER) ->
  functor (Revision: IrminStore.A_MAKER) ->
  functor (Tag     : IrminStore.M_MAKER) ->
    S with type key = K.t
       and type value = V.t
       and type tag = T.t

module Proxy
    (Store   : IrminStore.S_MAKER)
    (K: IrminKey.BINARY)
    (V: IrminValue.S)
    (T: IrminTag.S)
    (Value   : IrminStore.A_MAKER)
    (Tree    : IrminStore.A_MAKER)
    (Revision: IrminStore.A_MAKER)
    (Tag     : IrminStore.M_MAKER) =
struct

  module Key = K
  module Value = IrminValue.Make(Value)(K)(V)
  module Tree = IrminTree.Make(Tree)(K)(Value)
  module Revision = IrminRevision.Make(Revision)(K)(Tree)
  module Tag = IrminTag.Make(Tag)(T)(K)
  module Dump = Dump(K)(V)

  type key = K.t
  type value = V.t
  type tag = T.t
  type revision = key
  type dump = (key, value) store_dump

  module Store = Store(IrminTree.Path)(V)(K)(Dump)
  module State = State(Value)(Tree)(Revision)(Tag)

  (* XXX: The following is tedious code. Is there a better way ? *)

  type t = (Store.t * State.t)

  let create () =
    Store.create () >>= fun store ->
    State.create () >>= fun state ->
    return (store, state)

  let value_store (_, t) = State.value_store t

  let tree_store (_, t) = State.tree_store t

  let revision_store (_, t) = State.revision_store t

  let tag_store (_, t) = State.tag_store t

  let read (t, _) = Store.read t

  let read_exn (t, _) = Store.read_exn t

  let mem (t, _) = Store.mem t

  let list (t, _) = Store.list t

  let contents (t, _) = Store.contents t

  let update (t, _) = Store.update t

  let remove (t, _) = Store.remove t

  let snapshot (t, _) = Store.snapshot t

  let revert (t, _) = Store.revert t

  let watch (t, _) = Store.watch t

  let export (t, _) = Store.export t

  let import (t, _) = Store.import t

  let output _ _ =
    return_unit

end

module Make
    (K: IrminKey.BINARY)
    (V: IrminValue.S)
    (T: IrminTag.S)
    (Value   : IrminStore.A_MAKER)
    (Tree    : IrminStore.A_MAKER)
    (Revision: IrminStore.A_MAKER)
    (Tag     : IrminStore.M_MAKER) =
struct

  module Key = K
  module Value = IrminValue.Make(Value)(K)(V)
  module Tree = IrminTree.Make(Tree)(K)(Value)
  module Revision = IrminRevision.Make(Revision)(K)(Tree)
  module Tag = IrminTag.Make(Tag)(T)(K)
  module Dump = Dump(K)(V)
  module State = State(Value)(Tree)(Revision)(Tag)

  type key = Key.t
  type value = Value.value
  type tree = Tree.tree
  type revision = Revision.revision
  type path = string list
  type tag = T.t
  type dump = Dump.t

  include State

  let revision t =
    Tag.read t.tag t.branch >>= function
    | None   -> return_none
    | Some k -> Revision.read_exn t.revision k >>= function r -> return (Some r)

  let tree t = function
    | None     -> return Tree.empty
    | Some rev ->
      match Revision.tree t.revision rev with
      | None      -> return Tree.empty
      | Some tree -> tree

  let parents_of_revision = function
    | None   -> []
    | Some r -> [r]

  let update_tree t path fn =
    revision t >>= fun revision ->
    tree t revision >>= fun old_tree ->
    fn old_tree >>= fun tree ->
    if old_tree = tree then return_unit
    else (
      Revision.revision t.revision ~tree (parents_of_revision revision) >>= fun key ->
      Tag.update t.tag t.branch key
    )

  type watch = path * (path -> key -> unit)

  let watches: watch list ref = ref []

  let fire path (p, _) =
    if p = path then true
    else
      let rec aux = function
        | _   , []   -> true
        | []  , _    -> false
        | a::b, x::y -> a=x && aux (b, y) in
      aux (path, p)

  let update t path value =
    update_tree t path (fun tree ->
        Tree.update t.tree tree path value
      ) >>= fun () ->
    let ws = List.filter (fire path) !watches in
    if ws = [] then return_unit
    else (
      Tag.read_exn t.tag t.branch >>= fun rev ->
      List.iter (fun (_, f) -> f path rev) ws;
      return_unit
    )

  let remove t path =
    update_tree t path (fun tree ->
        Tree.remove t.tree tree path
      )

  let read_tree fn t path =
    revision t >>= fun revision ->
    tree t revision >>= fun tree ->
    fn t.tree tree path

  let read =
    read_tree Tree.find

  let read_exn =
    read_tree Tree.find_exn

  let mem =
    read_tree Tree.valid

  let snapshot t =
    Tag.read_exn t.tag t.branch

  let revert t r =
    Tag.update t.tag t.branch r

  (* Return the subpaths. *)
  let list t path =
    revision t >>= fun revision ->
    tree t revision >>= fun tree ->
    Tree.sub t.tree tree path >>= function
    | None      -> return_nil
    | Some tree ->
      let paths = List.map (fun (c,_) -> path @ [c]) tree.IrminTree.children in
      return paths

  let contents t =
    revision t >>= fun revision ->
    tree t revision >>= fun tree ->
    let rec aux seen = function
      | []       -> return (List.sort compare seen)
      | path::tl ->
        list t path >>= fun childs ->
        let todo = childs @ tl in
        Tree.find t.tree tree path >>= function
        | None   -> aux seen todo
        | Some v -> aux ((path, v) :: seen) todo in
    begin Tree.find t.tree tree [] >>= function
      | None   -> return_nil
      | Some v -> return [ ([], v) ]
    end
    >>= fun init ->
    list t [] >>= aux init

  module Graph = IrminGraph.Make(K)

  let output t name =
    debug "DUMP %s" name;
    Value.contents t.value       >>= fun values    ->
    Tree.contents  t.tree        >>= fun trees     ->
    Revision.contents t.revision >>= fun revisions ->
    let vertex = ref [] in
    let add_vertex v l =
      vertex := (v, l) :: !vertex in
    let edges = ref [] in
    let add_edge v1 l v2 =
      edges := (v1, l, v2) :: !edges in
    let label k =
      `Label (K.pretty k) in
    let label_of_value k v =
      let k = K.pretty k in
      let v =
        let v = V.pretty v in
        if v.[0] = '"' && v.[String.length v - 1] = '"' then
          String.sub v 1 (String.length v - 2)
        else v in
      `Label (Printf.sprintf "%s | %s" k v) in
    List.iter (fun (k, v) ->
        add_vertex k [`Shape `Record; label_of_value k v]
      ) values;
    List.iter (fun (k, t) ->
        add_vertex k [`Shape `Box; `Style `Dotted; label k];
        List.iter (fun (l,c) ->
            add_edge k [`Style `Solid; `Label l] c
          ) t.IrminTree.children;
        match t.IrminTree.value with
        | None   -> ()
        | Some v -> add_edge k [`Style `Dotted] v
      ) trees;
    List.iter (fun (k, r) ->
        add_vertex k [`Shape `Box; `Style `Bold; label k];
        List.iter (fun p ->
            add_edge k [`Style `Bold] p
          ) r.IrminRevision.parents;
        match r.IrminRevision.tree with
        | None      -> ()
        | Some tree -> add_edge k [`Style `Dashed] tree
      ) revisions;
    (* XXX: this is not Xen-friendly *)
    let file = name ^ ".dot" in
    let oc = open_out file in
    Graph.output (Format.formatter_of_out_channel oc) !vertex !edges name;
    close_out oc;
    let _ = Sys.command (Printf.sprintf "dot -Tpng %s.dot -o%s.png" name name) in
    return_unit

  let watch t path =
    let stream, push, _ = Lwt_stream.create_with_reference () in
    let callback path rev =
      push (Some (path, rev)) in
    watches := (path, callback) :: !watches;
    stream

  let debug fmt =
    IrminLog.debug "RAW" fmt

  module Set = Set.Make(K)

  let set_of_list l =
    let r = ref Set.empty in
    List.iter (fun elt ->
        r := Set.add elt !r
      ) l;
    !r

  (* XXX: can be improved quite a lot *)
  let export t roots =
    debug "export root=%s" (IrminMisc.pretty_list K.pretty roots);
    output t "export" >>= fun () ->
    let contents = Hashtbl.create 1024 in
    let add k v =
      Hashtbl.add contents k v in
    Tag.read t.tag t.branch >>= function
    | None          -> return_nil
    | Some revision ->
      begin
        if roots = [] then Revision.list t.revision revision
        else
          let pred k =
            Revision.read_exn t.revision k >>= fun r ->
            return r.IrminRevision.parents in
          Graph.closure pred ~min:roots ~max:[revision] >>= fun g ->
          return (Graph.vertex g)
      end
      >>= fun revisions ->
      Lwt_list.fold_left_s (fun set key ->
          Revision.read_exn t.revision key >>= fun revision ->
          add key (Revision revision);
          match revision.IrminRevision.tree with
          | None      -> return set
          | Some tree ->
            Tree.list t.tree tree >>= fun trees ->
            return (Set.union set (set_of_list trees))
        ) Set.empty revisions
      >>= fun trees ->
      let trees = Set.elements trees in
      debug "export TREES=%s" (IrminMisc.pretty_list Key.pretty trees);
      Lwt_list.fold_left_s (fun set key ->
          Tree.read_exn t.tree key >>= fun tree ->
          add key (Tree tree);
          match tree.IrminTree.value with
          | None       -> return set
          | Some value ->
            Value.list t.value value >>= fun values ->
            return (Set.union set (set_of_list values))
        ) Set.empty trees
      >>= fun values ->
      let values = Set.elements values in
      debug "export VALUES=%s" (IrminMisc.pretty_list Key.pretty values);
      Lwt_list.iter_p (fun key ->
          Value.read_exn t.value key >>= fun value ->
          add key (Value value);
          return_unit
        ) values
      >>= fun () ->
      let list = Hashtbl.fold (fun k v acc -> (k, v) :: acc) contents [] in
      return list

  exception Errors of (key * key * string) list

  let import t list =
    debug "import %s" (IrminMisc.pretty_list Key.pretty (List.map fst list));
    let errors = ref [] in
    let check msg k1 k2 =
      if k1 <> k2 then errors := (k1, k2, msg) :: !errors;
      return_unit
    in
    Lwt_list.iter_p (fun (k,v) ->
        match v with
        | Value value -> Value.add t.value value   >>= check "value" k
        | Tree tree   -> Tree.add t.tree tree      >>= check "tree" k
        | Revision r  -> Revision.add t.revision r >>= check "revision" k
      ) list
    >>= fun () ->
    if !errors = [] then return_unit
    else fail (Errors !errors)

end

module Binary
    (K: IrminKey.BINARY)
    (V: IrminValue.S)
    (T: IrminTag.S)
    (Value   : IrminStore.A_BINARY)
    (Tree    : IrminStore.A_BINARY)
    (Revision: IrminStore.A_BINARY)
    (Tag     : IrminStore.M_BINARY) =
struct

  include Make(K)(V)(T)
    (IrminStore.A(Value))
    (IrminStore.A(Tree))
    (IrminStore.A(Revision))
    (IrminStore.M(Tag))

end

module type SIMPLE = S
  with type key = IrminKey.SHA1.t
   and type value = IrminValue.Simple.t
   and type tag = IrminTag.Simple.t

module Simple (A: IrminStore.A_BINARY) (M: IrminStore.M_BINARY) = struct

  include Binary(IrminKey.SHA1)(IrminValue.Simple)(IrminTag.Simple)(A)(A)(A)(M)

end
