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

let debug fmt =
  IrminLog.debug "IRMIN" fmt

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
  type t = {
    value   : Value.t;
    tree    : Tree.t;
    revision: Revision.t;
    tag     : Tag.t;
    branch  : Tag.tag;
  }
  include IrminStore.S with type t := t
                        and type key := IrminTree.path
                        and type value := value
                        and type revision := key

  val tag: Tag.tag -> t Lwt.t
  val dump: t -> string -> unit Lwt.t
end

module Make
  (K: IrminKey.BINARY)
  (V: IrminValue.S)
  (T: IrminTag.S)
  (Value   : IrminStore.A_MAKER)
  (Tree    : IrminStore.A_MAKER)
  (Revision: IrminStore.A_MAKER)
  (Tag     : IrminStore.M_MAKER)  =
struct

  open Lwt

  module Key = K

  module Value = IrminValue.Make(Value)(K)(V)
  module Tree = IrminTree.Make(Tree)(K)(Value)
  module Revision = IrminRevision.Make(Revision)(K)(Tree)
  module Tag = IrminTag.Make(Tag)(T)(K)

  type key = Key.t
  type value = Value.value
  type tree = Tree.tree
  type revision = Revision.revision
  type path = string list
  type tag = T.t

  type t = {
    value: Value.t;
    tree: Tree.t;
    revision: Revision.t;
    tag: Tag.t;
    branch: Tag.tag;
  }

  let create () =
    Value.create () >>= fun value ->
    Tree.create () >>= fun tree ->
    Revision.create () >>= fun revision ->
    Tag.create () >>= fun tag ->
    let branch = Tag.master in
    return { value; tree; revision; tag; branch }

  let tag branch =
    create () >>= fun t ->
    return { t with branch }

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
    tree t revision >>= fun tree ->
    fn tree >>= fun tree ->
    Revision.revision t.revision ~tree (parents_of_revision revision) >>= fun key ->
    Tag.update t.tag t.branch key

  let update t path value =
    update_tree t path (fun tree ->
        Tree.update t.tree tree path value
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
    list t [] >>= aux []

  module Graph = IrminGraph.Make(K)

  let dump t name =
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

  let watch _ =
    failwith "watch: TODO"

  module Raw = struct

    let debug fmt =
      IrminLog.debug "RAW" fmt

    type key = Key.t

    type value =
      | Value of Value.value
      | Tree of tree
      | Revision of revision

    module Set = Set.Make(K)

    let set_of_list l =
      let r = ref Set.empty in
      List.iter (fun elt ->
          r := Set.add elt !r
        ) l;
      !r

    let export t roots =
      debug "export roots=%s" (IrminMisc.pretty_list K.pretty roots);
      dump t "export" >>= fun () ->
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
