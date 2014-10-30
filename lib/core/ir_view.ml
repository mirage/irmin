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

open Lwt
open Ir_merge.OP
open Ir_misc.OP
open Printf
open Sexplib.Std
open Bin_prot.Std

module Log = Log.Make(struct let section = "view" end)
module PathSet = Ir_misc.Set(Ir_path)

type ('path, 'a) action =
  | Read of 'path * 'a option
  | Write of 'path * 'a option
  | List of 'path list * 'path list
with bin_io, compare, sexp

module Action = struct

  include Tc.I2(struct
    type ('a, 'b) t = ('a, 'b) action with bin_io, compare, sexp
    end)

  let o f = function
    | None   -> "<none>"
    | Some x -> f x

  let l f = Ir_misc.list_pretty f

  let pretty pretty_path pretty_a = function
    | Read (p,x) -> sprintf "read %s -> %s" (pretty_path p) (o pretty_a x)
    | Write (p,x) ->sprintf "write %s %s" (pretty_path p) (o pretty_a x)
    | List (i,o) -> sprintf "list %s -> %s" (l pretty_path i) (l pretty_path o)

end

module XContents = struct

  type ('k, 'c) contents_or_key =
    | Key of 'k
    | Contents of 'c
    | Both of 'k * 'c

  type ('k, 'c) t = ('k, 'c) contents_or_key ref
  (* Same as [Contents.t] but can either be a raw contents or a
     key that will be fetched lazily. *)

  let create c =
    ref (Contents c)

  let export c =
    match !c with
    | Both (k, _)
    | Key k      -> k
    | Contents _ -> failwith "Contents.export"

  let key k =
    ref (Key k)

  let read ~contents t =
    match !t with
    | Both (_, c)
    | Contents c -> return (Some c)
    | Key k      ->
      contents k >>= function
      | None   -> return_none
      | Some c ->
        t := Contents c;
        return (Some c)

end

module XNode = struct

  type ('k, 'c) node = {
    contents: ('k, 'c) XContents.t option;
    succ    : ('k, 'c) t Ir_misc.StringMap.t;
  }

  and ('k, 'c) node_or_key  =
    | Key of 'k
    | Node of ('k, 'c) node
    | Both of 'k * ('k, 'c) node

  and ('k, 'c) t = ('k, 'c) node_or_key ref
  (* Similir to [Node.t] but using where all of the values can
     just be keys. *)

  let create' contents succ =
    Node { contents; succ }

  let create contents succ =
    ref (create' contents succ)

  let key k =
    ref (Key k)

  let empty () =
    create None Ir_misc.StringMap.empty

  let is_empty n =
    match !n with
    | Key _  -> false
    | Both (_, n)
    | Node n -> n.contents = None && Ir_misc.StringMap.is_empty n.succ

  let import n =
    let contents = match n.Ir_node.contents with
      | None   -> None
      | Some k -> Some (XContents.key k) in
    let succ = Ir_misc.StringMap.map key n.Ir_node.succ in
    { contents; succ }

  let export n =
    match !n with
    | Both (k, _)
    | Key k  -> k
    | Node _ -> failwith "Node.export"

  let export_node n =
    let contents = match n.contents with
      | None   -> None
      | Some c -> Some (XContents.export c) in
    let succ = Ir_misc.StringMap.map export n.succ in
    { Ir_node.contents; succ }

  let read ~node t =
    match !t with
    | Both (_, n)
    | Node n   -> return (Some n)
    | Key k    ->
      node k >>= function
      | None   -> return_none
      | Some n ->
        t := Node n;
        return (Some n)

  let contents ~node ~contents t =
    read ~node t >>= function
    | None   -> return_none
    | Some c ->
      match c.contents with
      | None   -> return_none
      | Some c -> XContents.read ~contents c

  let update_contents ~node t v =
    read ~node t >>= function
    | None   -> if v = None then return_unit else fail Not_found (* XXX ? *)
    | Some n ->
      let new_n = match v with
        | None   -> { n with contents = None }
        | Some c -> { n with contents = Some (XContents.create c) } in
      t := Node new_n;
      return_unit

  let update_succ ~node t succ =
    read ~node t >>= function
    | None   ->
      if Ir_misc.StringMap.is_empty succ then return_unit else
        fail Not_found (* XXX ? *)
    | Some n ->
      let new_n = { n with succ } in
      t := Node new_n;
      return_unit

end

type ('k, 'c) t = {
  node    : 'k -> ('k, 'c) XNode.node option Lwt.t;
  contents: 'k -> 'c option Lwt.t;
  view    : ('k, 'c) XNode.t;
  mutable ops: (Ir_path.t, 'c) Action.t list;
  mutable parents: 'k list;
}

module Make (K: Ir_uid.S) (C: Ir_contents.S) = struct

  type node = K.t
  type nonrec t = (K.t, C.t) t
  type key = Ir_path.t
  type value = C.t

  module CO = Tc.App1(Tc.O)(C)
  module PL = Tc.App1(Tc.L)(Ir_path)

  module A = Tc.App2(Action)(Ir_path)(C)

  let create () =
    Log.debugf "create";
    let node _ = return_none in
    let contents _ = return_none in
    let view = XNode.empty () in
    let ops = [] in
    let parents = [] in
    return { parents; node; contents; view; ops }

  let sub t path =
    let rec aux node = function
      | []   -> return (Some node)
      | h::p ->
        XNode.read ~node:t.node node >>= function
        | None -> return_none
        | Some { XNode.succ; _ } ->
          try
            let v =  Ir_misc.StringMap.find h succ in
            aux v p
          with Not_found ->
            return_none
    in
    aux t.view path

  let read (t:t) path =
    sub t path >>= function
    | None   -> return_none
    | Some n -> XNode.contents ~node:t.node ~contents:t.contents n

  let read t k =
    read t k >>= fun v ->
    t.ops <- Read (k, v) :: t.ops;
    return v

  let read_exn t k =
    read t k >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem t k =
    read t k >>= function
    | None  -> return false
    | _     -> return true

  let list t paths =
    let aux acc path =
      sub t path >>= function
      | None   -> return acc
      | Some n ->
        XNode.read ~node:t.node n >>= function
        | None -> return acc
        | Some { XNode.succ; _ } ->
          let paths = List.map (fun p -> path @ [p]) (Ir_misc.StringMap.keys succ) in
          let paths = PathSet.of_list paths in
          return (PathSet.union acc paths) in
    Lwt_list.fold_left_s aux PathSet.empty paths >>= fun paths ->
    return (PathSet.to_list paths)

  let list t paths =
    list t paths >>= fun result ->
    t.ops <- List (paths, result) :: t.ops;
    return result

  let dump t =
    failwith "TODO"

  let with_cleanup t view fn =
    fn () >>= fun () ->
    XNode.read ~node:t.node view >>= function
    | None   -> return_unit
    | Some n ->
      let succ =
        Ir_misc.StringMap.filter (fun _ n -> not (XNode.is_empty n)) n.XNode.succ
      in
      XNode.update_succ ~node:t.node view succ

  let update' t k v =
    let rec aux view = function
      | []   -> XNode.update_contents ~node:t.node view v
      | h::p ->
        XNode.read ~node:t.node view >>= function
        | None   -> if v = None then return_unit else fail Not_found (* XXX ?*)
        | Some n ->
          try
            let child = Ir_misc.StringMap.find h n.XNode.succ in
            if v = None then with_cleanup t view (fun () -> aux child p)
            else aux child p
          with Not_found ->
            if v = None then return_unit
            else
              let child = XNode.empty () in
              let succ = Ir_misc.StringMap.add h child n.XNode.succ in
              XNode.update_succ ~node:t.node view succ >>= fun () ->
              aux child p in
    aux t.view k

  let update' t k v =
    t.ops <- Write (k, v) :: t.ops;
    update' t k v

  let update t k v =
    update' t k (Some v)

  let remove t k =
    update' t k None

  let watch _ =
    failwith "TODO"

  let apply t a =
    Log.debugf "apply %a" force (show (module A) a);
    match a with
    | Write (k, v) -> update' t k v >>= ok
    | Read (k, v)  ->
      read t k >>= fun v' ->
      if Tc.equal (module CO) v v' then ok ()
      else
        let str = function
          | None   -> "<none>"
          | Some c -> Tc.show (module C) c in
        conflict "read %s: got %S, expecting %S"
          (Tc.show (module Ir_path) k) (str v') (str v)
    | List (l, r) ->
      list t l >>= fun r' ->
      if Tc.equal (module PL) r r' then ok ()
      else
        let str = Ir_misc.list_pretty (Tc.show (module Ir_path)) in
        conflict "list %s: got %s, expecting %s" (str l) (str r') (str r)

  let actions t =
    List.rev t.ops

  let merge t1 ~into =
    Ir_merge.iter (apply into) (List.rev t1.ops) >>| fun () ->
    into.parents <- Ir_misc.list_dedup (t1.parents @ into.parents);
    ok ()

end

module Store (S: Ir_bc.STORE) = struct

  module K = S.Block.Key
  module C = S.Value
  module Path = S.Key
  module Origin = S.Origin

  include Make(K)(C)

  type db = S.t
  type path = S.key

  let import ~parents ~contents ~node key =
    Log.debugf "import %a" force (show (module K) key);
    node key >>= function
    | None   -> fail Not_found
    | Some n ->
      let node k =
        node k >>= function
        | None   -> return_none
        | Some n -> return (Some (XNode.import n)) in
      let view = XNode.key key in
      let ops = [] in
      return { parents; node; contents; view; ops }
  let export ~contents ~node t =
    Log.debugf "export";
    let node n =
      node (XNode.export_node n) in
    let todo = Stack.create () in
    let rec add_to_todo n =
      match !n with
      | XNode.Both _
      | XNode.Key _  -> ()
      | XNode.Node x ->
        (* 1. we push the current node job on the stack. *)
        Stack.push (fun () ->
            node x >>= fun k ->
            n := XNode.Key k;
            return_unit
          ) todo;
        (* 2. we push the contents job on the stack. *)
        Stack.push (fun () ->
            match x.XNode.contents with
            | None   -> return_unit
            | Some c ->
              match !c with
              | XContents.Both _
              | XContents.Key _       -> return_unit
              | XContents.Contents x  ->
                contents x >>= fun k ->
                c := XContents.Key k;
                return_unit
          ) todo;
        (* 3. we push the children jobs on the stack. *)
        Ir_misc.StringMap.iter (fun _ n ->
            Stack.push (fun () -> add_to_todo n; return_unit) todo
          ) x.XNode.succ;
    in
    let rec loop () =
      let task =
        try Some (Stack.pop todo)
        with Not_found -> None
      in
      match task with
      | None   -> return_unit
      | Some t -> t () >>= loop
    in
    add_to_todo t.view;
    loop () >>= fun () ->
    return (XNode.export t.view)

  module Contents = S.Block.Contents
  module Node = S.Block.Node

  let of_path t path =
    Log.debugf "read_view %a" force (show (module Path) path);
    let contents = Contents.read (S.contents_t t) in
    let node = Node.read (S.node_t t) in
    let parents =
      S.head t >>= function
      | None   -> return_nil
      | Some h -> return [h] in
    S.read_node t path >>= function
    | None   -> create ()
    | Some n ->
      Node.add (S.node_t t) n >>= fun k ->
      parents >>= fun parents ->
      import ~parents ~contents ~node k

  let node_of_view t view =
    let contents = Contents.add (S.contents_t t) in
    let node = Node.add (S.node_t t) in
    export ~node ~contents view >>= fun key ->
    Node.read_exn (S.node_t t) key

  let update_path ?origin t path view =
    Log.debugf "update_view %a" force (show (module Path) path);
    let origin = match origin with
      | None   -> Origin.create "Update view to %s" (Tc.show (module Path) path)
      | Some o -> o in
    node_of_view t view >>= fun node ->
    S.update_node t origin path node

  let origin_of_actions ?origin actions =
    match origin with
    | None ->
      let buf = Buffer.create 1024 in
      let string_of_action =
        Action.pretty (Tc.show (module Path)) (fun x -> "")
      in
      List.iter (fun a ->
          bprintf buf "- %s\n" (string_of_action a)
        ) actions;
      Origin.create "Actions:\n%s\n" (Buffer.contents buf)
    | Some o -> o

  let rebase_path ?origin t path view =
    Log.debugf "merge_view %a" force (show (module Path) path);
    S.read_node t [] >>= function
    | None           -> fail Not_found
    | Some head_node ->
      of_path t path                       >>= fun head_view ->
      merge view ~into:head_view           >>| fun () ->
      let origin = origin_of_actions ?origin (actions view) in
      update_path ~origin t path head_view >>= fun () ->
      ok ()

  let rebase_path_exn ?origin t path view =
    rebase_path ?origin t path view >>=
    Ir_merge.exn

  let merge_path ?origin t path view =
    Log.debugf "merge_view %a" force (show (module Path) path);
    S.read_node t [] >>= function
    | None           -> fail Not_found
    | Some head_node ->
      (* First, we check than we can rebase the view on the current
         HEAD. *)
      of_path t path             >>= fun head_view ->
      merge view ~into:head_view >>| fun () ->
      (* Now that we know that rebasing is possible, we discard the
         result and proceed as a normal merge, ie. we apply the view
         on a branch, and we merge the branch back into the store. *)
      node_of_view t view        >>= fun view_node ->
      (* Create a commit with the contents of the view *)
      S.Block.Node.map (S.node_t t) head_node path (fun _ -> view_node)
      >>= fun new_head_node ->
      Lwt_list.map_p (S.Block.Commit.read_exn (S.commit_t t)) view.parents
      >>= fun parents ->
      let origin = origin_of_actions ?origin (actions view) in
      S.Block.Commit.commit (S.commit_t t) origin ~node:new_head_node ~parents
      >>= fun (k, _) ->
      (* We want to avoid to create a merge commit when the HEAD has
         not been updated since the view has been created. *)
      S.head t >>= function
      | None ->
        (* The store is empty, create a fresh commit. *)
        S.update_head t k >>= ok
      | Some head ->
        if List.mem head view.parents then
          S.update_head t k >>= ok
        else
          let origin =
            Origin.create "Merge view to %s\n" (Tc. show (module Path) path)
          in
          S.merge_head t ~origin k

  let merge_path_exn ?origin t path view =
    merge_path ?origin t path view >>=
    Ir_merge.exn

end

module type S = sig
  type value
  type node
  type path
  include Ir_rw.S
    with type t = (node, value) t
     and type value := value
     and type key = path
  val actions: t -> (path, value) Action.t list
  val merge: t -> into:t -> unit Ir_merge.result Lwt.t
end

module type STORE = sig
  include S
  type db
  type path = Path.t
  val of_path: db -> path -> t Lwt.t
  val update_path: ?origin:origin -> db -> path -> t -> unit Lwt.t
  val rebase_path: ?origin:origin -> db -> path -> t -> unit Merge.result Lwt.t
  val rebase_path_exn: ?origin:origin -> db -> path -> t -> unit Lwt.t
  val merge_path: ?origin:origin -> db -> path -> t -> unit Merge.result Lwt.t
  val merge_path_exn: ?origin:origin -> db -> path -> t -> unit Lwt.t
end
