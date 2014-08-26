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
open IrminCore
open IrminMerge.OP
open Printf

type origin = IrminOrigin.t
type path = IrminPath.t

module Log = Log.Make(struct let section = "view" end)

module Action = struct

  type 'a t =
    | Read of IrminPath.t * 'a option
    | Write of IrminPath.t * 'a option
    | List of IrminPath.t list * IrminPath.t list
  with bin_io, compare, sexp

  let o f = function
    | None   -> "<none>"
    | Some x -> f x

  let l f = IrminMisc.pretty_list f

  let to_string string_of_a = function
    | Read (p,x)  -> sprintf "read %s -> %s" (IrminPath.to_string p) (o string_of_a x)
    | Write (p,x) -> sprintf "write %s %s" (IrminPath.to_string p) (o string_of_a x)
    | List (i,o)  -> sprintf "list %s -> %s" (l IrminPath.to_string i) (l IrminPath.to_string o)

end

module Contents = struct

  type ('k, 'c) contents_or_key =
    | Key of 'k
    | Contents of 'c
    | Both of 'k * 'c

  type ('k, 'c) t = ('k, 'c) contents_or_key ref
  (* Same as [IrminContents.t] but can either be a raw contents or a
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

module Node = struct

  type ('k, 'c) node = {
    contents: ('k, 'c) Contents.t option;
    succ    : ('k, 'c) t String.Map.t;
  }

  and ('k, 'c) node_or_key  =
    | Key of 'k
    | Node of ('k, 'c) node
    | Both of 'k * ('k, 'c) node

  and ('k, 'c) t = ('k, 'c) node_or_key ref
  (* Similir to [IrminNode.t] but using where all of the values can
     just be keys. *)

  let create' contents succ =
    Node { contents; succ }

  let create contents succ =
    ref (create' contents succ)

  let key k =
    ref (Key k)

  let empty () =
    create None String.Map.empty

  let is_empty n =
    match !n with
    | Key _  -> false
    | Both (_, n)
    | Node n -> n.contents = None && String.Map.is_empty n.succ

  let import n =
    let contents = match n.IrminNode.contents with
      | None   -> None
      | Some k -> Some (Contents.key k) in
    let succ = String.Map.map ~f:key n.IrminNode.succ in
    { contents; succ }

  let export n =
    match !n with
    | Both (k, _)
    | Key k  -> k
    | Node _ -> failwith "Node.export"

  let export_node n =
    let contents = match n.contents with
      | None   -> None
      | Some c -> Some (Contents.export c) in
    let succ = String.Map.map ~f:export n.succ in
    { IrminNode.contents; succ }

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
    read node t >>= function
    | None   -> return_none
    | Some c ->
      match c.contents with
      | None   -> return_none
      | Some c -> Contents.read contents c

  let update_contents ~node t v =
    read node t >>= function
    | None   -> if v = None then return_unit else fail Not_found (* XXX ? *)
    | Some n ->
      let new_n = match v with
        | None   -> { n with contents = None }
        | Some c -> { n with contents = Some (Contents.create c) } in
      t := Node new_n;
      return_unit

  let update_succ ~node t succ =
    read node t >>= function
    | None   ->
      if String.Map.is_empty succ then return_unit else
        fail Not_found (* XXX ? *)
    | Some n ->
      let new_n = { n with succ } in
      t := Node new_n;
      return_unit

end

type ('k, 'c) t = {
  node    : 'k -> ('k, 'c) Node.node option Lwt.t;
  contents: 'k -> 'c option Lwt.t;
  view    : ('k, 'c) Node.t;
  mutable ops: 'c Action.t list;
  mutable parents: 'k list;
}

module Make (K: IrminKey.S) (C: IrminContents.S) = struct

  type node = K.t

  type nonrec t = (K.t, C.t) t

  type key = IrminPath.t

  type value = C.t

  let create () =
    Log.debugf "create";
    let node _ = return_none in
    let contents _ = return_none in
    let view = Node.empty () in
    let ops = [] in
    let parents = [] in
    return { parents; node; contents; view; ops }

  let sub t path =
    let rec aux node = function
      | []   -> return (Some node)
      | h::p ->
        Node.read t.node node >>= function
        | None               -> return_none
        | Some { Node.succ } ->
          match String.Map.find succ h with
          | None   -> return_none
          | Some v -> aux v p in
    aux t.view path

  let read (t:t) path =
    sub t path >>= function
    | None   -> return_none
    | Some n -> Node.contents ~node:t.node ~contents:t.contents n

  let read t k =
    read t k >>= fun v ->
    t.ops <- Action.Read (k, v) :: t.ops;
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
        Node.read t.node n >>= function
        | None               -> return acc
        | Some { Node.succ } ->
          let paths = List.map ~f:(fun p -> path @ [p]) (String.Map.keys succ) in
          let paths = IrminPath.Set.of_list paths in
          return (IrminPath.Set.union acc paths) in
    Lwt_list.fold_left_s aux IrminPath.Set.empty paths >>= fun paths ->
    return (IrminPath.Set.to_list paths)

  let list t paths =
    list t paths >>= fun result ->
    t.ops <- Action.List (paths, result) :: t.ops;
    return result

  let dump t =
    failwith "TODO"

  let with_cleanup t view fn =
    fn () >>= fun () ->
    Node.read t.node view >>= function
    | None   -> return_unit
    | Some n ->
      let succ = String.Map.filter
          ~f:(fun ~key:_ ~data:n -> not (Node.is_empty n)) n.Node.succ in
      Node.update_succ t.node view succ

  let update' t k v =
    let rec aux view = function
      | []   -> Node.update_contents t.node view v
      | h::p ->
        Node.read t.node view >>= function
        | None   -> if v = None then return_unit else fail Not_found (* XXX ?*)
        | Some n ->
          match String.Map.find n.Node.succ h with
          | Some child ->
            if v = None then with_cleanup t view (fun () -> aux child p)
            else aux child p
          | None      ->
            if v = None then return_unit
            else
              let child = Node.empty () in
              let succ = String.Map.add n.Node.succ h child in
              Node.update_succ t.node view succ >>= fun () ->
              aux child p in
    aux t.view k

  let update' t k v =
    t.ops <- Action.Write (k, v) :: t.ops;
    update' t k v

  let update t k v =
    update' t k (Some v)

  let remove t k =
    update' t k None

  let watch _ =
    failwith "TODO"

  let apply t a =
    Log.debugf "apply %S" (Action.to_string C.to_string a);
    match a with
    | Action.Write (k, v) -> update' t k v >>= ok
    | Action.Read (k, v)  ->
      read t k >>= fun v' ->
      if Option.equal C.equal v v' then ok ()
      else
        let str = function
          | None   -> "<none>"
          | Some c -> C.to_string c in
        conflict "read %s: got %S, expecting %S"
          (IrminPath.to_string k) (str v') (str v)
    | Action.List (l,r) ->
      list t l >>= fun r' ->
      if List.equal ~equal:IrminPath.equal r r' then ok ()
      else
        let str = IrminMisc.pretty_list IrminPath.to_string in
        conflict "list %s: got %s, expecting %s" (str l) (str r') (str r)

  let actions t =
    List.rev t.ops

  let merge t1 ~into =
    IrminMerge.iter (apply into) (List.rev t1.ops) >>| fun () ->
    into.parents <- List.dedup (t1.parents @ into.parents);
    ok ()

end

module Store (S: IrminBranch.STORE) = struct

  module K = S.Block.Key
  module C = S.Value

  include Make(K)(C)

  type db = S.t
  type path = S.key

  let import ~parents ~contents ~node key =
    Log.debugf "import %s" (K.to_string key);
    node key >>= function
    | None   -> fail Not_found
    | Some n ->
      let node k =
        node k >>= function
        | None   -> return_none
        | Some n -> return (Some (Node.import n)) in
      let view = Node.key key in
      let ops = [] in
      return { parents; node; contents; view; ops }
  let export ~contents ~node t =
    Log.debugf "export";
    let node n =
      node (Node.export_node n) in
    let todo = Stack.create () in
    let rec add_to_todo n =
      match !n with
      | Node.Both _
      | Node.Key _  -> ()
      | Node.Node x ->
        (* 1. we push the current node job on the stack. *)
        Stack.push todo (fun () ->
            node x >>= fun k ->
            n := Node.Key k;
            return_unit
          );
        (* 2. we push the contents job on the stack. *)
        Stack.push todo (fun () ->
            match x.Node.contents with
            | None   -> return_unit
            | Some c ->
              match !c with
              | Contents.Both _
              | Contents.Key _       -> return_unit
              | Contents.Contents x  ->
                contents x >>= fun k ->
                c := Contents.Key k;
                return_unit
          );
        (* 3. we push the children jobs on the stack. *)
        String.Map.iter ~f:(fun ~key:_ ~data:n ->
            Stack.push todo (fun () -> add_to_todo n; return_unit)
          ) x.Node.succ;
    in
    let rec loop () =
      match Stack.pop todo with
      | None      -> return_unit
      | Some task -> task () >>= loop in
    add_to_todo t.view;
    loop () >>= fun () ->
    return (Node.export t.view)

  module Contents = S.Block.Contents
  module Node = S.Block.Node

  let of_path t path =
    Log.debugf "read_view %s" (IrminPath.to_string path);
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
    Log.debugf "update_view %s" (IrminPath.to_string path);
    let origin = match origin with
      | None   -> IrminOrigin.create "Update view to %s" (IrminPath.to_string path)
      | Some o -> o in
    node_of_view t view >>= fun node ->
    S.update_node t origin path node

  let origin_of_actions ?origin actions =
    match origin with
    | None ->
      let buf = Buffer.create 1024 in
      let string_of_action = Action.to_string (fun x -> "") in
      List.iter ~f:(fun a ->
          bprintf buf "- %s\n" (string_of_action a)
        ) actions;
      IrminOrigin.create "Actions:\n%s\n" (Buffer.contents buf)
    | Some o -> o

  let rebase_path ?origin t path view =
    Log.debugf "merge_view %s" (IrminPath.to_string path);
    S.read_node t []           >>= function
    | None           -> fail Not_found
    | Some head_node ->
      of_path t path                       >>= fun head_view ->
      merge view ~into:head_view           >>| fun () ->
      let origin = origin_of_actions ?origin (actions view) in
      update_path ~origin t path head_view >>= fun () ->
      ok ()

  let rebase_path_exn ?origin t path view =
    rebase_path ?origin t path view >>=
    IrminMerge.exn

  let merge_path ?origin t path view =
    Log.debugf "merge_view %s" (IrminPath.to_string path);
    S.read_node t []           >>= function
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
        S.update_commit t k >>= ok
      | Some head ->
        if List.mem view.parents head then
          S.update_commit t k >>= ok
        else
          let origin =
            IrminOrigin.create "Merge view to %s\n"
              (IrminPath.to_string path) in
          S.merge_commit t ~origin k

  let merge_path_exn ?origin t path view =
    merge_path ?origin t path view >>=
    IrminMerge.exn

end

module type S = sig
  type value
  type node
  include IrminStore.RW
    with type t = (node, value) t
     and type value := value
     and type key = IrminPath.t
  val actions: t -> value Action.t list
  val merge: t -> into:t -> unit IrminMerge.result Lwt.t
end

module type STORE = sig
  include S
  type db
  type path = IrminPath.t
  val of_path: db -> path -> t Lwt.t
  val update_path: ?origin:origin -> db -> path -> t -> unit Lwt.t
  val rebase_path: ?origin:origin -> db -> path -> t -> unit IrminMerge.result Lwt.t
  val rebase_path_exn: ?origin:origin -> db -> path -> t -> unit Lwt.t
  val merge_path: ?origin:origin -> db -> path -> t -> unit IrminMerge.result Lwt.t
  val merge_path_exn: ?origin:origin -> db -> path -> t -> unit Lwt.t
end
