(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2016 Grégoire Henry <gregoire.henry@ocamlpro.com>
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

open Lwt.Infix

(***** views *)

module type NODE = sig
  type t
  type node
  type contents
  module Contents: Tc.S0 with type t = contents
  module Path: Ir_s.PATH

  val equal: t -> t -> bool
  val empty: unit -> t
  val is_empty: t -> bool Lwt.t

  val read: t -> node option Lwt.t

  val read_contents: t -> Path.step -> contents option Lwt.t

  val with_contents: t -> Path.step -> contents option -> t option Lwt.t
  (* Return [true] iff the contents has actually changed. Used for
     invalidating the view cache if needed. *)

  val read_succ: node -> Path.step -> t option

  val with_succ: t -> Path.step -> t option -> t option Lwt.t
  (* Return [true] iff the successors has actually changes. Used for
     invalidating the view cache if needed. *)

  val steps: t -> Path.step list Lwt.t
end

module Internal (Node: NODE) = struct

  module Path = Node.Path
  module PathSet = Ir_misc.Set(Path)

  type key = Path.t
  type value = Node.contents

  type t = [`Empty | `Node of Node.t | `Contents of Node.contents]

  let equal x y = match x, y with
    | `Node x, `Node y -> Node.equal x y
    | `Contents x, `Contents y -> Node.Contents.equal x y
    | _ -> false

  module CO = Tc.Option(Node.Contents)
  module PL = Tc.List(Path)

  let empty = `Empty

  let sub t path =
    let rec aux node path =
      match Path.decons path with
      | None        -> Lwt.return (Some node)
      | Some (h, p) ->
        Node.read node >>= function
        | None -> Lwt.return_none
        | Some t ->
          match Node.read_succ t h with
          | None   -> Lwt.return_none
          | Some v -> aux v p
    in
    match t with
    | `Empty      -> Lwt.return_none
    | `Node n     -> aux n path
    | `Contents _ -> Lwt.return_none

  let read_contents t path =
    match t, Path.rdecons path with
    | `Contents c, None -> Lwt.return (Some c)
    | _          , None -> Lwt.return_none
    | _          , Some (path, file) ->
      sub t path >>= function
      | None   -> Lwt.return_none
      | Some n -> Node.read_contents n file

  let read t k = read_contents t k

  let err_not_found n k =
    Ir_misc.invalid_arg "Irmin.View.%s: %s not found" n (Path.to_hum k)

  let read_exn t k =
    read t k >>= function
    | None   -> err_not_found "read" k
    | Some v -> Lwt.return v

  let mem t k =
    read t k >>= function
    | None  -> Lwt.return false
    | _     -> Lwt.return true

  let list_aux t path =
    sub t path >>= function
    | None   -> Lwt.return []
    | Some n ->
      Node.steps n >>= fun steps ->
      let paths =
        List.fold_left (fun set p ->
            PathSet.add (Path.rcons path p) set
          ) PathSet.empty steps
      in
      Lwt.return (PathSet.to_list paths)

  let list t path =
    list_aux t path

  let iter t fn =
    let rec aux = function
      | []       -> Lwt.return_unit
      | path::tl ->
        list t path >>= fun childs ->
        let todo = childs @ tl in
        mem t path >>= fun exists ->
        begin
          if not exists then Lwt.return_unit
          else fn path (fun () -> read_exn t path)
        end >>= fun () ->
        aux todo
    in
    (* FIXME take lock? *)
    list t Path.empty >>= aux

  let update_contents_aux t k v =
    match Path.rdecons k with
    | None -> begin
        match t, v with
        | `Empty, None -> Lwt.return t
        | `Contents c, Some v when Node.Contents.equal c v -> Lwt.return t
        | _, None -> Lwt.return `Empty
        | _, Some c -> Lwt.return (`Contents c)
      end
    | Some (path, file) ->
      let rec aux view path =
        match Path.decons path with
        | None        -> Node.with_contents view file v
        | Some (h, p) ->
          Node.read view >>= function
          | None ->
            if v = None then Lwt.return_none
            else err_not_found "update_contents" k (* XXX ?*)
          | Some n ->
            match Node.read_succ n h with
            | Some child -> begin
              aux child p >>= function
              | None -> Lwt.return_none
              | Some child -> begin
                  if v = None then
                    (* remove empty dirs *)
                    Node.is_empty child >>= function
                    | true  -> Lwt.return_none
                    | false -> Lwt.return (Some child)
                  else
                    Lwt.return (Some child)
                end >>= fun child ->
                Node.with_succ view h child
              end
            | None ->
              if v = None then
                Lwt.return_none
              else
                aux (Node.empty ()) p >>= function
                | None -> assert false
                | Some _ as child -> Node.with_succ view h child
      in
      let n = match t with `Node n -> n | _ -> Node.empty () in
      aux n path >>= function
      | None -> Lwt.return t
      | Some node ->
        Node.is_empty node >>= function
        | true  -> Lwt.return `Empty
        | false -> Lwt.return (`Node node)

  let update_contents t k v =
    update_contents_aux t k v

  let update t k v = update_contents t k (Some v)

  let remove t k = update_contents t k None

  let remove_rec t k =
    match Path.decons k with
    | None -> Lwt.return t
    | _    ->
      match t with
      | `Contents _ -> Lwt.return `Empty
      | `Empty -> Lwt.return t
      | `Node n ->
        let rec aux view path =
          match Path.decons path with
          | None       -> assert false
          | Some (h,p) ->
            if Path.is_empty p then
              Node.with_succ view h None
            else
              Node.read view >>= function
              | None   -> Lwt.return_none
              | Some n ->
                match Node.read_succ n h with
                | None       -> Lwt.return_none
                | Some child -> aux child p
        in
        aux n k >>= function
      | None -> Lwt.return t
      | Some node ->
        Node.is_empty node >>= function
        | true  -> Lwt.return `Empty
        | false -> Lwt.return (`Node node)

end

module Make (S: Ir_s.STORE_EXT) = struct

  module P = S.Private

  module Metadata = P.Node.Val.Metadata

  module Contents = struct

    type key = S.Repo.t * S.Private.Contents.key

    type contents_or_key =
      | Key of key
      | Contents of S.value
      | Both of key * S.value

    type t = contents_or_key ref
    (* Same as [Contents.t] but can either be a raw contents or a key
       that will be fetched lazily. *)

    let create c =
      ref (Contents c)

    let export c =
      match !c with
      | Both ((_, k), _)
      | Key (_, k) -> k
      | Contents _ -> failwith "Contents.export"

    let key db k =
      ref (Key (db, k))

    let read t =
      match !t with
      | Both (_, c)
      | Contents c -> Lwt.return (Some c)
      | Key (db, k as key) ->
        P.Contents.read (P.Repo.contents_t db) k >>= function
        | None   -> Lwt.return_none
        | Some c ->
          t := Both (key, c);
          Lwt.return (Some c)

    let equal (x:t) (y:t) =
      x == y
      ||
      match !x, !y with
      | (Key (_,x) | Both ((_,x),_)), (Key (_,y) | Both ((_,y),_)) ->
        P.Contents.Key.equal x y
      | (Contents x | Both (_, x)), (Contents y | Both (_, y)) ->
        P.Contents.Val.equal x y
      | _ -> false

  end

  module Node = struct

    module Path = S.Key

    module Step = Path.Step
    module StepMap = Ir_misc.Map(Path.Step)
    module StepSet = Ir_misc.Set(Path.Step)

    type contents = S.value
    (* type commit_id = S.commit_id *)
    type key = S.Repo.t * P.Node.key

    (* XXX: fix code duplication with Ir_node.Graph (using
       functors?) *)
    type node = {
      contents: Contents.t StepMap.t Lazy.t;
      succ    : t StepMap.t Lazy.t;
      alist   : (Path.step * [`Contents of Contents.t | `Node of t ]) list;
    }

    and t = {
      mutable node: node option ;
      mutable key: key option ;
    }

    let rec equal (x:t) (y:t) =
      match x, y with
      | { key = Some (_,x) ; _ }, { key = Some (_,y) ; _ } ->
        P.Node.Key.equal x y
      | { node = Some x ; _ }, { node = Some y ; _ } ->
        List.length x.alist = List.length y.alist
        && List.for_all2 (fun (s1, n1) (s2, n2) ->
            Step.equal s1 s2
            && match n1, n2 with
            | `Contents n1, `Contents n2 -> Contents.equal n1 n2
            | `Node n1, `Node n2 -> equal n1 n2
            | _ -> false) x.alist y.alist
      | _ -> false

    let mk_index alist =
      lazy (
        List.fold_left (fun (contents, succ) (l, x) ->
            match x with
            | `Contents c -> StepMap.add l c contents, succ
            | `Node n     -> contents, StepMap.add l n succ
          ) (StepMap.empty, StepMap.empty) alist
      )

    let create_node alist =
      let maps = mk_index alist in
      let contents = lazy (fst (Lazy.force maps)) in
      let succ = lazy (snd (Lazy.force maps)) in
      { contents; succ; alist }

    let create alist =
      { key = None ; node = Some (create_node alist) }

    let key db k =
      { key = Some (db, k) ; node = None }

    let both db k v =
      { key = Some (db, k) ; node = Some v }

    let empty () = create []

    let import t n =
      let alist = P.Node.Val.alist n in
      let alist = List.map (fun (l, x) ->
          match x with
          | `Contents (c, _meta) -> (l, `Contents (Contents.key t c))
          | `Node n     -> (l, `Node (key t n))
        ) alist in
      create_node alist

    let export n =
      match n.key with
      | Some (_, k) -> k
      | None -> failwith "Node.export"

    let export_node n =
      let alist = List.map (fun (l, x) ->
          match x with
          | `Contents c -> (l, `Contents (Contents.export c, Metadata.default))
          | `Node n     -> (l, `Node (export n))
        ) n.alist
      in
      P.Node.Val.create alist

    let read t =
      match t with
      | { key = None ; node = None } -> assert false
      | { node = Some n ; _ } -> Lwt.return (Some n)
      | { key = Some (db, k) ; _ } ->
        P.Node.read (P.Repo.node_t db) k >>= function
        | None   -> Lwt.return_none
        | Some n ->
          let n = import db n in
          t.node <- Some n;
          Lwt.return (Some n)

    let is_empty t =
      read t >>= function
      | None   -> Lwt.return false
      | Some n -> Lwt.return (n.alist = [])

    let steps t =
      read t >>= function
      | None    -> Lwt.return_nil
      | Some  n ->
        let steps = ref StepSet.empty in
        List.iter (fun (l, _) -> steps := StepSet.add l !steps) n.alist;
        Lwt.return (StepSet.to_list !steps)

    let read_contents t step =
      read t >>= function
      | None   -> Lwt.return_none
      | Some t ->
        try
          StepMap.find step (Lazy.force t.contents)
          |> Contents.read
        with Not_found ->
          Lwt.return_none

    let read_succ t step =
      try Some (StepMap.find step (Lazy.force t.succ))
      with Not_found -> None

    (* FIXME code duplication with Ir_node.Make.with_contents *)
    let with_contents t step contents =
      let mk c = `Contents (Contents.create c) in
      read t >>= function
      | None -> begin
          match contents with
          | None   -> Lwt.return_none
          | Some c -> Lwt.return (Some (create [ step, mk c ]))
        end
      | Some n ->
        let rec aux acc = function
          | (s, `Contents x as h) :: l ->
            if Step.equal step s then match contents with
              | None   -> List.rev_append acc l
              | Some c ->
                if Contents.equal (Contents.create c) x then n.alist
                else List.rev_append acc ((s, mk c) :: l)
            else aux (h :: acc) l
          | h::t -> aux (h :: acc) t
          | []   -> match contents with
            | None   -> n.alist
            | Some c -> List.rev ((step, mk c) :: acc)
        in
        let alist = aux [] n.alist in
        if n.alist != alist then
          Lwt.return (Some (create alist))
        else
          Lwt.return_none

    (* FIXME: code duplication with Ir_node.Make.with_succ *)
    let with_succ t step succ =
      let mk c = `Node c in
      read t >>= function
      | None -> begin
          match succ with
          | None   -> Lwt.return_none
          | Some c -> Lwt.return (Some (create [ step, mk c ]))
        end
      | Some n ->
        let rec aux acc = function
          | (s, `Node x as h) :: l ->
            if Step.equal step s then match succ with
              | None   -> List.rev_append acc l
              | Some c ->
                if equal c x then n.alist
                else List.rev_append acc ((s, mk c) :: l)
            else aux (h :: acc) l
          | h::t -> aux (h :: acc) t
          | []   -> match succ with
            | None   -> n.alist
            | Some c -> List.rev ((step, mk c) :: acc)
        in
        let alist = aux [] n.alist in
        if n.alist != alist then
          Lwt.return (Some (create alist))
        else
          Lwt.return_none

    module Contents = P.Contents.Val

  end

  include Internal(Node)

  type db = S.t

  let import db key =
    let repo = S.repo db in
    begin P.Node.read (P.Repo.node_t repo) key >|= function
    | None   -> `Empty
    | Some n -> `Node (Node.both repo key (Node.import repo n))
    end

  let export db t =
    let repo = S.repo db in
    let node n = P.Node.add (P.Repo.node_t repo) (Node.export_node n) in
    let todo = Stack.create () in
    let rec add_to_todo n =
      match n with
      | { Node.key = Some _ ; _ } -> ()
      | { Node.key = None ; node = None } -> assert false
      | { Node.key = None ; node = Some x } ->
        (* 1. we push the current node job on the stack. *)
        Stack.push (fun () ->
            node x >>= fun k ->
            n.Node.key <- Some (repo, k);
            n.Node.node <- None; (* Clear cache ?? *)
            Lwt.return_unit
          ) todo;
        (* 2. we push the contents job on the stack. *)
        List.iter (fun (_, x) ->
            match x with
            | `Node _ -> ()
            | `Contents c ->
              match !c with
              | Contents.Both _
              | Contents.Key _       -> ()
              | Contents.Contents x  ->
                Stack.push (fun () ->
                    P.Contents.add (P.Repo.contents_t repo) x >>= fun k ->
                    c := Contents.Key (repo, k);
                    Lwt.return_unit
                  ) todo
          ) x.Node.alist;
        (* 3. we push the children jobs on the stack. *)
        List.iter (fun (_, x) ->
            match x with
            | `Contents _ -> ()
            | `Node n ->
              Stack.push (fun () -> add_to_todo n; Lwt.return_unit) todo
          ) x.Node.alist;
    in
    let rec loop () =
      let task =
        try Some (Stack.pop todo)
        with Stack.Empty -> None
      in
      match task with
      | None   -> Lwt.return_unit
      | Some t -> t () >>= loop
    in
    match t with
    | `Empty      -> Lwt.return `Empty
    | `Contents c -> Lwt.return (`Contents c)
    | `Node n ->
      add_to_todo n;
      loop () >|= fun () ->
      `Node (Node.export n)

  let of_path db path =
    P.read_node db path >>= function
    | None   -> Lwt.return `Empty
    | Some n -> import db n

  let update_path db path view =
    export db view >>= function
    | `Empty      -> P.remove_node db path
    | `Contents c -> S.update db path c
    | `Node node  -> P.update_node db path node

  module Private = struct
    type key = S.Private.Node.key
    let import = import
    let export = export
    module Path = Path
    module Contents = P.Contents.Val
  end

end

module type S = sig
  include Ir_s.RO_STORE
  val update: t -> key -> value -> t Lwt.t
  val remove: t -> key -> t Lwt.t
  val list: t -> key -> key list Lwt.t
  val remove_rec: t -> key -> t Lwt.t
  val empty: t
  val equal: t -> t -> bool
  type db
  val of_path: db -> key -> t Lwt.t
  val update_path: db -> key -> t -> unit Lwt.t
  module Private : sig
    type key
    val import: db -> key -> t Lwt.t
    val export: db -> t -> [> `Contents of value | `Empty | `Node of key ] Lwt.t
    module Path : Ir_s.PATH
    module Contents: Tc.S0 with type t = value
  end
end
