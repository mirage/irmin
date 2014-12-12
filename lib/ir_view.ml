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

module Log = Log.Make(struct let section = "view" end)

(***** Actions *)

module Action (P: Tc.S0) (C: Tc.S0) = struct

  type path = P.t
  type contents = C.t

  type t =
    [ `Read of (path * contents option)
    | `Write of (path * contents option)
    | `Rmdir of path
    | `List of  (path * path list) ]

  module R = Tc.Pair( P )( Tc.Option(C) )
  module W = R
  module L = Tc.Pair( P )( Tc.List(P) )

  let compare = Pervasives.compare
  let hash = Hashtbl.hash

  let equal x y = match x, y with
    | `Read x , `Read y  -> R.equal x y
    | `Write x, `Write y -> W.equal x y
    | `Rmdir x, `Rmdir y -> P.equal x y
    | `List x , `List y  -> L.equal x y
    | _ -> false

  let to_sexp t =
    let open Sexplib.Type in
    match t with
    | `Read x  -> List [ Atom "read" ; R.to_sexp x ]
    | `Write x -> List [ Atom "write"; W.to_sexp x ]
    | `Rmdir x -> List [ Atom "rmdir"; P.to_sexp x ]
    | `List x  -> List [ Atom "list" ; L.to_sexp x ]

  let to_json = function
    | `Read x  -> `O [ "read" , R.to_json x ]
    | `Write x -> `O [ "write", W.to_json x ]
    | `Rmdir x -> `O [ "rmdir", P.to_json x ]
    | `List x  -> `O [ "list" , L.to_json x ]

  let of_json = function
    | `O [ "read" , x ] -> `Read  (R.of_json x)
    | `O [ "write", x ] -> `Write (W.of_json x)
    | `O [ "rmdir", x ] -> `Rmdir (P.of_json x)
    | `O [ "list" , x ] -> `List  (L.of_json x)
    | j -> Ezjsonm.parse_error j "View.Action.of_json"

  let write t buf = match t with
    | `Read x  -> R.write x (Ir_misc.tag buf 0)
    | `Write x -> W.write x (Ir_misc.tag buf 1)
    | `List x  -> L.write x (Ir_misc.tag buf 2)
    | `Rmdir x -> P.write x (Ir_misc.tag buf 3)

  let read buf = match Ir_misc.untag buf with
    | 0 -> `Read  (R.read buf)
    | 1 -> `Write (W.read buf)
    | 2 -> `List  (L.read buf)
    | 3 -> `Rmdir (P.read buf)
    | n -> Tc.Reader.error "View.Action.read (tag=%d)" n

  let size_of t = 1 + match t with
    | `Read x  -> R.size_of x
    | `Write x -> W.size_of x
    | `List x  -> L.size_of x
    | `Rmdir x -> P.size_of x

  let pretty t =
    let pretty_key = Tc.show (module P) in
    let pretty_val = Tc.show (module C) in
    let pretty_keys l = String.concat ", " (List.map pretty_key l) in
    let pretty_valo = function
      | None   -> "<none>"
      | Some x -> pretty_val x
    in
    match t with
    | `Read (p,x) -> sprintf "read  %s -> %s" (pretty_key p) (pretty_valo x)
    | `Write (p,x)-> sprintf "write %s <- %s" (pretty_key p) (pretty_valo x)
    | `List (i,o) -> sprintf "list  %s -> %s" (pretty_key i) (pretty_keys o)
    | `Rmdir p    -> sprintf "rmdir %s"       (pretty_key p)

  let prettys ts =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf "Actions:\n";
    List.iter (fun a ->
        bprintf buf "- %s\n" (pretty a)
      ) ts;
    Buffer.contents buf

end

(***** views *)

module type NODE = sig
  type t
  type commit
  type node
  type contents
  module Contents: Tc.S0 with type t = contents
  module Path: Ir_path.S
  module StepMap: Ir_misc.MAP with type key = Path.step
  val empty: unit -> t
  val read: t -> node option Lwt.t
  val succ: node -> t StepMap.t
  val contents: t -> contents StepMap.t Lwt.t
  val update_succ: t -> t StepMap.t -> unit Lwt.t
  val update_contents: t -> contents StepMap.t -> unit Lwt.t
end

module Internal (Node: NODE) = struct

  module Path = Node.Path
  module PathSet = Ir_misc.Set(Path)

  module StepMap = Node.StepMap

  type step = Path.step
  type key = step list
  type value = Node.contents

  module Action = Action(Path)(Node.Contents)
  type action = Action.t

  type t = {
    config: Ir_conf.t;
    task: Ir_task.t;
    view: Node.t;
    mutable ops: action list;
    mutable parents: Node.commit list;
  }

  module CO = Tc.Option(Node.Contents)
  module PL = Tc.List(Path)

  let create config task =
    Log.debugf "create";
    let view = Node.empty () in
    let ops = [] in
    let parents = [] in
    return (fun a -> { config; task = task a; parents; view; ops })

  let task t =
    (* FIXME: what if someone does multiple calls to the function ? *)
    Ir_task.add t.task (Action.prettys t.ops);
    t.task

  let config t = t.config

  let sub t path =
    let rec aux node = function
      | []   -> return (Some node)
      | h::p ->
        Node.read node >>= function
        | None -> return_none
        | Some t ->
          let succ = Node.succ t in
          try
            let v = StepMap.find h succ in
            aux v p
          with Not_found ->
            return_none
    in
    aux t.view path

  let read_aux t path =
    let path, file =
      try Ir_misc.list_end path with Not_found -> [], Path.Step.of_hum ""
    in
    sub t path >>= function
    | None   -> return_none
    | Some n ->
      Node.contents n >>= fun m ->
      try return (Some (StepMap.find file m))
      with Not_found -> return_none

  let read t k =
    read_aux t k >>= fun v ->
    t.ops <- `Read (k, v) :: t.ops;
    return v

  let read_exn t k =
    read t k >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem t k =
    read t k >>= function
    | None  -> return false
    | _     -> return true

  let list_aux t path =
    sub t path >>= function
    | None   -> return []
    | Some n ->
      Node.read n >>= function
      | None -> return []
      | Some t ->
        let succ = Node.succ t in
        Node.contents n >>= fun contents ->
        let paths x =
          List.fold_left (fun set p ->
              PathSet.add (path @ [p]) set
            ) PathSet.empty (StepMap.keys x)
        in
        let path = PathSet.union (paths succ) (paths contents) in
        return (PathSet.to_list path)

  let list t path =
    list_aux t path >>= fun result ->
    t.ops <- `List (path, result) :: t.ops;
    return result

  let iter t fn =
    Log.debugf "iter";
    let rec aux = function
      | []       -> return_unit
      | path::tl ->
        list t path >>= fun childs ->
        let todo = childs @ tl in
        fn path >>= fun () ->
        aux todo
    in
    list t [] >>= aux

  let update_opt_aux t k v =
    let rec aux view = function
      | []     -> failwith "empty path"
      | [file] ->
        Node.contents view >>= fun contents ->
        let contents = match v with
          | None   -> StepMap.remove file contents
          | Some v -> StepMap.add file v contents
        in
        Node.update_contents view contents
      | h::p   ->
        Node.read view >>= function
        | None   -> if v = None then return_unit else fail Not_found (* XXX ?*)
        | Some n ->
          try
            let child = StepMap.find h (Node.succ n) in
            aux child p
          with Not_found ->
            if v = None then return_unit
            else
              let child = Node.empty () in
              let succ = StepMap.add h child (Node.succ n) in
              Node.update_succ view succ >>= fun () ->
              aux child p in
    aux t.view k

  let update_opt t k v =
    t.ops <- `Write (k, v) :: t.ops;
    update_opt_aux t k v

  let update t k v =
    update_opt t k (Some v)

  let remove t k =
    update_opt t k None

  let remove_rec t k =
    let rec aux view = function
      | []    -> failwith "empty path"
      | [dir] -> begin
          Node.read view >>= function
          | None      -> return_unit
          | Some node ->
            let succ = Node.succ node in
            let succ = StepMap.remove dir succ in
            Node.update_succ view succ
        end
      | h::p ->
        Node.read view >>= function
        | None   -> return_unit
        | Some n ->
          try
            let child = StepMap.find h (Node.succ n) in
            aux child p
          with Not_found ->
            return_unit
    in
    t.ops <- `Rmdir k :: t.ops;
    aux t.view k

  let watch _ =
    failwith "TODO: View.watch"

  let apply t a =
    Log.debugf "apply %a" force (show (module Action) a);
    match a with
    | `Rmdir _ -> ok ()
    | `Write (k, v) -> update_opt t k v >>= ok
    | `Read (k, v)  ->
      read t k >>= fun v' ->
      if Tc.equal (module CO) v v' then ok ()
      else
        let str = function
          | None   -> "<none>"
          | Some c -> Tc.show (module Node.Contents) c in
        conflict "read %s: got %S, expecting %S"
          (Tc.show (module Path) k) (str v') (str v)
    | `List (l, r) ->
      list t l >>= fun r' ->
      if Tc.equal (module PL) r r' then ok ()
      else
        let one = Tc.show (module Path) in
        let many = Ir_misc.list_pretty one in
        conflict "list %s: got %s, expecting %s" (one l) (many r') (many r)

  let actions t =
    List.rev t.ops

  let merge t1 ~into =
    Ir_merge.iter (apply into) (List.rev t1.ops) >>| fun () ->
    into.parents <- Ir_misc.list_dedup (t1.parents @ into.parents);
    ok ()

  let merge_exn t1 ~into =
    merge t1 ~into >>=
    Ir_merge.exn

end

module Make (S: Ir_s.STORE) = struct

  module B = Ir_bc.Make_ext(S.Private)
  module P = S.Private

  module Graph = Ir_node.Graph(P.Contents)(P.Node)
  module History = Ir_commit.History(Graph.Store)(P.Commit)

  let graph_t t = P.contents_t t, P.node_t t
  let history_t t = graph_t t, P.commit_t t

  module Contents = struct

    type key = S.t * B.Private.Contents.key

    type contents_or_key =
      | Key of key
      | Contents of B.value
      | Both of key * B.value

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
      | Contents c -> return (Some c)
      | Key (db, k as key) ->
        P.Contents.read (P.contents_t db) k >>= function
        | None   -> return_none
        | Some c ->
          t := Both (key, c);
          return (Some c)

  end

  module Node = struct

    module Path = S.Key
    module StepMap = Ir_misc.Map(Path.Step)

    type contents = S.value
    type commit = S.head
    type key = S.t * P.Node.key

    type node = {
      contents: Contents.t StepMap.t;
      succ    : t StepMap.t;
    }

    and node_or_key  =
      | Key of key
      | Node of node
      | Both of key * node

    and t = node_or_key ref
    (* Similir to [Node.t] but using where all of the values can just
       be keys. *)

    let succ t = t.succ

    let create' contents succ =
      Node { contents; succ }

    let create contents succ =
      ref (create' contents succ)

    let key db k =
      ref (Key (db, k))

    let both db k v =
      ref (Both ((db, k), v))

    let empty () =
      create StepMap.empty StepMap.empty

    let import t n =
      let contents = ref StepMap.empty in
      P.Node.Val.iter_contents n (fun l k ->
          contents := StepMap.add l (Contents.key t k) !contents
        );
      let succ = ref StepMap.empty in
      P.Node.Val.iter_succ n (fun l k ->
          succ := StepMap.add l (key t k) !succ
        );
      { contents = !contents; succ = !succ }

    let export n =
      match !n with
      | Both ((_, k), _)
      | Key (_, k)  -> k
      | Node _ -> failwith "Node.export"

    let export_node n =
      let contents =
        StepMap.map Contents.export n.contents
        |> StepMap.to_alist
      in
      let succ =
        StepMap.map export n.succ
        |> StepMap.to_alist
      in
      P.Node.Val.create ~contents ~succ

    let read t =
      match !t with
      | Both (_, n)
      | Node n   -> return (Some n)
      | Key (db, k) ->
        P.Node.read (P.node_t db) k >>= function
        | None   -> return_none
        | Some n ->
          let n = import db n in
          t := Both ((db, k), n);
          return (Some n)

    let contents t =
      read t >>= function
      | None   -> return StepMap.empty
      | Some c ->
        StepMap.fold (fun l k acc ->
            Contents.read k >>= function
            | None   -> acc
            | Some c -> acc >>= fun m -> return (StepMap.add l c m)
          ) c.contents (return StepMap.empty)

    let update_contents t contents =
      read t >>= function
      | None -> if
        StepMap.is_empty contents then return_unit else fail Not_found (* ? *)
      | Some n ->
        let contents = StepMap.map Contents.create contents in
        let new_n = { n with contents } in
        t := Node new_n;
        return_unit

    let update_succ t succ =
      read t >>= function
      | None ->
        if StepMap.is_empty succ then return_unit else fail Not_found (* ? *)
      | Some n ->
        let new_n = { n with succ } in
        t := Node new_n;
        return_unit

    module Contents = P.Contents.Val

  end

  include Internal(Node)

  type db = S.t

  let import db ~parents key =
    Log.debugf "import %a" force (show (module P.Node.Key) key);
    P.Node.read (P.node_t db) key >>= function
    | None   -> fail Not_found
    | Some n ->
      let view = Node.both db key (Node.import db n) in
      let ops = [] in
      let task = S.task db in
      let config = S.config db in
      return { config; task; parents; view; ops }

  let export db t =
    Log.debugf "export";
    let node n = P.Node.add (P.node_t db) (Node.export_node n) in
    let todo = Stack.create () in
    let rec add_to_todo n =
      match !n with
      | Node.Both _
      | Node.Key _  -> ()
      | Node.Node x ->
        (* 1. we push the current node job on the stack. *)
        Stack.push (fun () ->
            node x >>= fun k ->
            n := Node.Key (db, k);
            return_unit
          ) todo;
        (* 2. we push the contents job on the stack. *)
        StepMap.iter (fun _ c ->
            match !c with
            | Contents.Both _
            | Contents.Key _       -> ()
            | Contents.Contents x  ->
              Stack.push (fun () ->
                  P.Contents.add (P.contents_t db) x >>= fun k ->
                  c := Contents.Key (db, k);
                  return_unit
                ) todo
          ) x.Node.contents;
        (* 3. we push the children jobs on the stack. *)
        StepMap.iter (fun _ n ->
            Stack.push (fun () -> add_to_todo n; return_unit) todo
          ) x.Node.succ;
    in
    let rec loop () =
      let task =
        try Some (Stack.pop todo)
        with Stack.Empty -> None
      in
      match task with
      | None   -> return_unit
      | Some t -> t () >>= loop
    in
    add_to_todo t.view;
    loop () >>= fun () ->
    return (Node.export t.view)

  let of_path db path =
    Log.debugf "read_view %a" force (show (module Path) path);
    P.read_node db path >>= function
    | None   ->
      create (S.config db) (fun () -> S.task db) >>= fun t ->
      return (t ())
    | Some n ->
      begin S.head db >>= function
        | None   -> return_nil
        | Some h -> return [h]
      end >>= fun parents ->
      import db ~parents n

  let update_path db path view =
    Log.debugf "update_view %a" force (show (module Path) path);
    export db view >>= fun node ->
    P.update_node db path node

  let rebase_path db path view =
    Log.debugf "merge_view %a" force (show (module Path) path);
    P.mem_node db [] >>= function
    | false -> fail Not_found
    | true  ->
      of_path db path >>= fun head_view ->
      merge view ~into:head_view >>| fun () ->
      update_path db path head_view >>= fun () ->
      ok ()

  let rebase_path_exn db path view =
    rebase_path db path view >>=
    Ir_merge.exn

  let merge_path db path view =
    Log.debugf "merge_view %a" force (show (module Path) path);
    P.read_node db [] >>= function
    | None           -> fail Not_found
    | Some head_node ->
      (* First, we check than we can rebase the view on the current
         HEAD. *)
      of_path db path >>= fun head_view ->
      merge view ~into:head_view >>| fun () ->
      (* Now that we know that rebasing is possible, we discard the
         result and proceed as a normal merge, ie. we apply the view
         on a branch, and we merge the branch back into the store. *)
      export db view >>= fun view_node ->
      (* Create a commit with the contents of the view *)
      Graph.add_node (graph_t db) head_node path view_node >>= fun new_node ->
      let parents = view.parents in
      History.commit (history_t db) ~node:new_node ~parents >>= fun k ->
      (* We want to avoid to create a merge commit when the HEAD has
         not been updated since the view has been created. *)
      S.head db >>= function
      | None ->
        (* The store is empty, create a fresh commit. *)
        S.update_head db k >>= ok
      | Some head ->
        if List.mem head view.parents then
          S.update_head db k >>= ok
        else
          S.merge_head db k

  let merge_path_exn db path t =
    merge_path db path t >>=
    Ir_merge.exn

end

module type S = sig
  include Ir_rw.HIERARCHICAL
  val merge: t -> into:t -> unit Ir_merge.result Lwt.t
  val merge_exn: t -> into:t -> unit Lwt.t
  type db
  val of_path: db -> key -> t Lwt.t
  val update_path: db -> key -> t -> unit Lwt.t
  val rebase_path: db -> key -> t -> unit Ir_merge.result Lwt.t
  val rebase_path_exn: db -> key -> t -> unit Lwt.t
  val merge_path: db -> key -> t -> unit Ir_merge.result Lwt.t
  val merge_path_exn: db -> key -> t -> unit Lwt.t
  module Action: sig
    type t =
      [ `Read of (key * value option)
      | `Write of (key * value option)
      | `Rmdir of key
      | `List of (key * key list) ]
    include Tc.S0 with type t := t
    val pretty: t -> string
    val prettys: t list -> string
  end
  val actions: t -> Action.t list
end
