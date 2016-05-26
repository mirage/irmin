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
open Ir_merge.OP
open Ir_misc.OP
open Printf


let src = Logs.Src.create "irmin.view" ~doc:"Irmin transactions"
module Log = (val Logs.src_log src : Logs.LOG)

(* TODO: the View interface should export file metadata (e.g. Git file type).
   Search for [_meta] and [Metadata.default] for places that may need to
   change. *)

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

module Make (S: Ir_s.STORE_EXT) = struct

  module P = S.Private

  module FunView = Ir_funview.Make(S)
  module Contents = FunView.Private.Contents

  module Graph = Ir_node.Graph(P.Node)
  module History = Ir_commit.History(P.Commit)
  module Metadata = P.Node.Val.Metadata

  let graph_t t = P.Repo.node_t t
  let history_t t = P.Repo.commit_t t

  module Path = S.Key
  module PathSet = Ir_misc.Set(Path)
  module PathMap = Ir_misc.Map(Path)

  module Step = Path.Step
  module StepMap = Ir_misc.Map(Path.Step)
  module StepSet = Ir_misc.Set(Path.Step)

  type key = Path.t
  type value = FunView.value
  type commit_id = S.commit_id

  module Action = Action(Path)(Contents)
  type action = Action.t

  type t = {
    mutable view: FunView.t;
    ops: action list ref;
    parents: commit_id list ref;
    lock: Lwt_mutex.t;
  }

  let equal x y = FunView.equal x.view y.view
  let parents t = !(t.parents)

  module CO = Tc.Option(Contents)
  module PL = Tc.List(Path)

  let empty () =
    Log.debug (fun f -> f "empty");
    let view = FunView.empty in
    let ops = ref [] in
    let parents = ref [] in
    let lock = Lwt_mutex.create () in
    Lwt.return { parents; view; ops; lock }

  let read t k =
    FunView.read t.view k >>= fun v ->
    t.ops := `Read (k, v) :: !(t.ops);
    Lwt.return v

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

  let list t path =
    Log.debug (fun f -> f "list %a" (show (module Path)) path);
    FunView.list t.view path >>= fun result ->
    t.ops := `List (path, result) :: !(t.ops);
    Lwt.return result

  let iter t fn =
    Log.debug (fun f -> f "iter");
    (* FIXME take lock? *)
    FunView.iter t.view fn

  let update_contents t k v =
    t.ops := `Write (k, v) :: !(t.ops);
    match v with
    | None ->
      FunView.remove t.view k >>= fun view -> t.view <- view ;
      Lwt.return_unit
    | Some v ->
      FunView.update t.view k v >>= fun view -> t.view <- view ;
      Lwt.return_unit

  let update t k v =
    Lwt_mutex.with_lock t.lock (fun () -> update_contents t k (Some v))

  let remove t k =
    Lwt_mutex.with_lock t.lock (fun () -> update_contents t k None)
  let compare_and_set t k ~test ~set =
    Lwt_mutex.with_lock t.lock (fun () ->
        read t k >>= fun v ->
        if Tc.O1.equal Contents.equal test v then
          update_contents t k set >>= fun () ->
          Lwt.return_true
        else
          Lwt.return_false
      )

  let remove_rec t k =
    Lwt_mutex.with_lock t.lock (fun () ->
        t.ops := `Rmdir k :: !(t.ops);
        FunView.remove_rec t.view k >>= fun view ->
        t.view <- view ;
        Lwt.return_unit)

  let apply t a =
    Log.debug (fun f -> f "apply %a" (show (module Action)) a);
    match a with
    | `Rmdir _ -> ok ()
    | `Write (k, v) -> update_contents t k v >>= fun _ -> ok ()
    | `Read (k, v)  ->
      read t k >>= fun v' ->
      if Tc.equal (module CO) v v' then ok ()
      else
        let str = function
          | None   -> "<none>"
          | Some c -> Tc.show (module Contents) c in
        conflict "read %s: got %S, expecting %S"
          (Tc.show (module Path) k) (str v') (str v)
    | `List (l, r) ->
      list t l >>= fun r' ->
      if Tc.equal (module PL) r r' then ok ()
      else
        let one = Tc.show (module Path) in
        let many = Ir_misc.list_pretty one in
        conflict "list %s: got %s, expecting %s" (one l) (many r') (many r)

  let actions t = List.rev !(t.ops)

  module KV = Ir_misc.Set(Tc.Pair(Path)(Contents))

  let diff x y =
    let set t =
      let acc = ref KV.empty in
      iter t (fun k v ->
          v () >>= fun v ->
          acc := KV.add (k, v) !acc;
          Lwt.return_unit
        ) >>= fun () ->
      Lwt.return !acc
    in
    (* FIXME very dumb and slow *)
    set x >>= fun sx ->
    set y >>= fun sy ->
    let added     = KV.diff sy sx in
    let removed   = KV.diff sx sy in
    let added_l   = KV.elements added in
    let removed_l = KV.elements removed in
    let added_m   = PathMap.of_alist added_l in
    let removed_m = PathMap.of_alist removed_l in
    let added_p   = PathSet.of_list (List.map fst added_l) in
    let removed_p = PathSet.of_list (List.map fst removed_l) in

    let updated_p = PathSet.inter added_p removed_p in
    let added_p   = PathSet.diff added_p updated_p in
    let removed_p = PathSet.diff removed_p updated_p in
    let added =
      PathSet.fold (fun path acc ->
          (path, `Added (PathMap.find path added_m)) :: acc
        ) added_p []
    in
    let removed =
      PathSet.fold (fun path acc ->
          (path, `Removed (PathMap.find path removed_m)) :: acc
        ) removed_p []
    in
    let updated =
      PathSet.fold (fun path acc ->
          let x = PathMap.find path removed_m in
          let y = PathMap.find path added_m in
          (path, `Updated (x, y)) :: acc
        ) updated_p []
    in
    Lwt.return (added @ updated @ removed)

  let rebase t1 ~into =
    Ir_merge.iter (apply into) (actions t1) >>| fun () ->
    into.parents := Ir_misc.list_dedup (!(t1.parents) @ !(into.parents));
    ok ()

  let rebase_exn t1 ~into = rebase t1 ~into >>= Ir_merge.exn

  type db = S.t

  let create_with_parents parents =
    Log.debug (fun f -> f "create_with_parents");
    let view = FunView.empty in
    let ops = ref [] in
    let parents = ref parents in
    let lock = Lwt_mutex.create () in
    { parents; view; ops; lock }

  let import db ~parents key =
    Log.debug (fun f -> f "import %a" (show (module P.Node.Key)) key);
    FunView.Private.import db key >>= fun view ->
    let ops = ref [] in
    let parents = ref parents in
    let lock = Lwt_mutex.create () in
    Lwt.return { parents; view; ops; lock }

  let export repo t =
    Log.debug (fun f -> f "export");
    FunView.Private.export repo t

  let of_path db path =
    Log.debug (fun f -> f "of_path %a" (show (module Path)) path);
    begin S.head db >>= function
      | None   -> Lwt.return_nil
      | Some h -> Lwt.return [h]
    end >>= fun parents ->
    P.read_node db path >>= function
    | None   -> Lwt.return (create_with_parents parents)
    | Some n -> import db ~parents n

  let update_path db path view =
    Log.debug (fun f -> f "update_path %a" (show (module Path)) path);
    FunView.Private.export db view.view >>= function
    | `Empty      -> P.remove_node db path
    | `Contents c -> S.update db path c
    | `Node node  -> P.update_node db path node

  let rebase_path db path view =
    Log.debug (fun f -> f "rebase_path %a" (show (module Path)) path);
    of_path db path >>= fun head_view ->
    rebase view ~into:head_view >>| fun () ->
    update_path db path head_view >>= fun () ->
    ok ()

  let rebase_path_exn db path view =
    rebase_path db path view >>= Ir_merge.exn

  let merge_node db ?max_depth ?n path view head_node view_node =
    let task = S.task db in
    let repo = S.repo db in
    (* Create a commit with the contents of the view *)
    Graph.read_node (graph_t repo) head_node path >>= fun current_node ->
    let old () = match !(view.parents) with
      | []      -> ok None
      | parents ->
        History.lca (history_t repo) ~task ?max_depth ?n parents
        >>| function
        | None   -> ok None
        | Some c ->
          History.node (history_t repo) c >>= fun n ->
          Graph.read_node (graph_t repo) n path >>= fun n ->
          ok (Some n)
    in
    P.Node.(merge Path.empty) (graph_t repo) ~old current_node (Some view_node)
    >>| fun merge_node ->
    if Tc.O1.equal P.Node.Key.equal merge_node current_node then ok `Unchanged
    else (
      begin match merge_node with
        | None   -> Graph.remove_node (graph_t repo) head_node path
        | Some n -> Graph.add_node (graph_t repo) head_node path n
      end >>= fun new_head_node ->
      S.head_exn db >>= fun head ->
      let parents = head :: !(view.parents) in
      History.create (history_t repo) ~node:new_head_node ~parents ~task >>= fun h ->
      ok (`Changed h)
    )

(*
  let merge_contents n ?max_depth ?n path view head_node view_contents =
    Graph.add_contents (graph_t db) head_node path >>= fun current_contents ->
    S.heads_exn db >>= fun head ->
*)

  let merge_path_one db0 ?max_depth ?n path view =
    Log.debug (fun f -> f "merge_path %a" (show (module Path)) path);
    S.head db0 >>= function
    | None      ->
      (* FIXME: race to update the store's head *)
      update_path db0 path view >>= fun () -> ok true
    | Some head ->
      let repo = S.repo db0 in
      S.of_commit_id (fun () -> S.task db0) head repo >>= fun db ->
      let db = db () in
      P.read_node db Path.empty >>= function
      | None           -> update_path db path view >>= fun () -> ok true
      | Some head_node ->
        (* First, we check than we can rebase the view on the current
           HEAD. *)
        of_path db path >>= fun head_view ->
        rebase view ~into:head_view >>| fun () ->
        (* Now that we know that rebasing is possible, we discard the
           result and proceed as a normal merge, ie. we apply the view
           on a branch, and we merge the branch back into the
           store. *)
        let merge () =
          export db view.view >>= function
          | `Node node -> merge_node db ?max_depth ?n path view head_node node
          | _ -> assert false
        in
        merge () >>| function
        | `Unchanged -> ok true
        | `Changed h ->
          S.compare_and_set_head db0 ~test:(Some head) ~set:(Some h) >>= ok

  let retry_merge name fn =
    let rec aux i =
      fn () >>= function
      | `Conflict _ as c -> Lwt.return c
      | `Ok true  -> ok ()
      | `Ok false ->
        Log.debug (fun f -> f "Irmin.View.%s: conflict, retrying (%d)." name i);
        aux (i+1)
    in
    aux 1

  let merge_path db ?max_depth ?n path view =
    retry_merge "merge_path" (fun () ->
        merge_path_one db ?max_depth ?n path view
      )

  let merge_path_exn db ?max_depth ?n path t =
    merge_path db ?max_depth ?n path t >>= Ir_merge.exn

  let err_value_at_root () =
    Ir_misc.invalid_arg
      "Irmin.View.make_head: cannot create a head with contents at the root."

  let make_head db task ~parents ~contents =
    let repo = S.repo db in
    let empty () =
      S.Private.Node.add (S.Private.Repo.node_t repo) (S.Private.Node.Val.empty)
    in
    let node =
      export db contents.view >>= function
      | `Contents _ -> err_value_at_root ()
      | `Empty      -> empty ()
      | `Node n     -> Lwt.return n
    in
    node >>= fun node ->
    let commit = S.Private.Commit.Val.create task ~parents ~node in
    S.Private.Commit.add (S.Private.Repo.commit_t repo) commit

  let watch_path db key ?init fn =
    let view_of_head h =
        S.of_commit_id (fun () -> S.task db) h (S.repo db) >>= fun db ->
        of_path (db ()) key
    in
    let init = match init with
      | None        -> None
      | Some (h, _) -> Some h
    in
    S.watch_head db ?init (function
        | `Removed h ->
          view_of_head h >>= fun v ->
          fn @@ `Removed (h, v)
        | `Added h ->
          view_of_head h >>= fun v ->
          fn @@ `Added (h, v)
        | `Updated (x, y) ->
          assert (not (S.Hash.equal x y));
          view_of_head x >>= fun vx ->
          view_of_head y >>= fun vy ->
          if equal vx vy then Lwt.return_unit
          else fn @@ `Updated ( (x, vx), (y, vy) )
      )

end

module type S = sig
  include Ir_s.HIERARCHICAL
  val empty: unit -> t Lwt.t
  val rebase: t -> into:t -> unit Ir_merge.result Lwt.t
  val rebase_exn: t -> into:t -> unit Lwt.t
  type db
  val of_path: db -> key -> t Lwt.t
  val update_path: db -> key -> t -> unit Lwt.t
  val rebase_path: db -> key -> t -> unit Ir_merge.result Lwt.t
  val rebase_path_exn: db -> key -> t -> unit Lwt.t
  val merge_path: db -> ?max_depth:int -> ?n:int -> key -> t ->
    unit Ir_merge.result Lwt.t
  val merge_path_exn: db -> ?max_depth:int -> ?n:int -> key -> t -> unit Lwt.t
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
  val diff: t -> t -> (key * value Ir_watch.diff) list Lwt.t
  type commit_id
  val parents: t -> commit_id list
  val make_head: db -> Ir_task.t -> parents:commit_id list -> contents:t -> commit_id Lwt.t
  val watch_path: db -> key -> ?init:(commit_id * t) ->
    ((commit_id * t) Ir_watch.diff -> unit Lwt.t) -> (unit -> unit Lwt.t) Lwt.t
end
