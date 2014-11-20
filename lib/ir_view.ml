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

module type ACTION = sig
  type path
  type contents
  type t =
    [ `Read of (path * contents option)
    | `Write of (path * contents option)
    | `List of (path list * path list) ]
  (** Operations on view. We record the result of reads to be able to
      replay them on merge. *)

  include Tc.I0 with type t := t

  val pretty: t -> string
  (** Pretty-print an action. *)
end

module Action (P: Tc.I0) (C: Tc.I0) = struct

  type path = P.t
  type contents = C.t

  type t =
    [ `Read of (path * contents option)
    | `Write of (path * contents option)
    | `List of  (path list * path list) ]

  module R = Tc.App2(Tc.P)( P )( Tc.App1(Tc.O)(C) )
  module W = R
  module L = Tc.App2(Tc.P)( Tc.App1(Tc.L)(P) )( Tc.App1(Tc.L)(P) )

  let compare = Pervasives.compare
  let hash = Hashtbl.hash

  let equal x y = match x, y with
    | `Read x , `Read y  -> R.equal x y
    | `Write x, `Write y -> W.equal x y
    | `List x , `List y  -> L.equal x y
    | _ -> false

  let to_sexp t =
    let open Sexplib.Type in
    match t with
    | `Read x  -> List [ Atom "read" ; R.to_sexp x ]
    | `Write x -> List [ Atom "write"; W.to_sexp x ]
    | `List x  -> List [ Atom "list" ; L.to_sexp x ]

  let to_json = function
    | `Read x  -> `O [ "read" , R.to_json x ]
    | `Write x -> `O [ "write", W.to_json x ]
    | `List x  -> `O [ "list" , L.to_json x ]

  let of_json = function
    | `O [ "read" , x ] -> `Read  (R.of_json x)
    | `O [ "write", x ] -> `Write (W.of_json x)
    | `O [ "list" , x ] -> `List  (L.of_json x)
    | j -> Ezjsonm.parse_error j "View.Action.of_json"

  let write t buf = match t with
    | `Read x  -> R.write x (Ir_misc.tag buf 0)
    | `Write x -> W.write x (Ir_misc.tag buf 1)
    | `List x  -> L.write x (Ir_misc.tag buf 2)

  let read buf = match Ir_misc.untag buf with
    | 0 -> `Read  (R.read buf)
    | 1 -> `Write (W.read buf)
    | 2 -> `List  (L.read buf)
    | n -> Tc.Reader.error "View.Action.read (tag=%d)" n

  let size_of t = 1 + match t with
    | `Read x  -> R.size_of x
    | `Write x -> W.size_of x
    | `List x  -> L.size_of x

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
    | `Write (p,x) ->sprintf "write %s <- %s" (pretty_key p) (pretty_valo x)
    | `List (i,o) -> sprintf "list  %s -> %s" (pretty_keys i) (pretty_keys o)

end

(***** views *)

module type NODE = sig
  type t
  type node
  type contents
  module Origin: Tc.I0
  module Contents: Tc.I0 with type t = contents
  module Step: Ir_step.S
  module StepMap: Ir_misc.MAP with type key = Step.t
  val empty: unit -> t
  val is_empty: t -> bool
  val read: t -> Origin.t -> node option Lwt.t
  val succ: node -> t StepMap.t
  val contents: t -> Origin.t -> contents option Lwt.t
  val update_succ: t -> Origin.t -> t StepMap.t -> unit Lwt.t
  val update_contents: t -> Origin.t -> contents option -> unit Lwt.t
end

module type S = sig
  type step
  include Ir_rw.STORE with type key = step list
  type action
  val actions: t -> action list
  val merge: t -> origin -> into:t -> unit Ir_merge.result Lwt.t
  module Action: ACTION
    with type path = key
     and type contents = value
end

module Internal (Node: NODE) = struct

  module Step = Node.Step
  module StepMap = Node.StepMap

  type origin = Node.Origin.t
  type step = Step.t
  type key = step list
  type value = Node.contents

  module Path = Ir_step.Path(Step)
  module PathSet = Ir_misc.Set(Path)

  module Action = Action(Path)(Node.Contents)
  type action = Action.t

  module Univ = struct
    type t = exn
    let create (type s) () =
      let module M = struct exception E of s option end in
      (fun x -> M.E (Some x)), (function M.E x -> x | _ -> None)
  end

  type t = {
    view: Node.t;
    mutable ops: action list;
    mutable parents: Univ.t list;
  }

  module CO = Tc.App1(Tc.O)(Node.Contents)
  module PL = Tc.App1(Tc.L)(Path)

  let create () =
    Log.debugf "create";
    let view = Node.empty () in
    let ops = [] in
    let parents = [] in
    { parents; view; ops }

  let sub t origin path =
    let rec aux node = function
      | []   -> return (Some node)
      | h::p ->
        Node.read node origin >>= function
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

  let read_aux t origin path =
    sub t origin path >>= function
    | None   -> return_none
    | Some n -> Node.contents n origin

  let read t origin k =
    read_aux t origin k >>= fun v ->
    t.ops <- `Read (k, v) :: t.ops;
    return v

  let read_exn t origin k =
    read t origin k >>= function
    | None   -> fail Not_found
    | Some v -> return v

  let mem t origin k =
    read t origin k >>= function
    | None  -> return false
    | _     -> return true

  let list_aux t origin paths =
    let aux acc path =
      sub t origin path >>= function
      | None   -> return acc
      | Some n ->
        Node.read n origin >>= function
        | None -> return acc
        | Some t ->
          let succ = Node.succ t in
          let paths = List.map (fun p -> path @ [p]) (StepMap.keys succ) in
          let paths = PathSet.of_list paths in
          return (PathSet.union acc paths) in
    Lwt_list.fold_left_s aux PathSet.empty paths >>= fun paths ->
    return (PathSet.to_list paths)

  let list t origin paths =
    list_aux t origin paths >>= fun result ->
    t.ops <- `List (paths, result) :: t.ops;
    return result

  let dump _t _origin =
    failwith "TODO"

  let with_cleanup _t origin view fn =
    fn () >>= fun () ->
    Node.read view origin >>= function
    | None   -> return_unit
    | Some n ->
      let succ =
        StepMap.filter (fun _ n -> not (Node.is_empty n)) (Node.succ n)
      in
      Node.update_succ view origin succ

  let update_opt_aux t origin k v =
    let rec aux view = function
      | []   -> Node.update_contents view origin v
      | h::p ->
        Node.read view origin >>= function
        | None   -> if v = None then return_unit else fail Not_found (* XXX ?*)
        | Some n ->
          try
            let child = StepMap.find h (Node.succ n) in
            if v = None then with_cleanup t origin view (fun () -> aux child p)
            else aux child p
          with Not_found ->
            if v = None then return_unit
            else
              let child = Node.empty () in
              let succ = StepMap.add h child (Node.succ n) in
              Node.update_succ view origin succ >>= fun () ->
              aux child p in
    aux t.view k

  let update_opt t origin k v =
    t.ops <- `Write (k, v) :: t.ops;
    update_opt_aux t origin k v

  let update t origin k v =
    update_opt t origin k (Some v)

  let remove t origin k =
    update_opt t origin k None

  let watch _ =
    failwith "TODO"

  let apply t origin a =
    Log.debugf "apply %a" force (show (module Action) a);
    match a with
    | `Write (k, v) -> update_opt t origin k v >>= ok
    | `Read (k, v)  ->
      read t origin k >>= fun v' ->
      if Tc.equal (module CO) v v' then ok ()
      else
        let str = function
          | None   -> "<none>"
          | Some c -> Tc.show (module Node.Contents) c in
        conflict "read %s: got %S, expecting %S"
          (Tc.show (module Path) k) (str v') (str v)
    | `List (l, r) ->
      list t origin l >>= fun r' ->
      if Tc.equal (module PL) r r' then ok ()
      else
        let str = Ir_misc.list_pretty (Tc.show (module Path)) in
        conflict "list %s: got %s, expecting %s" (str l) (str r') (str r)

  let actions t =
    List.rev t.ops

  let merge t1 origin ~into =
    Ir_merge.iter (apply into origin) (List.rev t1.ops) >>| fun () ->
    into.parents <- Ir_misc.list_dedup (t1.parents @ into.parents);
    ok ()

end

(*** Simple views stored in memory. No database access. Used to build
     a view programmatically and independently of any underlying
     database. *)

module Make (S: Tc.I0) (V: Tc.I0) (O: Tc.I0) = struct

  module Node = struct
    module Contents = V
    module Origin = O
    module Step = S
    module StepMap = Ir_misc.Map(S)

    type contents = V.t

    type node = {
      contents: Contents.t option;
      succ    : t StepMap.t;
    }
    and t = node ref

    let empty () = ref { contents = None; succ = StepMap.empty }
    let is_empty t = !t.contents = None && StepMap.is_empty !t.succ
    let read t _ = return (Some !t)
    let succ t = t.succ
    let contents t _ = return !t.contents
    let update_succ t _ succ = t := { !t with succ }; return_unit
    let update_contents t _ contents = t := { !t with contents }; return_unit
  end

  include Internal(Node)

end

module type OF_STORE = sig
  include S
  type db
  val origin_of_actions: t -> origin
  val of_path: db -> origin -> key -> t Lwt.t
  val update_path: db -> origin -> key -> t -> unit Lwt.t
  val rebase_path: db -> origin -> key -> t -> unit Ir_merge.result Lwt.t
  val merge_path: db -> origin -> key -> t -> unit Ir_merge.result Lwt.t
end

module Of_store (S: Ir_bc.STORE_EXT) = struct

  module B = S.Block

  module Contents = struct

    type contents_or_key =
      | Key of B.Contents.key
      | Contents of B.contents
      | Both of B.Contents.key * B.contents

    type t = contents_or_key ref
    (* Same as [Contents.t] but can either be a raw contents or a key
       that will be fetched lazily. *)

    let create c =
      ref (Contents c)

    let export c =
      match !c with
      | Both (k, _)
      | Key k -> k
      | Contents _ -> failwith "Contents.export"

    let key k =
      ref (Key k)

    let read t origin =
      match !t with
      | Both (_, c)
      | Contents c -> return (Some c)
      | Key k      ->
        B.Contents.read (B.Contents.create ()) origin k >>= function
        | None   -> return_none
        | Some c ->
          t := Both (k, c);
          return (Some c)

  end

  module Node = struct

    module Origin = B.Origin
    module Step = B.Node.Step
    module StepMap = Ir_misc.Map(B.Node.Step)

    type contents = B.contents

    type node = {
      contents: Contents.t option;
      succ    : t StepMap.t;
    }

    and node_or_key  =
      | Key of B.Node.key
      | Node of node
      | Both of B.Node.key * node

    and t = node_or_key ref
    (* Similir to [Node.t] but using where all of the values can just
       be keys. *)

    let succ t = t.succ

    let create' contents succ =
      Node { contents; succ }

    let create contents succ =
      ref (create' contents succ)

    let key k =
      ref (Key k)

    let both k v =
      ref (Both (k, v))

    let empty () =
      create None StepMap.empty

    let is_empty n =
      match !n with
      | Key _  -> false
      | Both (_, n)
      | Node n -> n.contents = None && StepMap.is_empty n.succ

    let import n =
      let contents = match B.Node.Val.contents n with
        | None   -> None
        | Some k -> Some (Contents.key k) in
      let succ = StepMap.map key (B.Node.Val.succ n) in
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
      let succ = StepMap.map export n.succ in
      B.Node.Val.create ?contents succ

    let read t origin =
      match !t with
      | Both (_, n)
      | Node n   -> return (Some n)
      | Key k    ->
        B.Node.read (B.Node.create ()) origin k >>= function
        | None   -> return_none
        | Some n ->
          let n = import n in
          t := Both (k, n);
          return (Some n)

    let contents t origin =
      read t origin >>= function
      | None   -> return_none
      | Some c ->
        match c.contents with
        | None   -> return_none
        | Some c -> Contents.read c origin

    let update_contents t origin v =
      read t origin >>= function
      | None   -> if v = None then return_unit else fail Not_found (* XXX ? *)
      | Some n ->
        let new_n = match v with
          | None   -> { n with contents = None }
          | Some c -> { n with contents = Some (Contents.create c) } in
        t := Node new_n;
        return_unit

    let update_succ t origin succ =
      read t origin >>= function
      | None   ->
        if StepMap.is_empty succ then return_unit else
          fail Not_found (* XXX ? *)
      | Some n ->
        let new_n = { n with succ } in
        t := Node new_n;
        return_unit

    module Contents = B.Contents.Val

  end

  include Internal(Node)

  type db = S.t

  let to_univ, of_univ = Univ.create ()

  let univ_of_parents (t:S.head list) =
    List.map to_univ t

  let parents_of_univ l: S.head list =
    Ir_misc.list_filter_map (fun x -> x) (List.map of_univ l)

  let import ~parents key origin =
    Log.debugf "import %a" force (show (module B.Node.Key) key);
    B.Node.read (B.Node.create ()) origin key >>= function
    | None   -> fail Not_found
    | Some n ->
      let view = Node.both key (Node.import n) in
      let ops = [] in
      let parents = univ_of_parents parents in
      return { parents; view; ops }

  let export t origin =
    Log.debugf "export";
    let node n = B.Node.add (B.Node.create ()) origin (Node.export_node n) in
    let todo = Stack.create () in
    let rec add_to_todo n =
      match !n with
      | Node.Both _
      | Node.Key _  -> ()
      | Node.Node x ->
        (* 1. we push the current node job on the stack. *)
        Stack.push (fun () ->
            node x >>= fun k ->
            n := Node.Key k;
            return_unit
          ) todo;
        (* 2. we push the contents job on the stack. *)
        Stack.push (fun () ->
            match x.Node.contents with
            | None   -> return_unit
            | Some c ->
              match !c with
              | Contents.Both _
              | Contents.Key _       -> return_unit
              | Contents.Contents x  ->
                B.Contents.add (B.Contents.create ()) origin x >>= fun k ->
                c := Contents.Key k;
                return_unit
          ) todo;
        (* 3. we push the children jobs on the stack. *)
        StepMap.iter (fun _ n ->
            Stack.push (fun () -> add_to_todo n; return_unit) todo
          ) (Node.succ x);
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
    return (Node.export t.view)

  let of_path t origin path =
    Log.debugf "read_view %a" force (show (module Path) path);
    let parents =
      S.head t origin >>= function
      | None   -> return_nil
      | Some h -> return [h] in
    S.read_node t origin path >>= function
    | None   -> return (create ())
    | Some n ->
      B.Node.add (B.Node.create ()) origin n >>= fun k ->
      parents >>= fun parents ->
      import ~parents k origin

  let node_of_view _t origin view =
    export view origin >>= fun key ->
    B.Node.read_exn (B.Node.create ()) origin key

  let update_path t origin path view =
    Log.debugf "update_view %a" force (show (module Path) path);
    node_of_view t origin view >>= fun node ->
    S.update_node t origin path node

  let origin_of_actions t =
    let buf = Buffer.create 1024 in
    List.iter (fun a ->
        bprintf buf "- %s\n" (Action.pretty a)
      ) t.ops;
    B.Origin.create "Actions:\n%s\n" (Buffer.contents buf)

  let rebase_path t origin path view =
    Log.debugf "merge_view %a" force (show (module Path) path);
    S.mem_node t origin [] >>= function
    | false -> fail Not_found
    | true  ->
      of_path t origin path >>= fun head_view ->
      merge view origin ~into:head_view >>| fun () ->
      update_path t origin path head_view >>= fun () ->
      ok ()

  let merge_path t origin path view =
    Log.debugf "merge_view %a" force (show (module Path) path);
    S.read_node t origin [] >>= function
    | None           -> fail Not_found
    | Some head_node ->
      (* First, we check than we can rebase the view on the current
         HEAD. *)
      of_path t origin path >>= fun head_view ->
      merge view origin ~into:head_view >>| fun () ->
      (* Now that we know that rebasing is possible, we discard the
         result and proceed as a normal merge, ie. we apply the view
         on a branch, and we merge the branch back into the store. *)
      node_of_view t origin view >>= fun view_node ->
      (* Create a commit with the contents of the view *)
      B.Node.map (B.Node.create ()) origin head_node path (fun _ -> view_node)
      >>= fun new_head_node ->
      let parents = parents_of_univ view.parents in
      let t_c = B.Commit.create () in
      Lwt_list.map_p (B.Commit.read_exn t_c origin) parents
      >>= fun parents ->
      B.Commit.commit t_c origin ~node:new_head_node ~parents
      >>= fun (k, _) ->
      (* We want to avoid to create a merge commit when the HEAD has
         not been updated since the view has been created. *)
      S.head t origin >>= function
      | None ->
        (* The store is empty, create a fresh commit. *)
        S.update_head t origin k >>= ok
      | Some head ->
        let parents = parents_of_univ view.parents in
        if List.mem head parents then
          S.update_head t origin k >>= ok
        else
          S.merge_head t origin k

end
