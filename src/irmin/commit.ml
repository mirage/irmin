(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open! Import
include Commit_intf
open Merge.Infix

let src = Logs.Src.create "irmin.commit" ~doc:"Irmin commits"

module Log = (val Logs.src_log src : Logs.LOG)

module Maker_generic_key (I : Info.S) = struct
  module Info = I

  module Make
      (H : Type.S)
      (N : Key.S with type hash = H.t)
      (C : Key.S with type hash = H.t) =
  struct
    module Info = I

    type hash = H.t [@@deriving irmin ~compare]
    type node_key = N.t [@@deriving irmin ~compare]
    type commit_key = C.t [@@deriving irmin]

    type t = { node : node_key; parents : commit_key list; info : Info.t }
    [@@deriving irmin]

    type t_not_prefixed = t [@@deriving irmin]

    let pre_hash = Type.(unstage (pre_hash t))

    (* Manually add a prefix to default commits, in order to prevent hash
       collision between contents and commits (see
       https://github.com/mirage/irmin/issues/1304).
       If we only prefix the prehash of contents, (suppose the prefix is "B"),
       then we can have a collision with the prehash of a commit (the prehash of
       a commit starts with the hash of the root and can start with a "B" - the
       prefix of the contents is not enough to prevent the collision). *)
    let pre_hash_prefixed x f =
      f "C";
      pre_hash x f

    let t = Type.(like t ~pre_hash:pre_hash_prefixed)
    let parents t = t.parents
    let node t = t.node
    let info t = t.info
    let compare_commit_key x y = compare_hash (C.to_hash x) (C.to_hash y)

    let v ~info ~node ~parents =
      let parents = List.fast_sort compare_commit_key parents in
      { node; parents; info }

    module Portable = struct
      module Info = I

      type commit = t

      type t = { node : hash; parents : hash list; info : Info.t }
      [@@deriving irmin]

      type t_not_prefixed = t [@@deriving irmin]

      let pre_hash = Type.(unstage (pre_hash t))

      let pre_hash_prefixed x f =
        f "C";
        pre_hash x f

      let t = Type.(like t ~pre_hash:pre_hash_prefixed)

      type commit_key = H.t [@@deriving irmin]
      type node_key = H.t [@@deriving irmin]
      type hash = H.t [@@deriving irmin]

      let parents t = t.parents
      let node t = t.node
      let info t = t.info

      let v ~info ~node ~parents =
        let parents = List.fast_sort compare_hash parents in
        { node; parents; info }

      let of_commit : commit -> t =
       fun { node; parents; info } ->
        let node = N.to_hash node in
        let parents = List.map C.to_hash parents in
        { node; parents; info }
    end
  end

  module Make_v2
      (H : Type.S)
      (N : Key.S with type hash = H.t)
      (C : Key.S with type hash = H.t) =
  struct
    include Make (H) (N) (C)

    let t = t_not_prefixed_t

    module Portable = struct
      include Portable

      let t = t_not_prefixed_t
    end
  end
end

module Maker (Info : Info.S) = struct
  include Maker_generic_key (Info)

  module Make (H : Type.S) = struct
    module Key = Key.Of_hash (H)
    include Make (H) (Key) (Key)
  end
end

module Store_generic_key
    (I : Info.S)
    (N : Node.Store)
    (S : Indexable.S)
    (H : Hash.S with type t = S.hash)
    (V : S_generic_key
           with type node_key = N.Key.t
            and type commit_key = S.Key.t
            and type t = S.value
            and module Info := I) =
struct
  module Node = N
  module Val = V
  module Key = S.Key
  module Hash = Hash.Typed (H) (V)
  module Info = I

  type 'a t = 'a N.t * 'a S.t
  type key = Key.t [@@deriving irmin ~equal]
  type value = S.value
  type hash = S.hash

  let add (_, t) = S.add t
  let unsafe_add (_, t) = S.unsafe_add t
  let mem (_, t) = S.mem t
  let index (_, t) = S.index t
  let find (_, t) = S.find t
  let batch (n, s) f = N.batch n (fun n -> S.batch s (fun s -> f (n, s)))

  let close (n, s) =
    let* () = N.close n in
    let+ () = S.close s in
    ()

  let merge_node (t, _) = Merge.f (N.merge t)
  let pp_key = Type.pp Key.t
  let err_not_found k = Fmt.kstr invalid_arg "Commit.get: %a not found" pp_key k

  let get (_, t) k =
    S.find t k >>= function None -> err_not_found k | Some v -> Lwt.return v

  let empty_if_none (n, _) = function
    | None -> N.add n (N.Val.empty ())
    | Some node -> Lwt.return node

  let equal_key = Type.(unstage (equal Key.t))
  let equal_opt_keys = Type.(unstage (equal (option Key.t)))

  let merge_commit info t ~old k1 k2 =
    [%log.debug "Commit.merge %a %a" pp_key k1 pp_key k2];
    let* v1 = get t k1 in
    let* v2 = get t k2 in
    if List.mem ~equal:equal_key k1 (Val.parents v2) then Merge.ok k2
    else if List.mem ~equal:equal_key k2 (Val.parents v1) then Merge.ok k1
    else
      (* If we get an error while looking the the lca, then we
         assume that there is no common ancestor. Maybe we want to
         expose this to the user in a more structured way. But maybe
         that's too much low-level details. *)
      let* old =
        old () >>= function
        | Error (`Conflict msg) ->
            [%log.debug "old: conflict %s" msg];
            Lwt.return_none
        | Ok o -> Lwt.return o
      in
      if equal_opt_keys old (Some k1) then Merge.ok k2
      else if equal_opt_keys old (Some k2) then Merge.ok k1
      else
        let old () =
          match old with
          | None -> Merge.ok None
          | Some old ->
              let* vold = get t old in
              Merge.ok (Some (Some (Val.node vold)))
        in
        merge_node t ~old (Some (Val.node v1)) (Some (Val.node v2))
        >>=* fun node ->
        let* node = empty_if_none t node in
        let parents = [ k1; k2 ] in
        let commit = Val.v ~node ~parents ~info:(info ()) in
        let* key = add t commit in
        Merge.ok key

  let merge t ~info = Merge.(option (v Key.t (merge_commit info t)))
end

module Generic_key = struct
  module type S = S_generic_key
  module type Maker = Maker_generic_key

  module Maker = Maker_generic_key
  module Store = Store_generic_key
  include Maker (Info.Default)
end

module Portable = struct
  module Of_commit (X : S) = struct
    include X

    let of_commit t = t
  end

  module type S = Portable
end

module Store
    (I : Info.S)
    (N : Node.Store)
    (S : Content_addressable.S with type key = N.key)
    (H : Hash.S with type t = S.key)
    (V : S with type hash = S.key and type t = S.value and module Info := I) =
struct
  include
    Store_generic_key (I) (N) (Indexable.Of_content_addressable (H) (S)) (H) (V)

  module Val = struct
    include Val

    type hash = H.t [@@deriving irmin]
  end
end

module History (S : Store) = struct
  type commit_key = S.Key.t [@@deriving irmin]
  type node_key = S.Val.node_key [@@deriving irmin]
  type v = S.Val.t [@@deriving irmin]
  type info = S.Info.t [@@deriving irmin]
  type 'a t = 'a S.t

  let merge t ~info =
    let f ~old c1 c2 =
      let somify = Merge.map_promise (fun x -> Some x) in
      let merge = S.merge t ~info in
      Merge.f merge ~old:(somify old) (Some c1) (Some c2) >>=* function
      | None -> Merge.conflict "History.merge"
      | Some x -> Merge.ok x
    in
    Merge.v S.Key.t f

  let v t ~node ~parents ~info =
    let commit = S.Val.v ~node ~parents ~info in
    let+ hash = S.add t commit in
    (hash, commit)

  let pp_key = Type.pp S.Key.t

  let parents t c =
    [%log.debug "parents %a" pp_key c];
    S.find t c >|= function None -> [] | Some c -> S.Val.parents c

  module U = struct
    type t = unit [@@deriving irmin]
  end

  module Graph = Object_graph.Make (U) (S.Node.Key) (S.Key) (U)

  let edges t =
    [%log.debug "edges"];
    [ `Node (S.Val.node t) ] @ List.map (fun k -> `Commit k) (S.Val.parents t)

  let closure t ~min ~max =
    [%log.debug "closure"];
    let pred = function
      | `Commit k -> ( S.find t k >|= function Some r -> edges r | None -> [])
      | _ -> Lwt.return_nil
    in
    let min = List.map (fun k -> `Commit k) min in
    let max = List.map (fun k -> `Commit k) max in
    let+ g = Graph.closure ~pred ~min ~max () in
    List.fold_left
      (fun acc -> function `Commit k -> k :: acc | _ -> acc)
      [] (Graph.vertex g)

  let ignore_lwt _ = Lwt.return_unit

  let iter t ~min ~max ?(commit = ignore_lwt) ?edge
      ?(skip = fun _ -> Lwt.return_false) ?(rev = true) () =
    let max = List.map (fun x -> `Commit x) max in
    let min = List.map (fun x -> `Commit x) min in
    let node = function `Commit x -> commit x | _ -> assert false in
    let skip = function `Commit x -> skip x | _ -> assert false in
    let pred = function
      | `Commit k -> parents t k >|= List.map (fun x -> `Commit x)
      | _ -> assert false
    in
    let edge =
      Option.map
        (fun edge n pred ->
          match (n, pred) with
          | `Commit src, `Commit dst -> edge src dst
          | _ -> assert false)
        edge
    in
    Graph.iter ~pred ~min ~max ~node ?edge ~skip ~rev ()

  module K = struct
    type t = S.Key.t

    let compare = Type.(unstage (compare S.Key.t))
    let hash k = S.Hash.short_hash (S.Key.to_hash k)
    let equal = Type.(unstage (equal S.Key.t))
  end

  module KSet = Set.Make (K)
  module KHashtbl = Hashtbl.Make (K)

  let read_parents t commit =
    S.find t commit >|= function
    | None -> KSet.empty
    | Some c -> KSet.of_list (S.Val.parents c)

  let equal_keys = Type.(unstage (equal S.Key.t))
  let str_key k = String.sub (Type.to_string S.Key.t k) 0 4
  let pp_key = Fmt.of_to_string str_key

  let pp_keys ppf keys =
    let keys = KSet.elements keys in
    Fmt.pf ppf "[%a]" Fmt.(list ~sep:(any " ") pp_key) keys

  let str_keys = Fmt.to_to_string pp_keys
  let lca_calls = ref 0

  let rec unqueue todo seen =
    if Queue.is_empty todo then None
    else
      let ((_, commit) as pop) = Queue.pop todo in
      if KSet.mem commit seen then unqueue todo seen else Some pop

  (* Traverse the graph of commits using a breadth first search
     strategy. Start by visiting the commits in [init] and stops
     either when [check] returns [`Stop] or when all the ancestors of
     [init] have been visited. *)
  let traverse_bfs t ~f ~pp:_ ~check ~init ~return =
    let todo = Queue.create () in
    let add_todo d x = Queue.add (d, x) todo in
    KSet.iter (add_todo 0) init;
    let rec aux seen =
      match check () with
      | (`Too_many_lcas | `Max_depth_reached) as x -> Lwt.return (Error x)
      | `Stop -> return ()
      | `Continue -> (
          match unqueue todo seen with
          | None -> return ()
          | Some (depth, commit) ->
              (* Log.debug "lca %d: %s.%d %a"
                 !lca_calls (pp_key commit) depth force (pp ()); *)
              let seen = KSet.add commit seen in
              let* parents = read_parents t commit in
              let () = f depth commit parents in
              let parents = KSet.diff parents seen in
              KSet.iter (add_todo (depth + 1)) parents;
              aux seen)
    in
    aux KSet.empty

  (* Initially the first node is marked as [Seen1] and the second as [Seen2].
     Marks are updated as the search progresses, and may change. *)
  type mark =
    | Seen1 (* reachable from the first commit *)
    | Seen2 (* reachable from the second commit *)
    | SeenBoth (* reachable from both, but below an LCA *)
    | LCA

  (* reachable from both; candidate for the answer set *)

  let _pp_mark = function
    | Seen1 -> "seen1"
    | Seen2 -> "seen2"
    | SeenBoth -> "seenBoth"
    | LCA -> "LCA"

  (* Exploration state *)
  type state = {
    marks : mark KHashtbl.t;
    (* marks of commits already explored *)
    parents : KSet.t KHashtbl.t;
    (* parents of commits already explored *)
    layers : (int, KSet.t) Hashtbl.t;
    (* layers of commit, sorted by depth *)
    c1 : S.key;
    (* initial state 1 *)
    c2 : S.key;
    (* initial state 2 *)
    mutable depth : int;
    (* the current exploration depth *)
    mutable lcas : int;
    (* number of commit marked with LCA *)
    mutable complete : bool; (* is the exploration complete? *)
  }

  let pp_state t =
    lazy
      (let pp m =
         KHashtbl.fold
           (fun k v acc -> if v = m then str_key k :: acc else acc)
           t.marks []
         |> String.concat " "
       in
       Fmt.str "d: %d, seen1: %s, seen2: %s, seenboth: %s, lcas: %s (%d) %s"
         t.depth (pp Seen1) (pp Seen2) (pp SeenBoth) (pp LCA) t.lcas
         (String.concat " | "
            (Hashtbl.fold
               (fun d ks acc -> Fmt.str "(%d: %s)" d (str_keys ks) :: acc)
               t.layers [])))

  let get_mark_exn t elt = KHashtbl.find t.marks elt
  let get_mark t elt = try Some (get_mark_exn t elt) with Not_found -> None
  let set_mark t elt mark = KHashtbl.replace t.marks elt mark
  let get_layer t d = try Hashtbl.find t.layers d with Not_found -> KSet.empty

  let add_to_layer t d k =
    Hashtbl.replace t.layers d (KSet.add k (get_layer t d))

  let add_parent t c p = KHashtbl.add t.parents c p

  let get_parent t c =
    try KHashtbl.find t.parents c with Not_found -> KSet.empty

  let incr_lcas t = t.lcas <- t.lcas + 1
  let decr_lcas t = t.lcas <- t.lcas - 1

  let both_seen t k =
    match get_mark t k with
    | None | Some Seen1 | Some Seen2 -> false
    | _ -> true

  let empty_state c1 c2 =
    let t =
      {
        marks = KHashtbl.create 10;
        parents = KHashtbl.create 10;
        layers = Hashtbl.create 10;
        c1;
        c2;
        depth = 0;
        lcas = 0;
        complete = false;
      }
    in
    set_mark t c1 Seen1;
    set_mark t c2 Seen2;
    t

  (* update the parent mark and keep the number of lcas up-to-date. *)
  let update_mark t mark commit =
    let new_mark =
      match (mark, get_mark t commit) with
      | Seen1, Some Seen1 | Seen1, None -> Seen1
      | Seen2, Some Seen2 | Seen2, None -> Seen2
      | SeenBoth, Some LCA ->
          decr_lcas t;
          SeenBoth
      | SeenBoth, _ -> SeenBoth
      | Seen1, Some Seen2 | Seen2, Some Seen1 ->
          incr_lcas t;
          LCA
      | _, Some LCA -> LCA
      | _ -> SeenBoth
    in
    (* check for fast-forwards *)
    let is_init () = equal_keys commit t.c1 || equal_keys commit t.c2 in
    let is_shared () = new_mark = SeenBoth || new_mark = LCA in
    if is_shared () && is_init () then (
      [%log.debug "fast-forward"];
      t.complete <- true);
    set_mark t commit new_mark;
    new_mark

  (* update the ancestors which have already been visisted. *)
  let update_ancestors_marks t mark commit =
    let todo = Queue.create () in
    Queue.add commit todo;
    let rec loop mark =
      if Queue.is_empty todo then ()
      else
        let a = Queue.pop todo in
        let old_mark = get_mark t a in
        let mark = update_mark t mark a in
        let () =
          match old_mark with
          | Some (SeenBoth | LCA) -> () (* Can't be an LCA lower down *)
          | Some old when old = mark -> () (* No change *)
          | _ -> KSet.iter (fun x -> Queue.push x todo) (get_parent t a)
        in
        loop (if mark = LCA then SeenBoth else mark)
    in
    loop mark

  (* We are looking for LCAs, doing a breadth-first-search from the two starting commits.
     This is called each time we visit a new commit. *)
  let update_parents t depth commit parents =
    add_parent t commit parents;
    add_to_layer t depth commit;
    if depth <> t.depth then (
      assert (depth = t.depth + 1);

      (* before starting to explore a new layer, check if we really
         have some work to do, ie. do we still have a commit seen only
         by one node? *)
      let layer = get_layer t t.depth in
      let complete = KSet.for_all (both_seen t) layer in
      if complete then t.complete <- true else t.depth <- depth);
    let mark = get_mark_exn t commit in
    KSet.iter (update_ancestors_marks t mark) parents

  let lcas t =
    KHashtbl.fold (fun k v acc -> if v = LCA then k :: acc else acc) t.marks []

  let check ~max_depth ~n t =
    if t.depth > max_depth then `Max_depth_reached
    else if t.lcas > n then `Too_many_lcas
    else if t.lcas = n || t.complete then `Stop
    else `Continue

  let lcas t ?(max_depth = max_int) ?(n = max_int) c1 c2 =
    incr lca_calls;
    if max_depth < 0 then Lwt.return (Error `Max_depth_reached)
    else if n <= 0 then Lwt.return (Error `Too_many_lcas)
    else if equal_keys c1 c2 then Lwt.return (Ok [ c1 ])
    else
      let init = KSet.of_list [ c1; c2 ] in
      let s = empty_state c1 c2 in
      let check () = check ~max_depth ~n s in
      let pp () = pp_state s in
      let return () = Lwt.return (Ok (lcas s)) in
      let t0 = Sys.time () in
      Lwt.finalize
        (fun () ->
          traverse_bfs t ~f:(update_parents s) ~pp ~check ~init ~return)
        (fun () ->
          let t1 = Sys.time () -. t0 in
          [%log.debug "lcas %d: depth=%d time=%.4fs" !lca_calls s.depth t1];
          Lwt.return_unit)

  let rec three_way_merge t ~info ?max_depth ?n c1 c2 =
    [%log.debug "3-way merge between %a and %a" pp_key c1 pp_key c2];
    if equal_keys c1 c2 then Merge.ok c1
    else
      let* lcas = lcas t ?max_depth ?n c1 c2 in
      let old () =
        match lcas with
        | Error `Too_many_lcas -> Merge.conflict "Too many lcas"
        | Error `Max_depth_reached -> Merge.conflict "Max depth reached"
        | Ok [] -> Merge.ok None (* no common ancestor *)
        | Ok (old :: olds) ->
            let rec aux acc = function
              | [] -> Merge.ok (Some acc)
              | old :: olds ->
                  three_way_merge t ~info acc old >>=* fun acc -> aux acc olds
            in
            aux old olds
      in
      let merge =
        merge t ~info
        |> Merge.with_conflict (fun msg ->
               Fmt.str "Recursive merging of common ancestors: %s" msg)
        |> Merge.f
      in
      merge ~old c1 c2

  let lca_aux t ~info ?max_depth ?n c1 c2 =
    if equal_keys c1 c2 then Merge.ok (Some c1)
    else
      lcas t ?max_depth ?n c1 c2 >>= function
      | Error `Too_many_lcas -> Merge.conflict "Too many lcas"
      | Error `Max_depth_reached -> Merge.conflict "Max depth reached"
      | Ok [] -> Merge.ok None (* no common ancestor *)
      | Ok [ x ] -> Merge.ok (Some x)
      | Ok (c :: cs) ->
          let rec aux acc = function
            | [] -> Merge.ok (Some acc)
            | c :: cs -> (
                three_way_merge t ~info ?max_depth ?n acc c >>= function
                | Error (`Conflict _) -> Merge.ok None
                | Ok acc -> aux acc cs)
          in
          aux c cs

  let rec lca t ~info ?max_depth ?n = function
    | [] -> Merge.conflict "History.lca: empty"
    | [ c ] -> Merge.ok (Some c)
    | c1 :: c2 :: cs -> (
        lca_aux t ~info ?max_depth ?n c1 c2 >>=* function
        | None -> Merge.ok None
        | Some c -> lca t ~info ?max_depth ?n (c :: cs))
end

module V1 = struct
  module Info = struct
    include Info.Default

    let t : t Type.t =
      let open Type in
      record "info" (fun date author message -> v ~author ~message date)
      |+ field "date" int64 (fun t -> date t)
      |+ field "author" (string_of `Int64) (fun t -> author t)
      |+ field "message" (string_of `Int64) (fun t -> message t)
      |> sealr
  end

  module Make (Hash : Hash.S) (C : Generic_key.S with module Info := Info) =
  struct
    module K (K : Type.S) = struct
      let h = Type.string_of `Int64

      type t = K.t [@@deriving irmin ~pre_hash ~to_bin_string ~of_bin_string]

      let size_of = Type.Size.using to_bin_string (Type.Size.t h)

      let encode_bin =
        let encode_bin = Type.(unstage (encode_bin h)) in
        fun e k -> encode_bin (to_bin_string e) k

      let decode_bin =
        let decode_bin = Type.(unstage (decode_bin h)) in
        fun buf pos_ref ->
          let v = decode_bin buf pos_ref in
          match of_bin_string v with
          | Ok v -> v
          | Error (`Msg e) -> Fmt.failwith "decode_bin: %s" e

      (* Manually box hashes in V1 commits with length headers: *)
      let pre_hash =
        let hash_length_header : string =
          let b = Bytes.create 8 in
          Bytes.set_int64_be b 0 (Int64.of_int Hash.hash_size);
          Bytes.unsafe_to_string b
        in
        fun x f ->
          f hash_length_header;
          pre_hash x f

      let t = Type.like K.t ~bin:(encode_bin, decode_bin, size_of) ~pre_hash
    end

    module Node_key = K (struct
      type t = C.node_key [@@deriving irmin]
    end)

    module Commit_key = K (struct
      type t = C.commit_key [@@deriving irmin]
    end)

    type node_key = Node_key.t [@@deriving irmin]
    type commit_key = Commit_key.t [@@deriving irmin]
    type t = { parents : commit_key list; c : C.t }

    module Info = Info

    let import c = { c; parents = C.parents c }
    let export t = t.c
    let node t = C.node t.c
    let parents t = t.parents
    let info t = C.info t.c
    let v ~info ~node ~parents = { parents; c = C.v ~node ~parents ~info }
    let make = v

    let t : t Type.t =
      let open Type in
      record "commit" (fun node parents info -> make ~info ~node ~parents)
      |+ field "node" Node_key.t node
      |+ field "parents" (list ~len:`Int64 Commit_key.t) parents
      |+ field "info" Info.t info
      |> sealr
  end
end

include Maker (Info.Default)
