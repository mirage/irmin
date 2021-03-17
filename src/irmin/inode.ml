(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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
include Inode_intf

let src = Logs.Src.create "irmin.i" ~doc:"inodes for irmin backend"

module Log = (val Logs.src_log src : Logs.LOG)

module Make
    (Conf : S.INODE_CONF)
    (H : Hash.S)
    (Node : Node.S with type hash = H.t) =
struct
  let () =
    if Conf.entries > Conf.stable_hash then
      invalid_arg "entries should be lower or equal to stable_hash"

  module Node = struct
    include Node
    module H = Hash.Typed (H) (Node)

    let hash = H.hash
  end

  module T = struct
    type hash = H.t [@@deriving irmin]
    type step = Node.step [@@deriving irmin]
    type metadata = Node.metadata [@@deriving irmin]

    let default = Node.default

    type value = Node.value

    let value_t = Node.value_t
    let pp_hash = Type.(pp hash_t)
  end

  module StepMap = struct
    include Map.Make (struct
      type t = T.step

      let compare = Type.(unstage (compare T.step_t))
    end)

    let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
  end

  (* Binary representation, useful to compute hashes *)
  module Bin = struct
    open T

    type ptr = { index : int; hash : H.t }
    type tree = { depth : int; length : int; entries : ptr list }
    type v = Values of (step * value) list | Tree of tree

    let ptr_t : ptr Type.t =
      let open Type in
      record "Bin.ptr" (fun index hash -> { index; hash })
      |+ field "index" int (fun t -> t.index)
      |+ field "hash" H.t (fun (t : ptr) -> t.hash)
      |> sealr

    let tree_t : tree Type.t =
      let open Type in
      record "Bin.tree" (fun depth length entries -> { depth; length; entries })
      |+ field "depth" int (fun t -> t.depth)
      |+ field "length" int (fun t -> t.length)
      |+ field "entries" (list ptr_t) (fun t -> t.entries)
      |> sealr

    let v_t : v Type.t =
      let open Type in
      variant "Bin.v" (fun values tree -> function
        | Values l -> values l | Tree i -> tree i)
      |~ case1 "Values" (list (pair step_t value_t)) (fun t -> Values t)
      |~ case1 "Tree" tree_t (fun t -> Tree t)
      |> sealv

    module V =
      Hash.Typed
        (H)
        (struct
          type t = v

          let t = v_t
        end)

    type t = { hash : H.t Lazy.t; stable : bool; v : v }

    let pre_hash_v = Type.(unstage (pre_hash v_t))

    let t : t Type.t =
      let open Type in
      let pre_hash = stage (fun x -> pre_hash_v x.v) in
      record "Bin.t" (fun hash stable v -> { hash = lazy hash; stable; v })
      |+ field "hash" H.t (fun t -> Lazy.force t.hash)
      |+ field "stable" bool (fun t -> t.stable)
      |+ field "v" v_t (fun t -> t.v)
      |> sealr
      |> like ~pre_hash

    let v ~stable ~hash v = { stable; hash; v }
    let hash t = Lazy.force t.hash
  end

  (** [Tree] defines the recursive structure of inodes.

      {3 Inode Layout}

      {4 Layout Types}

      The layout ['a layout] associated to an inode ['a t] defines certain
      properties of the inode:

      - When [Total], the inode is self contained and immutable.
      - When [Partial], chunks of the inode might be missing but they can be
        fetched from the backend when needed using the available [find] function
        stored in the layout. Mutable pointers act as cache.
      - When [Truncated], chunks of the inode might be missing. Those chunks are
        unreachable because the pointer to the backend is missing. The inode is
        immutable.

      {4 Layout Instantiation}

      The layout of an inode is determined from the module [Val], it depends on
      the way the inode was constructed:

      - When [Total], it originates from [Val.v] or [Val.empty].
      - When [Partial], it originates from [Val.of_bin], which is only used by
        [Inode.find].
      - When [Truncated], it originates from an [Irmin.Type] deserialisation
        made possible by [Val.t].

      Almost all other functions in [Tree] are polymorphic regarding the layout
      of the manipulated inode.

      {4 Details on the [Truncated] Layout}

      The [Truncated] layout is identical to [Partial] except for the missing
      [find] function.

      On the one hand, when creating the root of a [Truncated] inode, the
      pointers to children inodes - if any - are set to the [Broken] tag,
      meaning that we know the hash to such children but we will have to way to
      load them in the future. On the other hand, when adding children to a
      [Truncated] inode, there is no such problem, the pointer is then set to
      the [Intact] tag.

      As of Irmin 2.4 (February 2021), inode deserialisation using Repr happens
      in [irmin/slice.ml] and [irmin/sync_ext.ml], and maybe some other places.

      At some point we might want to forbid such deserialisations and instead
      use something in the flavour of [Val.of_bin] to create [Partial] inodes.

      {3 Topmost Inode Ancestor}

      [Tree.t] is a recursive type, it is labelled with a [depth] integer that
      indicates the recursion depth. An inode with [depth = 0] corresponds to
      the root of a directory, its hash is the hash of the directory.

      A [Val.t] points to the topmost [Tree.t] of an inode tree. In most
      scenarios, that topmost inode has [depth = 0], but it is also legal for
      the topmost inode to be an intermediate inode, i.e. with [depth > 0].

      The only way for an inode tree to have an intermediate inode as root is to
      fetch it from the backend by calling [Make_ext.find], using the hash of
      that inode.

      Write-only operations are not permitted when the root is an intermediate
      inode. *)
  module Tree = struct
    open T

    let equal_value = Type.(unstage (equal value_t))

    type _ layout =
      | Total : total_ptr layout
      | Partial : (hash -> partial_ptr t option Lwt.t) -> partial_ptr layout
      | Truncated : truncated_ptr layout

    and partial_ptr = {
      target_hash : hash Lazy.t;
      mutable target : partial_ptr t option;
    }
    (** [mutable target : partial_ptr t option] could be turned to
        [target : partial_ptr t Lazy.t] to make the code even clearer (we never
        set it back to [None]), but we might soon implement a garbage collection
        method for inodes that will necessitate that mutable option (among other
        things). *)

    and total_ptr = Total_ptr of total_ptr t [@@unboxed]

    and truncated_ptr = Broken of hash | Intact of truncated_ptr t

    and 'ptr tree = { depth : int; length : int; entries : 'ptr option array }

    and 'ptr v = Values of value StepMap.t | Tree of 'ptr tree

    and 'ptr t = { hash : hash Lazy.t; stable : bool; v : 'ptr v }

    module Ptr = struct
      let hash : type ptr. ptr layout -> ptr -> _ = function
        | Total -> fun (Total_ptr ptr) -> Lazy.force ptr.hash
        | Partial _ -> fun { target_hash; _ } -> Lazy.force target_hash
        | Truncated -> (
            function Broken h -> h | Intact ptr -> Lazy.force ptr.hash)

      let target : type ptr. ptr layout -> ptr -> ptr t Lwt.t =
       fun layout ->
        match layout with
        | Total -> fun (Total_ptr t) -> Lwt.return t
        | Partial find -> (
            function
            | { target = Some entry; _ } -> Lwt.return entry
            | t -> (
                let h = hash layout t in
                let+ v = find h in
                match v with
                | None -> Fmt.failwith "%a: unknown key" pp_hash h
                | Some x ->
                    t.target <- Some x;
                    x))
        | Truncated -> (
            function
            | Intact entry -> Lwt.return entry
            | _ ->
                failwith
                  "Impossible to load the subtree on an inode deserialized \
                   using Repr")

      let of_target : type ptr. ptr layout -> ptr t -> ptr = function
        | Total -> fun target -> Total_ptr target
        | Partial _ ->
            fun target -> { target = Some target; target_hash = target.hash }
        | Truncated -> fun target -> Intact target

      let of_hash : type ptr. ptr layout -> hash -> ptr = function
        | Total -> assert false
        | Partial _ -> fun hash -> { target = None; target_hash = lazy hash }
        | Truncated -> fun hash -> Broken hash

      let iter_if_loaded :
          type ptr.
          broken:(hash -> unit Lwt.t) ->
          ptr layout ->
          (ptr t -> unit Lwt.t) ->
          ptr ->
          unit Lwt.t =
       fun ~broken -> function
        | Total -> fun f (Total_ptr entry) -> f entry
        | Partial _ -> (
            fun f -> function
              | { target = Some entry; _ } -> f entry
              | _ -> Lwt.return_unit)
        | Truncated -> (
            fun f -> function Broken h -> broken h | Intact entry -> f entry)
    end

    let pred layout t =
      match t.v with
      | Tree i ->
          let hash_of_ptr = Ptr.hash layout in
          Array.fold_left
            (fun acc -> function
              | None -> acc
              | Some ptr -> `Inode (hash_of_ptr ptr) :: acc)
            [] i.entries
      | Values l ->
          StepMap.fold
            (fun _ v acc ->
              let v =
                match v with
                | `Node _ as k -> k
                | `Contents (k, _) -> `Contents k
              in
              v :: acc)
            l []

    let length_of_v = function
      | Values vs -> StepMap.cardinal vs
      | Tree vs -> vs.length

    let length t = length_of_v t.v
    let stable t = t.stable

    type acc = {
      cursor : int;
      values : (step * value) list list;
      remaining : int;
    }

    let empty_acc n = { cursor = 0; values = []; remaining = n }

    let rec list_entry layout ~offset ~length acc = function
      | None -> Lwt.return acc
      | Some i ->
          let* target = Ptr.target layout i in
          list_values layout ~offset ~length acc target.v

    and list_tree layout ~offset ~length acc t =
      if acc.remaining <= 0 || offset + length <= acc.cursor then Lwt.return acc
      else if acc.cursor + t.length < offset then
        Lwt.return { acc with cursor = t.length + acc.cursor }
      else
        Array.fold_left
          (fun acc e ->
            let* acc = acc in
            list_entry layout ~offset ~length acc e)
          (Lwt.return acc) t.entries

    and list_values layout ~offset ~length acc v =
      if acc.remaining <= 0 || offset + length <= acc.cursor then Lwt.return acc
      else
        match v with
        | Values vs ->
            let len = StepMap.cardinal vs in
            if acc.cursor + len < offset then
              Lwt.return { acc with cursor = len + acc.cursor }
            else
              let to_drop =
                if acc.cursor > offset then 0 else offset - acc.cursor
              in
              let vs =
                StepMap.to_seq vs |> Seq.drop to_drop |> Seq.take acc.remaining
              in
              let n = List.length vs in
              Lwt.return
                {
                  values = vs :: acc.values;
                  cursor = acc.cursor + len;
                  remaining = acc.remaining - n;
                }
        | Tree t -> list_tree layout ~offset ~length acc t

    let list_v layout ?(offset = 0) ?length v =
      let length =
        match length with
        | Some n -> n
        | None -> (
            match v with
            | Values vs -> StepMap.cardinal vs - offset
            | Tree i -> i.length - offset)
      in
      let+ entries = list_values layout ~offset ~length (empty_acc length) v in
      List.concat (List.rev entries.values)

    let list layout ?offset ?length t = list_v layout ?offset ?length t.v

    let to_bin_v layout = function
      | Values vs ->
          let vs = StepMap.bindings vs in
          Bin.Values vs
      | Tree t ->
          let hash_of_ptr = Ptr.hash layout in
          let _, entries =
            Array.fold_left
              (fun (i, acc) -> function
                | None -> (i + 1, acc)
                | Some ptr ->
                    let hash = hash_of_ptr ptr in
                    (i + 1, { Bin.index = i; hash } :: acc))
              (0, []) t.entries
          in
          let entries = List.rev entries in
          Bin.Tree { depth = t.depth; length = t.length; entries }

    let to_bin layout t =
      let v = to_bin_v layout t.v in
      Bin.v ~stable:t.stable ~hash:t.hash v

    module Concrete = struct
      type kind = Contents | Contents_x of metadata | Node [@@deriving irmin]
      type entry = { name : step; kind : kind; hash : hash } [@@deriving irmin]

      type 'a pointer = { index : int; pointer : hash; tree : 'a }
      [@@deriving irmin]

      type 'a tree = { depth : int; length : int; pointers : 'a pointer list }
      [@@deriving irmin]

      type t = Tree of t tree | Value of entry list [@@deriving irmin]

      let metadata_equal = Type.(unstage (equal metadata_t))

      let to_entry (name, v) =
        match v with
        | `Contents (hash, m) ->
            if metadata_equal m Node.default then
              { name; kind = Contents; hash }
            else { name; kind = Contents_x m; hash }
        | `Node hash -> { name; kind = Node; hash }

      let of_entry e =
        ( e.name,
          match e.kind with
          | Contents -> `Contents (e.hash, Node.default)
          | Contents_x m -> `Contents (e.hash, m)
          | Node -> `Node e.hash )

      type error =
        [ `Invalid_hash of hash * hash * t
        | `Invalid_depth of int * int * t
        | `Invalid_length of int * int * t
        | `Duplicated_entries of t
        | `Duplicated_pointers of t
        | `Unsorted_entries of t
        | `Unsorted_pointers of t
        | `Empty ]
      [@@deriving irmin]

      let rec length = function
        | Value l -> List.length l
        | Tree t ->
            List.fold_left (fun acc p -> acc + length p.tree) 0 t.pointers

      let pp = Type.pp_json t

      let pp_error ppf = function
        | `Invalid_hash (got, expected, t) ->
            Fmt.pf ppf "invalid hash for %a@,got: %a@,expecting: %a" pp t
              pp_hash got pp_hash expected
        | `Invalid_depth (got, expected, t) ->
            Fmt.pf ppf "invalid depth for %a@,got: %d@,expecting: %d" pp t got
              expected
        | `Invalid_length (got, expected, t) ->
            Fmt.pf ppf "invalid length for %a@,got: %d@,expecting: %d" pp t got
              expected
        | `Duplicated_entries t -> Fmt.pf ppf "duplicated entries: %a" pp t
        | `Duplicated_pointers t -> Fmt.pf ppf "duplicated pointers: %a" pp t
        | `Unsorted_entries t -> Fmt.pf ppf "entries should be sorted: %a" pp t
        | `Unsorted_pointers t ->
            Fmt.pf ppf "pointers should be sorted: %a" pp t
        | `Empty -> Fmt.pf ppf "concrete subtrees cannot be empty"
    end

    let to_concrete (la : 'ptr layout) (t : 'ptr t) =
      let rec aux t =
        match t.v with
        | Tree tr ->
            let+ _, pointers =
              Array.fold_left
                (fun acc e ->
                  match e with
                  | None ->
                      let+ i, acc = acc in
                      (i + 1, acc)
                  | Some t ->
                      let* i, acc = acc in
                      let* target = Ptr.target la t in
                      let+ pointer, tree = aux target in
                      (i + 1, { Concrete.index = i; tree; pointer } :: acc))
                (Lwt.return (0, []))
                tr.entries
            in
            ( Lazy.force t.hash,
              Concrete.Tree
                {
                  depth = tr.depth;
                  length = tr.length;
                  pointers = List.rev pointers;
                } )
        | Values l ->
            Lwt.return
              ( Lazy.force t.hash,
                Concrete.Value (List.map Concrete.to_entry (StepMap.bindings l))
              )
      in
      let+ _, tree = aux t in
      tree

    exception Invalid_hash of hash * hash * Concrete.t
    exception Invalid_depth of int * int * Concrete.t
    exception Invalid_length of int * int * Concrete.t
    exception Empty
    exception Duplicated_entries of Concrete.t
    exception Duplicated_pointers of Concrete.t
    exception Unsorted_entries of Concrete.t
    exception Unsorted_pointers of Concrete.t

    let hash_equal = Type.(unstage (equal hash_t))

    let of_concrete_exn t =
      let sort_entries =
        List.sort_uniq (fun x y -> compare x.Concrete.name y.Concrete.name)
      in
      let sort_pointers =
        List.sort_uniq (fun x y -> compare x.Concrete.index y.Concrete.index)
      in
      let check_entries t es =
        if es = [] then raise Empty;
        let s = sort_entries es in
        if List.length s <> List.length es then raise (Duplicated_entries t);
        if s <> es then raise (Unsorted_entries t)
      in
      let check_pointers t ps =
        if ps = [] then raise Empty;
        let s = sort_pointers ps in
        if List.length s <> List.length ps then raise (Duplicated_pointers t);
        if s <> ps then raise (Unsorted_pointers t)
      in
      let hash v = Bin.V.hash (to_bin_v Total v) in
      let rec aux depth t =
        match t with
        | Concrete.Value l ->
            check_entries t l;
            Values (StepMap.of_list (List.map Concrete.of_entry l))
        | Concrete.Tree tr ->
            let entries = Array.make Conf.entries None in
            check_pointers t tr.pointers;
            List.iter
              (fun { Concrete.index; pointer; tree } ->
                let v = aux (depth + 1) tree in
                let hash = hash v in
                if not (hash_equal hash pointer) then
                  raise (Invalid_hash (hash, pointer, t));
                let t = { hash = lazy pointer; stable = false; v } in
                entries.(index) <- Some (Ptr.of_target Total t))
              tr.pointers;
            let length = Concrete.length t in
            if depth <> tr.depth then raise (Invalid_depth (depth, tr.depth, t));
            if length <> tr.length then
              raise (Invalid_length (length, tr.length, t));
            Tree { depth = tr.depth; length = tr.length; entries }
      in
      let v = aux 0 t in
      let length = length_of_v v in
      let+ stable, hash =
        if length > Conf.stable_hash then Lwt.return (false, hash v)
        else
          let+ ls = list_v Total v in
          let node = Node.v ls in
          (true, Node.hash node)
      in
      { hash = lazy hash; stable; v }

    let of_concrete t =
      try Ok (of_concrete_exn t) with
      | Invalid_hash (x, y, z) -> Error (`Invalid_hash (x, y, z))
      | Invalid_depth (x, y, z) -> Error (`Invalid_depth (x, y, z))
      | Invalid_length (x, y, z) -> Error (`Invalid_length (x, y, z))
      | Empty -> Error `Empty
      | Duplicated_entries t -> Error (`Duplicated_entries t)
      | Duplicated_pointers t -> Error (`Duplicated_pointers t)
      | Unsorted_entries t -> Error (`Unsorted_entries t)
      | Unsorted_pointers t -> Error (`Unsorted_pointers t)

    let hash t = Lazy.force t.hash

    let is_root t =
      match t.v with
      | Tree { depth; _ } -> depth = 0
      | Values _ ->
          (* When [t] is of tag [Values], then [t] is root iff [t] is stable. It
             is implied by the following.

             When [t] is stable, then [t] is a root, because:
              - Only 2 functions produce stable inodes: [stabilize] and [empty].
              - Only the roots are output of [stabilize].
              - An empty map can only be located at the root.

             When [t] is a root of tag [Value], then [t] is stable, because:
             - All the roots are output of [stabilize].
             - When an unstable inode enters [stabilize], it becomes stable if
               it has at most [Conf.stable_hash] leaves.
             - A [Value] has at most [Conf.stable_hash] leaves because
               [Conf.entries <= Conf.stable_hash] is enforced.
          *)
          t.stable

    let check_write_op_supported t =
      if not @@ is_root t then
        failwith "Cannot perform operation on non-root inode value."

    let empty : 'a. 'a layout -> 'a t =
     fun _ ->
      let hash = lazy (Node.hash Node.empty) in
      { stable = true; hash; v = Values StepMap.empty }

    let values layout vs =
      let length = StepMap.cardinal vs in
      if length = 0 then empty layout
      else
        let v = Values vs in
        let hash = lazy (Bin.V.hash (to_bin_v layout v)) in
        { hash; stable = false; v }

    let tree layout is =
      let v = Tree is in
      let hash = lazy (Bin.V.hash (to_bin_v layout v)) in
      { hash; stable = false; v }

    let hash_key = Type.(unstage (short_hash step_t))
    let index ~depth k = abs (hash_key ~seed:depth k) mod Conf.entries

    module Total : sig
      type nonrec t = total_ptr t

      val list : ?offset:int -> ?length:int -> t -> (step * value) list
      val add : t -> step -> value -> t
      val stabilize : t -> t
    end = struct
      type nonrec t = total_ptr t

      let find_value ~depth t s =
        let rec aux ~depth = function
          | Values vs -> (
              try Some (StepMap.find s vs) with Not_found -> None)
          | Tree t -> (
              let i = index ~depth s in
              let x = t.entries.(i) in
              match x with
              | None -> None
              | Some (Total_ptr t) -> aux ~depth:(depth + 1) t.v)
        in
        aux ~depth t.v

      let empty_acc n = { cursor = 0; values = []; remaining = n }

      let rec list_entry ~offset ~length acc = function
        | None -> acc
        | Some (Total_ptr t) -> list_values ~offset ~length acc t.v

      and list_tree ~offset ~length acc t =
        if acc.remaining <= 0 || offset + length <= acc.cursor then acc
        else if acc.cursor + t.length < offset then
          { acc with cursor = t.length + acc.cursor }
        else
          Array.fold_left
            (fun acc e -> list_entry ~offset ~length acc e)
            acc t.entries

      and list_values ~offset ~length acc v =
        if acc.remaining <= 0 || offset + length <= acc.cursor then acc
        else
          match v with
          | Values vs ->
              let len = StepMap.cardinal vs in
              if acc.cursor + len < offset then
                { acc with cursor = len + acc.cursor }
              else
                let to_drop =
                  if acc.cursor > offset then 0 else offset - acc.cursor
                in
                let vs =
                  StepMap.to_seq vs
                  |> Seq.drop to_drop
                  |> Seq.take acc.remaining
                in
                let n = List.length vs in
                {
                  values = vs :: acc.values;
                  cursor = acc.cursor + len;
                  remaining = acc.remaining - n;
                }
          | Tree t -> list_tree ~offset ~length acc t

      let list_v ?(offset = 0) ?length v =
        let length =
          match length with
          | Some n -> n
          | None -> (
              match v with
              | Values vs -> StepMap.cardinal vs - offset
              | Tree i -> i.length - offset)
        in
        let entries = list_values ~offset ~length (empty_acc length) v in
        List.concat (List.rev entries.values)

      let list ?offset ?length t = list_v ?offset ?length t.v

      let stabilize t =
        if t.stable then t
        else
          let n = length t in
          if n > Conf.stable_hash then t
          else
            let vs = list t in
            let hash = lazy (Node.hash (Node.v vs)) in
            { hash; stable = true; v = t.v }

      let rec add ~depth ~copy ~replace (t : t) s v k =
        match t.v with
        | Values vs ->
            let length =
              if replace then StepMap.cardinal vs else StepMap.cardinal vs + 1
            in
            let t =
              if length <= Conf.entries then values Total (StepMap.add s v vs)
              else
                let vs = StepMap.bindings (StepMap.add s v vs) in
                let empty =
                  tree Total
                    {
                      length = 0;
                      depth;
                      entries = Array.make Conf.entries None;
                    }
                in
                let aux t (s, v) =
                  add ~depth ~copy:false ~replace t s v Fun.id
                in
                List.fold_left aux empty vs
            in
            k t
        | Tree t -> (
            let length = if replace then t.length else t.length + 1 in
            let entries = if copy then Array.copy t.entries else t.entries in
            let i = index ~depth s in
            match entries.(i) with
            | None ->
                let target = values Total (StepMap.singleton s v) in
                entries.(i) <- Some (Ptr.of_target Total target);
                let t = tree Total { depth; length; entries } in
                k t
            | Some n ->
                let (Total_ptr t) = n in
                add ~depth:(depth + 1) ~copy ~replace t s v @@ fun target ->
                entries.(i) <- Some (Ptr.of_target Total target);
                let t = tree Total { depth; length; entries } in
                k t)

      let add t s v =
        (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
        let v' = find_value ~depth:0 t s in
        match v' with
        | Some v' when equal_value v v' -> stabilize t
        | Some _ ->
            let v' = add ~depth:0 ~copy:false ~replace:true t s v Fun.id in
            stabilize v'
        | None ->
            let v' = add ~depth:0 ~copy:false ~replace:false t s v Fun.id in
            stabilize v'
    end

    let stabilize layout t =
      if t.stable then Lwt.return t
      else
        let n = length t in
        if n > Conf.stable_hash then Lwt.return t
        else
          (* FIXME(samoht): this reads the full tree *)
          let+ vs = list layout t in
          let hash = lazy (Node.hash (Node.v vs)) in
          { hash; stable = true; v = t.v }

    (** This function shouldn't be called with the [Total] layout. In the
        future, we could add a polymorphic variant to the GADT parameter to
        enfoce that. *)
    let of_bin layout t =
      let v =
        match t.Bin.v with
        | Bin.Values vs ->
            let vs = StepMap.of_list vs in
            Values vs
        | Tree t ->
            let entries = Array.make Conf.entries None in
            let ptr_of_hash = Ptr.of_hash layout in
            List.iter
              (fun { Bin.index; hash } ->
                entries.(index) <- Some (ptr_of_hash hash))
              t.entries;
            Tree { depth = t.Bin.depth; length = t.length; entries }
      in
      { hash = t.Bin.hash; stable = t.Bin.stable; v }

    let of_values layout l = values layout (StepMap.of_list l)

    let is_empty t =
      match t.v with Values vs -> StepMap.is_empty vs | Tree _ -> false

    let find_value layout ~depth t s =
      let target_of_ptr = Ptr.target layout in
      let rec aux ~depth = function
        | Values vs ->
            Lwt.return (try Some (StepMap.find s vs) with Not_found -> None)
        | Tree t -> (
            let i = index ~depth s in
            let x = t.entries.(i) in
            match x with
            | None -> Lwt.return_none
            | Some i ->
                let* target = target_of_ptr i in
                aux ~depth:(depth + 1) target.v)
      in
      aux ~depth t.v

    let find layout t s = find_value ~depth:0 layout t s

    let rec add layout ~depth ~copy ~replace t s v k =
      match t.v with
      | Values vs ->
          let length =
            if replace then StepMap.cardinal vs else StepMap.cardinal vs + 1
          in
          let* t =
            if length <= Conf.entries then
              Lwt.return (values layout (StepMap.add s v vs))
            else
              let vs = StepMap.bindings (StepMap.add s v vs) in
              let empty =
                tree layout
                  { length = 0; depth; entries = Array.make Conf.entries None }
              in
              let aux t (s, v) =
                let* t = t in
                add layout ~depth ~copy:false ~replace t s v Lwt.return
              in
              List.fold_left aux (Lwt.return empty) vs
          in
          k t
      | Tree t -> (
          let length = if replace then t.length else t.length + 1 in
          let entries = if copy then Array.copy t.entries else t.entries in
          let i = index ~depth s in
          match entries.(i) with
          | None ->
              let target = values layout (StepMap.singleton s v) in
              entries.(i) <- Some (Ptr.of_target layout target);
              let t = tree layout { depth; length; entries } in
              k t
          | Some n ->
              let* t = Ptr.target layout n in
              add layout ~depth:(depth + 1) ~copy ~replace t s v
              @@ fun target ->
              entries.(i) <- Some (Ptr.of_target layout target);
              let t = tree layout { depth; length; entries } in
              k t)

    let add layout ~copy t s v =
      (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
      let* v' = find_value ~depth:0 layout t s in
      match v' with
      | Some v' when equal_value v v' -> stabilize layout t
      | Some _ ->
          let* v' = add ~depth:0 layout ~copy ~replace:true t s v Lwt.return in
          stabilize layout v'
      | None ->
          let* v' = add ~depth:0 layout ~copy ~replace:false t s v Lwt.return in
          stabilize layout v'

    let rec remove layout ~depth t s k =
      match t.v with
      | Values vs ->
          let t = values layout (StepMap.remove s vs) in
          k t
      | Tree t -> (
          let len = t.length - 1 in
          if len <= Conf.entries then
            let* vs =
              list_tree layout ~offset:0 ~length:t.length (empty_acc t.length) t
            in
            let vs = List.concat (List.rev vs.values) in
            let vs = StepMap.of_list vs in
            let vs = StepMap.remove s vs in
            let t = values layout vs in
            k t
          else
            let entries = Array.copy t.entries in
            let i = index ~depth s in
            match entries.(i) with
            | None -> assert false
            | Some t ->
                let* t = Ptr.target layout t in
                if length t = 1 then (
                  entries.(i) <- None;
                  let t = tree layout { depth; length = len; entries } in
                  k t)
                else
                  remove ~depth:(depth + 1) layout t s @@ fun target ->
                  entries.(i) <- Some (Ptr.of_target layout target);
                  let t = tree layout { depth; length = len; entries } in
                  k t)

    let remove layout t s =
      (* XXX: [find_value ~depth:42] should break the unit tests. It doesn't. *)
      let* v' = find_value layout ~depth:0 t s in
      match v' with
      | None -> stabilize layout t
      | Some _ ->
          let* v' = remove layout ~depth:0 t s Lwt.return in
          stabilize layout v'

    let v l =
      let len = List.length l in
      let t =
        if len <= Conf.entries then of_values Total l
        else
          let aux acc (s, v) = Total.add acc s v in
          List.fold_left aux (empty Total) l
      in
      Total.stabilize t

    let save layout ~add ~mem t =
      let iter_entries =
        let broken h =
          let+ mem = mem h in
          (* This function is called when we encounter a Broken pointer with
             Truncated layouts. *)
          if not mem then
            Fmt.failwith
              "You are trying to save to the backend an inode deserialized \
               using [Irmin.Type] that used to contain pointer(s) to inodes \
               which are unknown to the backend. Hash: %a"
              pp_hash h
          else
            (* The backend already knows this target inode, there is no need to
               traverse further down. This happens during the unit tests. *)
            ()
        in
        let iter_ptr = Ptr.iter_if_loaded ~broken layout in
        fun f arr ->
          Array.fold_left
            (fun acc e ->
              let* () = acc in
              match e with None -> Lwt.return_unit | Some e -> iter_ptr f e)
            Lwt.return_unit arr
      in
      let rec aux ~depth t =
        Log.debug (fun l -> l "save depth:%d" depth);
        match t.v with
        | Values _ ->
            let key = Lazy.force t.hash in
            let value = to_bin layout t in
            add key value
        | Tree n ->
            let* () =
              iter_entries
                (fun t ->
                  let key = Lazy.force t.hash in
                  let* mem = mem key in
                  if mem then Lwt.return_unit else aux ~depth:(depth + 1) t)
                n.entries
            in
            let key = Lazy.force t.hash in
            let value = to_bin layout t in
            add key value
      in
      aux ~depth:0 t

    let check_stable layout t =
      let target_of_ptr = Ptr.target layout in
      let rec check t any_stable_ancestor =
        let stable = t.stable || any_stable_ancestor in
        match t.v with
        | Values _ -> Lwt.return true
        | Tree tree ->
            Array.fold_left
              (fun acc e ->
                let* acc = acc in
                if not acc then Lwt.return false
                else
                  match e with
                  | None -> Lwt.return true
                  | Some t ->
                      let* t = target_of_ptr t in
                      let+ check = check t stable in
                      (if stable then not t.stable else true) && check)
              (Lwt.return true) tree.entries
      in
      check t t.stable

    let contains_empty_map layout t =
      let target_of_ptr = Ptr.target layout in
      let rec check_lower t =
        match t.v with
        | Values l when StepMap.is_empty l -> Lwt.return true
        | Values _ -> Lwt.return false
        | Tree inodes ->
            Array.fold_left
              (fun acc e ->
                let* acc = acc in
                if acc then Lwt.return true
                else
                  match e with
                  | None -> Lwt.return false
                  | Some t ->
                      let* t = target_of_ptr t in
                      check_lower t)
              (Lwt.return false) inodes.entries
      in
      check_lower t

    let is_tree t = match t.v with Tree _ -> true | Values _ -> false
  end

  include T
  module I = Tree

  let pp_hash = T.pp_hash

  type t =
    | Total of I.total_ptr I.t
    | Partial of I.partial_ptr I.layout * I.partial_ptr I.t
    | Truncated of I.truncated_ptr I.t

  type 'b apply_fn = { f : 'a. 'a I.layout -> 'a I.t -> 'b } [@@unboxed]

  let apply : t -> 'b apply_fn -> 'b =
   fun t f ->
    match t with
    | Total v -> f.f I.Total v
    | Partial (layout, v) -> f.f layout v
    | Truncated v -> f.f I.Truncated v

  type map_fn = { f : 'a. 'a I.layout -> 'a I.t -> 'a I.t Lwt.t } [@@unboxed]

  let map : t -> map_fn -> t Lwt.t =
   fun t f ->
    match t with
    | Total v ->
        let+ v' = f.f I.Total v in
        if v == v' then t else Total v'
    | Partial (layout, v) ->
        let+ v' = f.f layout v in
        if v == v' then t else Partial (layout, v')
    | Truncated v ->
        let+ v' = f.f I.Truncated v in
        if v == v' then t else Truncated v'

  let pred t = apply t { f = (fun layout v -> I.pred layout v) }

  let v l =
    let i = I.v l in
    Total i

  let list ?offset ?length t =
    apply t { f = (fun layout v -> I.list layout ?offset ?length v) }

  let empty = v []
  let is_empty t = apply t { f = (fun _ v -> I.is_empty v) }
  let find t s = apply t { f = (fun layout v -> I.find layout v s) }

  let add t s value =
    Log.debug (fun l ->
        l "inode.val.add %a %a" (Type.pp step_t) s (Type.pp value_t) value);
    let f layout v =
      I.check_write_op_supported v;
      I.add ~copy:true layout v s value
    in
    map t { f }

  let remove t s =
    let f layout v =
      I.check_write_op_supported v;
      I.remove layout v s
    in
    map t { f }

  let pre_hash_binv = Type.(unstage (pre_hash Bin.v_t))
  let pre_hash_node = Type.(unstage (pre_hash Node.t))

  let t : t Type.t =
    let pre_hash =
      Type.stage @@ fun x ->
      let stable = apply x { f = (fun _ v -> I.stable v) } in
      if not stable then
        let bin = apply x { f = (fun layout v -> I.to_bin layout v) } in
        pre_hash_binv bin.v
      else
        match x with
        | Total x ->
            let vs = I.Total.list x in
            pre_hash_node (Node.v vs)
        | Partial _ -> invalid_arg "pre_hash: partial inode"
        | Truncated _ -> invalid_arg "pre_hash: truncated inode"
    in
    Type.map ~pre_hash Bin.t
      (fun bin -> Truncated (I.of_bin I.Truncated bin))
      (fun x -> apply x { f = (fun layout v -> I.to_bin layout v) })

  let hash t = apply t { f = (fun _ v -> I.hash v) }

  let save ~add ~mem t =
    let f layout v =
      I.check_write_op_supported v;
      I.save layout ~add ~mem v
    in
    apply t { f }

  let of_bin find' v =
    let rec find h =
      let+ v = find' h in
      match v with None -> None | Some v -> Some (I.of_bin layout v)
    and layout = I.Partial find in
    Partial (layout, I.of_bin layout v)

  let to_bin t = apply t { f = (fun layout v -> I.to_bin layout v) }
  let stable t = apply t { f = (fun _ v -> I.stable v) }
  let length t = apply t { f = (fun _ v -> I.length v) }
  let index = I.index

  let integrity_check t =
    let f layout v =
      let check_stable () =
        let check () = I.check_stable layout v in
        let n = length t in
        if n > Conf.stable_hash then
          if stable t then Lwt.return false else check ()
        else if not (stable t) then Lwt.return true
        else check ()
      in
      let contains_empty_map_non_root () =
        let check () = I.contains_empty_map layout v in
        (* we are only looking for empty maps that are not at the root *)
        if I.is_tree v then check () else Lwt.return false
      in
      let* x = check_stable () in
      let+ y = contains_empty_map_non_root () in
      x && not y
    in
    apply t { f }

  module Concrete = I.Concrete

  let to_concrete t = apply t { f = (fun la v -> I.to_concrete la v) }

  let of_concrete t =
    match I.of_concrete t with
    | Ok t ->
        let+ t = t in
        Ok (Total t)
    | Error _ as e -> Lwt.return e
end

module Raw_store
    (CA : S.CONTENT_ADDRESSABLE_STORE_MAKER)
    (K : Hash.S)
    (V : S with type hash = K.t) =
struct
  module Val = V
  module Key = K
  module CA = CA (K) (Val.Bin)

  type 'a t = 'a CA.t
  type key = Key.t
  type value = Val.t

  let mem t k = CA.mem t k

  let find t k =
    let+ v = CA.find t k in
    match v with
    | None -> None
    | Some v ->
        let find = CA.find t in
        let v = V.of_bin find v in
        Some v

  let save t v =
    let add k v = CA.unsafe_add t k v in
    Val.save ~add ~mem:(CA.mem t) v

  let hash v = Val.hash v

  let add t v =
    let+ () = save t v in
    hash v

  let equal_hash = Type.(unstage (equal K.t))

  let check_hash expected got =
    if equal_hash expected got then ()
    else
      Fmt.invalid_arg "corrupted value: got %a, expecting %a" Val.pp_hash
        expected Val.pp_hash got

  let unsafe_add t k v =
    check_hash k (hash v);
    save t v

  let batch = CA.batch
  let v = CA.v
  let close = CA.close
  let clear = CA.clear
end
