(*
 * Copyright (c) 2013-2019 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type CONFIG = sig
  val entries : int
end

module Make
    (Conf : CONFIG)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S with type hash = H.t)
    (Pack : Pack.MAKER with type key = Node.hash) =
struct
  type hash = Node.hash

  type step = Node.step

  type metadata = Node.metadata

  let step_t = Node.step_t

  let hash_t = Node.hash_t

  let metadata_t = Node.metadata_t

  type 'a item = { magic : char; hash : hash; v : 'a }

  let item a =
    let open Irmin.Type in
    record "value" (fun hash magic v -> { magic; hash; v })
    |+ field "hash" hash_t (fun v -> v.hash)
    |+ field "magic" char (fun v -> v.magic)
    |+ field "v" a (fun v -> v.v)
    |> sealr

  module Val = struct
    type entry =
      | Node of { name : step; node : hash }
      | Contents of { metadata : metadata; name : step; node : hash }
      | Inode of { index : int; node : hash }

    let entry : entry Irmin.Type.t =
      let open Irmin.Type in
      variant "Inode.entry" (fun node contents inode -> function
        | Node n -> node (n.name, n.node)
        | Contents c -> contents (c.metadata, c.name, c.node)
        | Inode i -> inode (i.index, i.node) )
      |~ case1 "Node" (pair step_t hash_t) (fun (name, node) ->
             Node { name; node } )
      |~ case1 "Contents" (triple metadata_t step_t hash_t)
           (fun (metadata, name, node) -> Contents { metadata; name; node })
      |~ case1 "Inode" (pair int hash_t) (fun (index, node) ->
             Inode { index; node } )
      |> sealv

    (* hash is the hash of the corresponding sub-node *)
    type t = entry list item

    module H = Irmin.Hash.Typed (H) (Node)

    let hash = H.hash

    let magic = 'T'

    let empty = { magic; hash = hash Node.empty; v = [] }

    let v ~hash v = { magic; hash; v }

    let t = item (Irmin.Type.list entry)

    let entry_of_value name v =
      match v with
      | `Node node -> Node { name; node }
      | `Contents (node, metadata) -> Contents { metadata; name; node }

    let entries_of_values l =
      let v =
        List.fold_left
          (fun acc (s, v) -> entry_of_value s v :: acc)
          [] (List.rev l)
      in
      let hash = hash (Node.v l) in
      { magic; hash; v }

    let entry_of_inode index node = Inode { index; node }
  end

  module Tree = struct
    module StepMap = struct
      include Map.Make (struct
        type t = step

        let compare = Irmin.Type.compare step_t
      end)

      let of_list l = List.fold_left (fun acc (k, v) -> add k v acc) empty l
    end

    type t = Values of Node.value StepMap.t | Nodes of t array | Empty

    let empty : t = Empty

    let list (t : t) =
      let rec aux acc = function
        | Empty -> acc
        | Values t ->
            List.fold_left (fun acc e -> e :: acc) acc (StepMap.bindings t)
        | Nodes n -> Array.fold_left aux acc n
      in
      aux [] t

    module H_entries =
      Irmin.Hash.Typed
        (H)
        (struct
          type t = Val.entry list

          let t = Irmin.Type.list Val.entry
        end)

    let hash_entries = H_entries.hash

    let index ~seed k =
      abs (Irmin.Type.short_hash step_t ~seed k) mod Conf.entries

    let singleton s v = Values (StepMap.singleton s v)

    let rec add_values : type a. seed:_ -> _ -> _ -> _ -> _ -> (t -> a) -> a =
     fun ~seed t vs s v k ->
      let values = StepMap.add s v vs in
      if values == vs then k t
      else if StepMap.cardinal values <= Conf.entries then k (Values values)
      else (nodes_of_values [@tailcall]) ~seed values @@ fun n -> k (Nodes n)

    and nodes_of_values : type a. seed:_ -> _ -> (t array -> a) -> a =
     fun ~seed entries k ->
      let n = Array.make Conf.entries empty in
      StepMap.iter (fun s e -> (update_nodes [@tailcall]) ~seed n s e) entries;
      k n

    and with_node : type a.
        seed:_ -> copy:_ -> _ -> _ -> _ -> (int -> t -> t -> a) -> a =
     fun ~seed ~copy n s v k ->
      let i = index ~seed s in
      let x = n.(i) in
      match x with
      | Empty -> k i x (singleton s v)
      | Values vs ->
          (add_values [@tailcall]) ~seed:(seed + 1) x vs s v @@ fun y ->
          k i x y
      | Nodes n ->
          (add_nodes [@tailcall]) ~seed:(seed + 1) ~copy x n s v @@ fun y ->
          k i x y

    and update_nodes ~seed n s e =
      with_node ~seed ~copy:false n s e @@ fun i x y ->
      if x != y then n.(i) <- y

    and add_nodes : type a.
        seed:_ -> copy:_ -> t -> _ -> _ -> _ -> (t -> a) -> a =
     fun ~seed ~copy t n s e k ->
      with_node ~seed ~copy n s e @@ fun i x y ->
      if x == y then k t
      else
        let n = if copy then Array.copy n else n in
        n.(i) <- y;
        k (Nodes n)

    let values l = Values (StepMap.of_list l)

    let v l : t =
      if List.length l < Conf.entries then values l
      else
        let aux t (s, v) =
          match t with
          | Empty -> singleton s v
          | Values vs -> (add_values [@tailcall]) ~seed:0 t vs s v (fun x -> x)
          | Nodes n ->
              (add_nodes [@tailcall]) ~seed:0 ~copy:false t n s v (fun x -> x)
        in
        List.fold_left aux empty l

    let fold f t ~hash init =
      let rec inode t k =
        match t with
        | Empty -> k Val.empty
        | Values vs ->
            let values = StepMap.bindings vs in
            assert (List.length values <= Conf.entries);
            k (Val.entries_of_values values)
        | Nodes n ->
            (inodes [@tailcall]) n 0 @@ fun entries ->
            let hash = hash_entries entries in
            k (Val.v ~hash entries)
      and inodes n i k =
        if i >= Array.length n then k []
        else
          match n.(i) with
          | Empty -> inodes n (i + 1) k
          | Values es when StepMap.cardinal es = 1 ->
              let s, v = StepMap.choose es in
              (inodes [@tailcall]) n (i + 1) @@ fun entries ->
              let entries = Val.entry_of_value s v :: entries in
              k entries
          | _ ->
              (inode [@tailcall]) n.(i) @@ fun t ->
              f t.hash t @@ fun () ->
              (inodes [@tailcall]) n (i + 1) @@ fun entries ->
              let entries = Val.entry_of_inode i t.hash :: entries in
              k entries
      in
      inode t (fun v ->
          let v = { v with hash } in
          init hash v )

    let save ~add t =
      fold (fun k v f -> add k v >>= f) t (fun k v -> add k v >|= fun () -> k)

    let load ~find h =
      let rec inode ~seed h k =
        find h >>= function
        | None -> k None
        | Some { v = []; _ } -> k (Some empty)
        | Some { v = i; _ } ->
            let vs, is =
              List.fold_left
                (fun (vs, is) -> function
                  | Val.Node n -> (StepMap.add n.name (`Node n.node) vs, is)
                  | Contents c ->
                      ( StepMap.add c.name (`Contents (c.node, c.metadata)) vs,
                        is ) | Inode i -> (vs, (i.index, i.node) :: is) )
                (StepMap.empty, []) i
            in
            if is = [] then
              let t = Values vs in
              k (Some t)
            else
              (nodes_of_values [@tailcall]) ~seed vs @@ fun n ->
              (inodes [@tailcall]) ~seed n is @@ fun n -> k (Some (Nodes n))
      and inodes ~seed n l k =
        match l with
        | [] -> k n
        | (i, h) :: t -> (
            (inode [@tailcall]) ~seed:(seed + 1) h @@ fun v ->
            (inodes [@tailcall]) ~seed n t @@ fun n ->
            match v with
            | None -> k n
            | Some v ->
                assert (i < Conf.entries);
                assert (n.(i) = Empty);
                n.(i) <- v;
                k n )
      in
      inode ~seed:0 h (function
        | None -> Lwt.return None
        | Some x -> Lwt.return (Some x) )
  end

  module Compress = struct
    type name = Indirect of int | Direct of step

    type address = Indirect of int64 | Direct of H.t

    type entry =
      | Contents of name * address * metadata
      | Node of name * address
      | Inode of int * address

    let entry : entry Irmin.Type.t =
      let open Irmin.Type in
      variant "Compress.entry"
        (fun contents_ii
        node_ii
        inode_i
        contents_id
        node_id
        inode_d
        contents_di
        node_di
        contents_dd
        node_dd
        -> function
        | Contents (Indirect n, Indirect h, m) -> contents_ii (n, h, m)
        | Node (Indirect n, Indirect h) -> node_ii (n, h)
        | Inode (n, Indirect h) -> inode_i (n, h)
        | Contents (Indirect n, Direct h, m) -> contents_id (n, h, m)
        | Node (Indirect n, Direct h) -> node_id (n, h)
        | Inode (n, Direct h) -> inode_d (n, h)
        | Contents (Direct n, Indirect h, m) -> contents_di (n, h, m)
        | Node (Direct n, Indirect h) -> node_di (n, h)
        | Contents (Direct n, Direct h, m) -> contents_dd (n, h, m)
        | Node (Direct n, Direct h) -> node_dd (n, h) )
      |~ case1 "contents-ii" (triple int int64 metadata_t) (fun (n, i, m) ->
             Contents (Indirect n, Indirect i, m) )
      |~ case1 "node-ii" (pair int int64) (fun (n, i) ->
             Node (Indirect n, Indirect i) )
      |~ case1 "inode-i" (pair int int64) (fun (n, i) -> Inode (n, Indirect i))
      |~ case1 "contents-id" (triple int H.t metadata_t) (fun (n, h, m) ->
             Contents (Indirect n, Direct h, m) )
      |~ case1 "node-id" (pair int H.t) (fun (n, h) ->
             Node (Indirect n, Direct h) )
      |~ case1 "inode-d" (pair int H.t) (fun (n, h) -> Inode (n, Direct h))
      |~ case1 "contents-di" (triple step_t int64 metadata_t) (fun (n, i, m) ->
             Contents (Direct n, Indirect i, m) )
      |~ case1 "node-di" (pair step_t int64) (fun (n, i) ->
             Node (Direct n, Indirect i) )
      |~ case1 "contents-dd" (triple step_t H.t metadata_t) (fun (n, i, m) ->
             Contents (Direct n, Direct i, m) )
      |~ case1 "node-dd" (pair step_t H.t) (fun (n, i) ->
             Node (Direct n, Direct i) )
      |> sealv

    let t = item (Irmin.Type.list entry)
  end

  include Pack.Make (struct
    include Val

    let hash t = t.hash

    let encode_bin ~dict ~offset (t : t) k =
      assert (Irmin.Type.equal H.t k t.hash);
      let step s : Compress.name =
        let str = Irmin.Type.to_bin_string step_t s in
        if String.length str <= 4 then Direct s
        else
          let s = dict str in
          Indirect s
      in
      let hash h : Compress.address =
        match offset h with
        | None -> Compress.Direct h
        | Some off -> Compress.Indirect off
      in
      let inode : entry -> Compress.entry = function
        | Contents c ->
            let s = step c.name in
            let v = hash c.node in
            Compress.Contents (s, v, c.metadata)
        | Node n ->
            let s = step n.name in
            let v = hash n.node in
            Compress.Node (s, v)
        | Inode i ->
            let v = hash i.node in
            Compress.Inode (i.index, v)
      in
      (* List.map is fine here as the number of entries is small *)
      let inodes = List.map inode t.v in
      Irmin.Type.encode_bin Compress.t (Val.v ~hash:k inodes)

    exception Exit of [ `Msg of string ]

    let decode_bin ~dict ~hash t off : t =
      let _, i = Irmin.Type.decode_bin ~headers:false Compress.t t off in
      let step : Compress.name -> step = function
        | Direct n -> n
        | Indirect s -> (
          match dict s with
          | None -> raise_notrace (Exit (`Msg "dict"))
          | Some s -> (
            match Irmin.Type.of_bin_string step_t s with
            | Error e -> raise_notrace (Exit e)
            | Ok v -> v ) )
      in
      let hash : Compress.address -> H.t = function
        | Indirect off -> hash off
        | Direct n -> n
      in
      let inode : Compress.entry -> entry = function
        | Contents (n, h, metadata) ->
            let name = step n in
            let node = hash h in
            Contents { name; node; metadata }
        | Node (n, h) ->
            let name = step n in
            let node = hash h in
            Node { name; node }
        | Inode (index, h) ->
            let node = hash h in
            Inode { index; node }
      in
      try
        (* List.map is fine here as the number of inodes is small. *)
        let entries = List.map inode i.v in
        Val.v ~hash:i.hash entries
      with Exit (`Msg e) -> failwith e
  end)
end
