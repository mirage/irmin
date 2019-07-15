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

let src = Logs.Src.create "irmin.pack" ~doc:"irmin-pack backend"

module Log = (val Logs.src_log src : Logs.LOG)

let current_version = "00000001"

let fresh_key =
  Irmin.Private.Conf.key ~doc:"Start with a fresh disk." "fresh"
    Irmin.Private.Conf.bool false

let lru_size_key =
  Irmin.Private.Conf.key ~doc:"Size of the LRU cache for pack entries."
    "lru-size" Irmin.Private.Conf.int 10_000

let readonly_key =
  Irmin.Private.Conf.key ~doc:"Start with a read-only disk." "readonly"
    Irmin.Private.Conf.bool false

let fresh config = Irmin.Private.Conf.get config fresh_key

let lru_size config = Irmin.Private.Conf.get config lru_size_key

let readonly config = Irmin.Private.Conf.get config readonly_key

let root_key = Irmin.Private.Conf.root

let root config =
  match Irmin.Private.Conf.get config root_key with
  | None -> failwith "no root set"
  | Some r -> r

let config ?(fresh = false) ?(readonly = false) ?(lru_size = 10_000) root =
  let config = Irmin.Private.Conf.empty in
  let config = Irmin.Private.Conf.add config fresh_key fresh in
  let config = Irmin.Private.Conf.add config root_key (Some root) in
  let config = Irmin.Private.Conf.add config lru_size_key lru_size in
  let config = Irmin.Private.Conf.add config readonly_key readonly in
  config

let ( ++ ) = Int64.add

let with_cache = IO.with_cache

open Lwt.Infix

exception RO_Not_Allowed = IO.RO_Not_Allowed

module Dict = Dict
module Pack = Pack
module IO = IO.Unix

module Table (K : Irmin.Type.S) = Hashtbl.Make (struct
  type t = K.t

  let hash (t : t) = Irmin.Type.short_hash K.t t

  let equal (x : t) (y : t) = Irmin.Type.equal K.t x y
end)

module Atomic_write (K : Irmin.Type.S) (V : Irmin.Hash.S) = struct
  module Tbl = Table (K)
  module W = Irmin.Private.Watch.Make (K) (V)

  type key = K.t

  type value = V.t

  type watch = W.watch

  type t = {
    index : int64 Tbl.t;
    cache : V.t Tbl.t;
    block : IO.t;
    lock : Lwt_mutex.t;
    w : W.t
  }

  let page = Bytes.create 4

  let read_length32 ~off block =
    let n = IO.read block ~off page in
    assert (n = 4);
    let n, v = Irmin.Type.(decode_bin int32) (Bytes.unsafe_to_string page) 0 in
    assert (n = 4);
    Int32.to_int v

  let entry = Irmin.Type.(pair (string_of `Int32) V.t)

  let set_entry t ?off k v =
    let k = Irmin.Type.to_bin_string K.t k in
    let buf = Irmin.Type.to_bin_string entry (k, v) in
    match off with
    | None -> IO.append t.block buf
    | Some off -> IO.set t.block buf ~off

  let pp_branch = Irmin.Type.pp K.t

  let unsafe_find t k =
    Log.debug (fun l -> l "[branches] find %a" pp_branch k);
    try Lwt.return (Some (Tbl.find t.cache k))
    with Not_found -> Lwt.return None

  let find t k = Lwt_mutex.with_lock t.lock (fun () -> unsafe_find t k)

  let unsafe_mem t k =
    Log.debug (fun l -> l "[branches] mem %a" pp_branch k);
    try Lwt.return (Tbl.mem t.cache k) with Not_found -> Lwt.return false

  let mem t v = Lwt_mutex.with_lock t.lock (fun () -> unsafe_mem t v)

  let zero =
    match Irmin.Type.of_bin_string V.t (String.make V.hash_size '\000') with
    | Ok x -> x
    | Error _ -> assert false

  let unsafe_remove t k =
    Tbl.remove t.cache k;
    try
      let off = Tbl.find t.index k in
      set_entry t ~off k zero
    with Not_found -> ()

  let remove t k =
    Log.debug (fun l -> l "[branches] remove %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () ->
        unsafe_remove t k;
        Lwt.return () )
    >>= fun () -> W.notify t.w k None

  let unsafe_clear t =
    Lwt.async (fun () -> W.clear t.w);
    IO.clear t.block;
    Tbl.clear t.cache;
    Tbl.clear t.index

  let create = Lwt_mutex.create ()

  let watches = W.v ()

  let unsafe_v ~fresh ~readonly file =
    let block = IO.v ~fresh ~version:current_version ~readonly file in
    let cache = Tbl.create 997 in
    let index = Tbl.create 997 in
    let len = IO.offset block in
    let rec aux offset k =
      if offset >= len then k ()
      else
        let len = read_length32 ~off:offset block in
        let buf = Bytes.create (len + V.hash_size) in
        let off = offset ++ 4L in
        let n = IO.read block ~off buf in
        assert (n = Bytes.length buf);
        let buf = Bytes.unsafe_to_string buf in
        let h =
          let h = String.sub buf 0 len in
          match Irmin.Type.of_bin_string K.t h with
          | Ok k -> k
          | Error (`Msg e) -> failwith e
        in
        let n, v = Irmin.Type.decode_bin V.t buf len in
        assert (n = String.length buf);
        if not (Irmin.Type.equal V.t v zero) then Tbl.add cache h v;
        Tbl.add index h offset;
        (aux [@tailcall]) (off ++ Int64.(of_int @@ (len + V.hash_size))) k
    in
    (aux [@tailcall]) 0L @@ fun () ->
    { cache; index; block; w = watches; lock = Lwt_mutex.create () }

  let unsafe_v = with_cache ~clear:unsafe_clear ~v:unsafe_v "store.branches"

  let v ?fresh ?shared ?readonly file =
    Lwt_mutex.with_lock create (fun () ->
        let v = unsafe_v ?fresh ?shared ?readonly file in
        Lwt.return v )

  let unsafe_set t k v =
    try
      let off = Tbl.find t.index k in
      Tbl.replace t.cache k v;
      set_entry t ~off k v
    with Not_found ->
      let offset = IO.offset t.block in
      set_entry t k v;
      Tbl.add t.cache k v;
      Tbl.add t.index k offset

  let set t k v =
    Log.debug (fun l -> l "[branches] set %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () ->
        unsafe_set t k v;
        Lwt.return () )
    >>= fun () -> W.notify t.w k (Some v)

  let unsafe_test_and_set t k ~test ~set =
    let v = try Some (Tbl.find t.cache k) with Not_found -> None in
    if not (Irmin.Type.(equal (option V.t)) v test) then Lwt.return false
    else
      let return () = Lwt.return true in
      match set with
      | None -> unsafe_remove t k |> return
      | Some v -> unsafe_set t k v |> return

  let test_and_set t k ~test ~set =
    Log.debug (fun l -> l "[branches] test-and-set %a" pp_branch k);
    Lwt_mutex.with_lock t.lock (fun () -> unsafe_test_and_set t k ~test ~set)
    >>= function
    | true -> W.notify t.w k set >|= fun () -> true
    | false -> Lwt.return false

  let list t =
    Log.debug (fun l -> l "[branches] list");
    let keys = Tbl.fold (fun k _ acc -> k :: acc) t.cache [] in
    Lwt.return keys

  let watch_key t = W.watch_key t.w

  let watch t = W.watch t.w

  let unwatch t = W.unwatch t.w
end

module Make_ext
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S)
    (Node : Irmin.Private.Node.S
            with type metadata = M.t
             and type hash = H.t
             and type step = P.step)
    (Commit : Irmin.Private.Commit.S with type hash = H.t) =
struct
  module Pack = Pack.File (H)
  module Index = Pack_index.Make (H)

  module X = struct
    module Hash = H

    type 'a value = { magic : char; hash : H.t; v : 'a }

    let value a =
      let open Irmin.Type in
      record "value" (fun hash magic v -> { magic; hash; v })
      |+ field "hash" H.t (fun v -> v.hash)
      |+ field "magic" char (fun v -> v.magic)
      |+ field "v" a (fun v -> v.v)
      |> sealr

    module Contents = struct
      module CA = struct
        module Key = H
        module Val = C

        include Pack.Make (struct
          include Val

          type hash = H.t

          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let magic = 'B'

          let value = value Val.t

          let encode_bin ~dict:_ ~offset:_ v hash =
            Irmin.Type.encode_bin value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, t = Irmin.Type.decode_bin ~headers:false value s off in
            t.v
        end)
      end

      include Irmin.Contents.Store (CA)
    end

    module Inode = struct
      module Conf = struct
        let max_values = 32

        let max_inodes = 32
      end

      module Val = struct
        type entry =
          | Node of { name : P.step; node : H.t }
          | Contents of { metadata : M.t; name : P.step; node : H.t }
          | Inode of { index : int; node : H.t }

        let entry : entry Irmin.Type.t =
          let open Irmin.Type in
          variant "Inode.entry" (fun node contents inode -> function
            | Node n -> node (n.name, n.node)
            | Contents c -> contents (c.metadata, c.name, c.node)
            | Inode i -> inode (i.index, i.node) )
          |~ case1 "Node" (pair P.step_t H.t) (fun (name, node) ->
                 Node { name; node } )
          |~ case1 "Contents" (triple M.t P.step_t H.t)
               (fun (metadata, name, node) -> Contents { metadata; name; node })
          |~ case1 "Inode" (pair int H.t) (fun (index, node) ->
                 Inode { index; node } )
          |> sealv

        (* hash is the hash of the corresponding sub-node *)
        type t = entry list value

        module H = Irmin.Hash.Typed (H) (Node)

        let hash = H.hash

        let magic = 'T'

        let empty = { magic; hash = hash Node.empty; v = [] }

        let v ~hash v = { magic; hash; v }

        let t = value (Irmin.Type.list entry)

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
            type t = P.step

            let compare = Irmin.Type.compare P.step_t
          end)

          let of_list l =
            List.fold_left (fun acc (k, v) -> add k v acc) empty l
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
          abs (Irmin.Type.short_hash P.step_t ~seed k) mod Conf.max_inodes

        let singleton s v = Values (StepMap.singleton s v)

        let rec add_values : type a.
            seed:_ -> _ -> _ -> _ -> _ -> (t -> a) -> a =
         fun ~seed t vs s v k ->
          let values = StepMap.add s v vs in
          if values == vs then k t
          else if StepMap.cardinal values <= Conf.max_values then
            k (Values values)
          else
            (nodes_of_values [@tailcall]) ~seed values @@ fun n -> k (Nodes n)

        and nodes_of_values : type a. seed:_ -> _ -> (t array -> a) -> a =
         fun ~seed entries k ->
          let n = Array.make Conf.max_inodes empty in
          StepMap.iter
            (fun s e -> (update_nodes [@tailcall]) ~seed n s e)
            entries;
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
              (add_nodes [@tailcall]) ~seed:(seed + 1) ~copy x n s v
              @@ fun y -> k i x y

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
          if List.length l < Conf.max_values then values l
          else
            let aux t (s, v) =
              match t with
              | Empty -> singleton s v
              | Values vs ->
                  (add_values [@tailcall]) ~seed:0 t vs s v (fun x -> x)
              | Nodes n ->
                  (add_nodes [@tailcall]) ~seed:0 ~copy:false t n s v (fun x ->
                      x )
            in
            List.fold_left aux empty l

        let fold f t ~hash init =
          let rec inode t k =
            match t with
            | Empty -> k Val.empty
            | Values vs ->
                let values = StepMap.bindings vs in
                assert (List.length values <= Conf.max_values);
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
          fold
            (fun k v f -> add k v >>= f)
            t
            (fun k v -> add k v >|= fun () -> k)

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
                          ( StepMap.add c.name
                              (`Contents (c.node, c.metadata))
                              vs,
                            is ) | Inode i -> (vs, (i.index, i.node) :: is) )
                    (StepMap.empty, []) i
                in
                if is = [] then
                  let t = Values vs in
                  k (Some t)
                else
                  (nodes_of_values [@tailcall]) ~seed vs @@ fun n ->
                  (inodes [@tailcall]) ~seed n is @@ fun n ->
                  k (Some (Nodes n))
          and inodes ~seed n l k =
            match l with
            | [] -> k n
            | (i, h) :: t -> (
                (inode [@tailcall]) ~seed:(seed + 1) h @@ fun v ->
                (inodes [@tailcall]) ~seed n t @@ fun n ->
                match v with
                | None -> k n
                | Some v ->
                    assert (i < Conf.max_inodes);
                    assert (n.(i) = Empty);
                    n.(i) <- v;
                    k n )
          in
          inode ~seed:0 h (function
            | None -> Lwt.return None
            | Some x -> Lwt.return (Some x) )
      end

      module Compress = struct
        type name = Indirect of int | Direct of P.step

        type address = Indirect of int64 | Direct of H.t

        type entry =
          | Contents of name * address * M.t
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
          |~ case1 "contents-ii" (triple int int64 M.t) (fun (n, i, m) ->
                 Contents (Indirect n, Indirect i, m) )
          |~ case1 "node-ii" (pair int int64) (fun (n, i) ->
                 Node (Indirect n, Indirect i) )
          |~ case1 "inode-i" (pair int int64) (fun (n, i) ->
                 Inode (n, Indirect i) )
          |~ case1 "contents-id" (triple int H.t M.t) (fun (n, h, m) ->
                 Contents (Indirect n, Direct h, m) )
          |~ case1 "node-id" (pair int H.t) (fun (n, h) ->
                 Node (Indirect n, Direct h) )
          |~ case1 "inode-d" (pair int H.t) (fun (n, h) -> Inode (n, Direct h))
          |~ case1 "contents-di" (triple P.step_t int64 M.t) (fun (n, i, m) ->
                 Contents (Direct n, Indirect i, m) )
          |~ case1 "node-di" (pair P.step_t int64) (fun (n, i) ->
                 Node (Direct n, Indirect i) )
          |~ case1 "contents-dd" (triple P.step_t H.t M.t) (fun (n, i, m) ->
                 Contents (Direct n, Direct i, m) )
          |~ case1 "node-dd" (pair P.step_t H.t) (fun (n, i) ->
                 Node (Direct n, Direct i) )
          |> sealv

        let t = value (Irmin.Type.list entry)
      end

      include Pack.Make (struct
        include Val

        type hash = H.t

        let hash t = t.hash

        let encode_bin ~dict ~offset (t : t) k =
          assert (Irmin.Type.equal H.t k t.hash);
          let step s : Compress.name =
            let str = Irmin.Type.to_bin_string P.step_t s in
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
          let step : Compress.name -> P.step = function
            | Direct n -> n
            | Indirect s -> (
              match dict s with
              | None -> raise_notrace (Exit (`Msg "dict"))
              | Some s -> (
                match Irmin.Type.of_bin_string P.step_t s with
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

    module Node = struct
      module CA = struct
        module Val = Node
        module Key = H

        type 'a t = 'a Inode.t

        type key = Key.t

        type value = Val.t

        let mem t k = Inode.mem t k

        let find t k =
          Inode.Tree.load
            ~find:(fun k ->
              let v = Inode.unsafe_find t k in
              Lwt.return v )
            k
          >|= function
          | None -> None
          | Some t -> Some (Val.v (Inode.Tree.list t))

        module H_node = Irmin.Hash.Typed (H) (Val)

        let hash_node = H_node.hash

        let add t v =
          let n = Val.list v in
          let hash = hash_node v in
          let v = Inode.Tree.v n in
          Inode.Tree.save ~hash
            ~add:(fun k v ->
              Inode.unsafe_append t k v;
              Lwt.return () )
            v

        let unsafe_add t k v =
          let n = Val.list v in
          let v = Inode.Tree.v n in
          Inode.Tree.save ~hash:k
            ~add:(fun k v ->
              Inode.unsafe_append t k v;
              Lwt.return () )
            v
          >|= fun _ -> ()

        let batch = Inode.batch

        let v = Inode.v
      end

      include Irmin.Private.Node.Store (Contents) (P) (M) (CA)
    end

    module Commit = struct
      module CA = struct
        module Key = H
        module Val = Commit

        include Pack.Make (struct
          include Val
          module H = Irmin.Hash.Typed (H) (Val)

          let hash = H.hash

          let value = value Val.t

          let magic = 'C'

          let encode_bin ~dict:_ ~offset:_ v hash =
            Irmin.Type.encode_bin value { magic; hash; v }

          let decode_bin ~dict:_ ~hash:_ s off =
            let _, v = Irmin.Type.decode_bin ~headers:false value s off in
            v.v
        end)
      end

      include Irmin.Private.Commit.Store (Node) (CA)
    end

    module Branch = struct
      module Key = B
      module Val = H
      include Atomic_write (Key) (Val)
    end

    module Slice = Irmin.Private.Slice.Make (Contents) (Node) (Commit)
    module Sync = Irmin.Private.Sync.None (H) (B)

    module Repo = struct
      type t = {
        config : Irmin.Private.Conf.t;
        contents : [ `Read ] Contents.CA.t;
        node : [ `Read ] Node.CA.t;
        commit : [ `Read ] Commit.CA.t;
        branch : Branch.t;
        index : Index.t
      }

      let contents_t t : 'a Contents.t = t.contents

      let node_t t : 'a Node.t = (contents_t t, t.node)

      let commit_t t : 'a Commit.t = (node_t t, t.commit)

      let branch_t t = t.branch

      let batch t f =
        Commit.CA.batch t.commit (fun commit ->
            Node.CA.batch t.node (fun node ->
                Contents.CA.batch t.contents (fun contents ->
                    let contents : 'a Contents.t = contents in
                    let node : 'a Node.t = (contents, node) in
                    let commit : 'a Commit.t = (node, commit) in
                    f contents node commit ) ) )

      let v config =
        let root = root config in
        let fresh = fresh config in
        let lru_size = lru_size config in
        let readonly = readonly config in
        let index =
          Index.v ~fresh ~read_only:readonly ~log_size:10_000_000
            ~fan_out_size:256 root
        in
        Contents.CA.v ~fresh ~readonly ~lru_size root >>= fun contents ->
        Node.CA.v ~fresh ~readonly ~lru_size root >>= fun node ->
        Commit.CA.v ~fresh ~readonly ~lru_size root >>= fun commit ->
        Branch.v ~fresh ~readonly root >|= fun branch ->
        { contents; node; commit; branch; config; index }
    end
  end

  include Irmin.Of_private (X)
end

module Hash = Irmin.Hash.SHA1
module Path = Irmin.Path.String_list
module Metadata = Irmin.Metadata.None

module Make
    (M : Irmin.Metadata.S)
    (C : Irmin.Contents.S)
    (P : Irmin.Path.S)
    (B : Irmin.Branch.S)
    (H : Irmin.Hash.S) =
struct
  module XNode = Irmin.Private.Node.Make (H) (P) (M)
  module XCommit = Irmin.Private.Commit.Make (H)
  include Make_ext (M) (C) (P) (B) (H) (XNode) (XCommit)
end

module KV (C : Irmin.Contents.S) =
  Make (Metadata) (C) (Path) (Irmin.Branch.String) (Hash)
