(*
 * Copyright (c) 2013-2022 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2015 Mounir Nasr Allah <mounir@nasrallah.co>
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

let src = Logs.Src.create "irmin.chunk" ~doc:"Irmin chunks"

module Log = (val Logs.src_log src : Logs.LOG)

module Conf = struct
  include Irmin.Backend.Conf

  let spec = Spec.v "chunk"

  module Key = struct
    let min_size =
      key ~spec ~doc:"Minimal chunk size" "min-size" Irmin.Type.int 4000

    let chunk_size = key ~spec ~doc:"Size of chunk" "size" Irmin.Type.int 4096

    let chunk_type_t =
      Irmin.Type.(enum "chunk_type" [ ("max", `Max); ("best-fit", `Best_fit) ])

    let chunking =
      key ~spec ~doc:"Chunking algorithm" "chunking" chunk_type_t `Best_fit
  end
end

let err_too_small ~min size =
  Printf.ksprintf invalid_arg
    "Chunks of %d bytes are too small. Size should at least be %d bytes." size
    min

let config ?size ?min_size ?(chunking = `Best_fit) config =
  let min_size =
    match min_size with None -> Conf.default Conf.Key.min_size | Some v -> v
  in
  let size =
    match size with
    | None -> Conf.default Conf.Key.chunk_size
    | Some v -> if v < min_size then err_too_small ~min:min_size v else v
  in
  let add x y c = Conf.add c x y in
  let cfg =
    Conf.empty Conf.spec
    |> add Conf.Key.min_size min_size
    |> add Conf.Key.chunk_size size
    |> add Conf.Key.chunking chunking
  in
  Conf.(verify (union cfg config))

module Chunk (H : Irmin.Hash.S) = struct
  type v = Data of string | Index of H.t list

  let v_t =
    let open Irmin.Type in
    variant "chunk" (fun d i -> function
      | Data data -> d data | Index index -> i index)
    |~ case1 "Data" string (fun d -> Data d)
    |~ case1 "Index" (list ~len:`Int16 H.t) (fun i -> Index i)
    |> sealv

  type value = v
  [@@deriving irmin ~size_of ~to_bin_string ~decode_bin ~encode_bin]

  type t = { len : int; v : v }

  let size_of_v t =
    match size_of_value t with
    | Some n -> n
    | None -> String.length (value_to_bin_string t)

  let size_of_data_header = size_of_v (Data "")
  let size_of_index_header = size_of_v (Index [])

  let of_string b =
    let len = String.length b in
    let pos_ref = ref 0 in
    let v = decode_bin_value b pos_ref in
    if !pos_ref = len then { len; v }
    else Fmt.invalid_arg "invalid length: got %d, expecting %d" !pos_ref len

  let to_string t =
    let buf = Bytes.make t.len '\000' in
    let b = Buffer.create t.len in
    encode_bin_value t.v (Buffer.add_string b);
    let s = Buffer.contents b in
    Bytes.blit_string s 0 buf 0 (String.length s);
    Bytes.unsafe_to_string buf

  let t = Irmin.Type.(map string) of_string to_string
end

module Content_addressable
    (Make_append_only : Irmin.Append_only.Maker)
    (H : Irmin.Hash.S)
    (V : Irmin.Type.S) =
struct
  module Chunk = Chunk (H)
  module AO = Make_append_only (H) (Chunk)
  module CA = Irmin.Content_addressable.Make (Make_append_only) (H) (Chunk)

  type key = H.t [@@deriving irmin ~pp ~equal]
  type value = V.t [@@deriving irmin ~of_bin_string ~to_bin_string ~pre_hash]

  type 'a t = {
    chunking : [ `Max | `Best_fit ];
    db : 'a CA.t;
    (* An handler to the underlying database. *)
    chunk_size : int;
    (* the size of chunks. *)
    max_children : int;
    (* the maximum number of children a node can have. *)
    max_data : int;
        (* the maximum length (in bytes) of data stored in one
                          chunk. *)
  }

  let index t i =
    let v = Chunk.Index i in
    match t.chunking with
    | `Max -> { Chunk.v; len = t.chunk_size }
    | `Best_fit -> { Chunk.v; len = Chunk.size_of_v v }

  let data t s =
    let v = Chunk.Data s in
    match t.chunking with
    | `Max -> { Chunk.v; len = t.chunk_size }
    | `Best_fit -> { Chunk.v; len = Chunk.size_of_v v }

  module Tree = struct
    (* return all the tree leaves *)
    let find_leaves t root =
      let rec aux acc { Chunk.v; _ } =
        match v with
        | Chunk.Data d -> d :: acc
        | Chunk.Index i ->
            List.fold_left
              (fun acc key ->
                match CA.find t.db key with None -> acc | Some v -> aux acc v)
              acc i
      in
      aux [] root |> List.rev

    (* partition a list into a list of elements of at most size [n] *)
    let list_partition n l =
      let rec aux done_ i acc = function
        | [] -> List.rev (List.rev acc :: done_)
        | h :: t ->
            if i >= n then aux (List.rev acc :: done_) 1 [ h ] t
            else aux done_ (i + 1) (h :: acc) t
      in
      aux [] 0 [] l

    let add t ~key l =
      let rec aux = function
        | [] -> invalid_arg "Irmin_chunk.Tree.add"
        | [ k ] -> k
        | l -> (
            let n =
              if List.length l >= t.max_children then t.max_children
              else List.length l
            in
            match list_partition n l with
            | [ i ] ->
                AO.add t.db key (index t i);
                key
            | l -> Fiber.List.map (fun i -> CA.add t.db (index t i)) l |> aux)
      in
      aux l
  end

  let v ~sw config =
    let chunk_size = Conf.get config Conf.Key.chunk_size in
    let max_data = chunk_size - Chunk.size_of_data_header in
    let max_children =
      (chunk_size - Chunk.size_of_index_header) / H.hash_size
    in
    let chunking = Conf.get config Conf.Key.chunking in
    (if max_children <= 1 then
       let min = Chunk.size_of_index_header + (H.hash_size * 2) in
       err_too_small ~min chunk_size);
    [%log.debug
      "config: chunk-size=%d digest-size=%d max-data=%d max-children=%d"
        chunk_size H.hash_size max_data max_children];
    let db = CA.v ~sw config in
    { chunking; db; chunk_size; max_children; max_data }

  let close _ = ()
  let batch t f = CA.batch t.db (fun db -> f { t with db })

  let find_leaves t key =
    match AO.find t.db key with
    | None -> None (* shallow objects *)
    | Some x -> Tree.find_leaves t x |> Option.some

  let check_hash k v =
    let k' = H.hash (pre_hash_value v) in
    if equal_key k k' then ()
    else
      Fmt.kstr failwith "corrupted value: got %a, expecting %a" pp_key k' pp_key
        k

  let find t key =
    match find_leaves t key with
    | None -> None
    | Some bufs -> (
        let buf = String.concat "" bufs in
        match value_of_bin_string buf with
        | Ok va ->
            check_hash key va;
            Some va
        | Error _ -> None)

  let list_range ~init ~stop ~step =
    let rec aux acc n =
      if n >= stop then List.rev acc else aux (n :: acc) (n + step)
    in
    aux [] init

  let unsafe_add_buffer t key buf =
    let len = String.length buf in
    if len <= t.max_data then (
      AO.add t.db key (data t buf);
      [%log.debug "add -> %a (no split)" pp_key key])
    else
      let offs = list_range ~init:0 ~stop:len ~step:t.max_data in
      let aux off =
        let len = min t.max_data (String.length buf - off) in
        let payload = String.sub buf off len in
        CA.add t.db (data t payload)
      in
      let k = List.map aux offs |> Tree.add ~key t in
      [%log.debug "add -> %a (split)" pp_key k]

  let add t v =
    let buf = value_to_bin_string v in
    let key = H.hash (pre_hash_value v) in
    let () = unsafe_add_buffer t key buf in
    key

  let unsafe_add t key v =
    let buf = value_to_bin_string v in
    unsafe_add_buffer t key buf

  let mem t key = CA.mem t.db key
end
