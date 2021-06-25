(*
 * Copyright (c) 2013-2021 Thomas Gazagnaire <thomas@gazagnaire.org>
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
  include Irmin.Private.Conf.Make ()

  let min_size = key ~doc:"Minimal chunk size" "min-size" Irmin.Type.int 4000
  let chunk_size = key ~doc:"Size of chunk" "size" Irmin.Type.int 4096

  let chunk_type_t =
    Irmin.Type.(enum "chunk_type" [ ("max", `Max); ("best-fit", `Best_fit) ])

  let chunking = key ~doc:"Chunking algorithm" "chunking" chunk_type_t `Best_fit
end

let chunk_size = Conf.chunk_size

let err_too_small ~min size =
  Printf.ksprintf invalid_arg
    "Chunks of %d bytes are too small. Size should at least be %d bytes." size
    min

let config ?(config = Conf.empty) ?size ?min_size ?(chunking = `Best_fit) () =
  let min_size =
    match min_size with None -> Conf.default Conf.min_size | Some v -> v
  in
  let size =
    match size with
    | None -> Conf.default Conf.chunk_size
    | Some v -> if v < min_size then err_too_small ~min:min_size v else v
  in
  let add x y c = Conf.add c x y in
  config
  |> add Conf.min_size min_size
  |> add Conf.chunk_size size
  |> add Conf.chunking chunking

module Chunk (K : Irmin.Hash.S) = struct
  type v = Data of string | Index of K.t list

  let v_t =
    let open Irmin.Type in
    variant "chunk" (fun d i -> function
      | Data data -> d data | Index index -> i index)
    |~ case1 "Data" string (fun d -> Data d)
    |~ case1 "Index" (list ~len:`Int16 K.t) (fun i -> Index i)
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
    let n, v = decode_bin_value b 0 in
    if len = n then { len; v }
    else Fmt.invalid_arg "invalid length: got %d, expecting %d" n len

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
    (K : Irmin.Hash.S)
    (V : Irmin.Type.S) =
struct
  module Chunk = Chunk (K)
  module AO = Make_append_only (K) (Chunk)
  module CA = Irmin.Content_addressable.Make (Make_append_only) (K) (Chunk)

  type key = K.t [@@deriving irmin ~pp ~equal]
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
        | Chunk.Data d -> Lwt.return (d :: acc)
        | Chunk.Index i ->
            Lwt_list.fold_left_s
              (fun acc key ->
                CA.find t.db key >>= function
                | None -> Lwt.return acc
                | Some v -> aux acc v)
              acc i
      in
      aux [] root >|= List.rev

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
        | [ k ] -> Lwt.return k
        | l -> (
            let n =
              if List.length l >= t.max_children then t.max_children
              else List.length l
            in
            match list_partition n l with
            | [ i ] -> AO.add t.db key (index t i) >|= fun () -> key
            | l -> Lwt_list.map_p (fun i -> CA.add t.db (index t i)) l >>= aux)
      in
      aux l
  end

  let v config =
    let chunk_size = Conf.get config Conf.chunk_size in
    let max_data = chunk_size - Chunk.size_of_data_header in
    let max_children =
      (chunk_size - Chunk.size_of_index_header) / K.hash_size
    in
    let chunking = Conf.get config Conf.chunking in
    (if max_children <= 1 then
     let min = Chunk.size_of_index_header + (K.hash_size * 2) in
     err_too_small ~min chunk_size);
    Log.debug (fun l ->
        l "config: chunk-size=%d digest-size=%d max-data=%d max-children=%d"
          chunk_size K.hash_size max_data max_children);
    let+ db = CA.v (config :> Irmin.config) in
    { chunking; db; chunk_size; max_children; max_data }

  let close _ = Lwt.return_unit
  let clear t = CA.clear t.db
  let batch t f = CA.batch t.db (fun db -> f { t with db })

  let find_leaves t key =
    AO.find t.db key >>= function
    | None -> Lwt.return_none (* shallow objects *)
    | Some x -> Tree.find_leaves t x >|= Option.some

  let check_hash k v =
    let k' = K.hash (pre_hash_value v) in
    if equal_key k k' then Lwt.return_unit
    else
      Fmt.kstrf Lwt.fail_invalid_arg "corrupted value: got %a, expecting %a"
        pp_key k' pp_key k

  let find t key =
    find_leaves t key >>= function
    | None -> Lwt.return_none
    | Some bufs -> (
        let buf = String.concat "" bufs in
        match value_of_bin_string buf with
        | Ok va -> check_hash key va >|= fun () -> Some va
        | Error _ -> Lwt.return_none)

  let list_range ~init ~stop ~step =
    let rec aux acc n =
      if n >= stop then List.rev acc else aux (n :: acc) (n + step)
    in
    aux [] init

  let unsafe_add_buffer t key buf =
    let len = String.length buf in
    if len <= t.max_data then
      AO.add t.db key (data t buf) >|= fun () ->
      Log.debug (fun l -> l "add -> %a (no split)" pp_key key)
    else
      let offs = list_range ~init:0 ~stop:len ~step:t.max_data in
      let aux off =
        let len = min t.max_data (String.length buf - off) in
        let payload = String.sub buf off len in
        CA.add t.db (data t payload)
      in
      let+ k = Lwt_list.map_s aux offs >>= Tree.add ~key t in
      Log.debug (fun l -> l "add -> %a (split)" pp_key k)

  let add t v =
    let buf = value_to_bin_string v in
    let key = K.hash (pre_hash_value v) in
    let+ () = unsafe_add_buffer t key buf in
    key

  let unsafe_add t key v =
    let buf = value_to_bin_string v in
    unsafe_add_buffer t key buf

  let mem t key = CA.mem t.db key
end
