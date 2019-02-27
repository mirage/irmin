(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix

let src = Logs.Src.create "irmin.chunk" ~doc:"Irmin chunks"
module Log = (val Logs.src_log src : Logs.LOG)

module Conf = struct

  let min_size =
    Irmin.Private.Conf.key ~doc:"Minimal chunk size" "min-size"
      Irmin.Private.Conf.int 4000

  let chunk_size =
    Irmin.Private.Conf.key ~doc:"Size of chunk" "size"
      Irmin.Private.Conf.int 4096

  let chunking =
    let pp ppf = function
      | `Max -> Fmt.pf ppf "max"
      | `Best_fit -> Fmt.pf ppf "best-fit"
    in
    let of_string = function
      | "max" -> Ok `Max
      | "best-fit" -> Ok `Best_fit
      | e ->
        Error (`Msg (e ^ " is not a valid chunking algorithm. Use 'max' or \
                          'best-fit'"))
    in
    Irmin.Private.Conf.key ~doc:"Chunking algorithm" "chunking"
      (of_string, pp) `Best_fit

end

let chunk_size = Conf.chunk_size

let err_too_small ~min size =
  Printf.ksprintf invalid_arg
    "Chunks of %d bytes are too small. Size should at least be %d bytes."
    size min

let config ?(config=Irmin.Private.Conf.empty)
    ?size ?min_size ?(chunking=`Best_fit) ()
  =
  let module C = Irmin.Private.Conf in
  let min_size = match min_size with
    | None   -> C.default Conf.min_size
    | Some v -> v
  in
  let size = match size with
    | None   -> C.default Conf.chunk_size
    | Some v -> if v < min_size then err_too_small ~min:min_size v else v
  in
  let add x y c = C.add c x y in
  config
  |> add Conf.min_size min_size
  |> add Conf.chunk_size size
  |> add Conf.chunking chunking

module Chunk (K: Irmin.Hash.S) = struct

  type v =
    | Data  of string
    | Index of K.t list

  let v =
    let open Irmin.Type in
    variant "chunk" (fun d i -> function
        | Data  data  -> d data
        | Index index -> i index)
    |~ case1 "Data" string (fun d -> Data d)
    |~ case1 "Index" (list ~len:`Int16 K.t) (fun i -> Index i)
    |> sealv

  type t = { len: int; v: v }

  let size_of_v t = match Irmin.Type.size_of v t with
    | `Size i   -> i
    | `Buffer b -> String.length b

  let size_of_data_header = size_of_v (Data "")
  let size_of_index_header = size_of_v (Index [])

  let of_string b =
    let len = String.length b in
    let n, v = Irmin.Type.decode_bin v b 0 in
    if len=n then { len; v }
    else Fmt.invalid_arg "invalid length: got %d, expecting %d" n len

  let to_string t =
    let buf = Bytes.make t.len '\000' in
    let (_:int) = Irmin.Type.encode_bin v buf 0 t.v in
    Bytes.unsafe_to_string buf

  let t = Irmin.Type.(like_map string) of_string to_string

end

module Content_addressable
    (S:Irmin.APPEND_ONLY_STORE_MAKER)
    (K:Irmin.Hash.S) (V: Irmin.Type.S) =
struct

  module Chunk = Chunk(K)

  module AO = S(K)(Chunk)
  module CA = Irmin.Content_addressable(S)(K)(Chunk)
  type key = CA.key
  type value = V.t

  type 'a t = {
    chunking    : [`Max | `Best_fit];
    db          : 'a CA.t;          (* An handler to the underlying database. *)
    chunk_size  : int;                                 (* the size of chunks. *)
    max_children: int;     (* the maximum number of children a node can have. *)
    max_data    : int; (* the maximum length (in bytes) of data stored in one
                          chunk. *)
  }

  let index t i =
    let v = Chunk.Index i in
    match t.chunking with
    | `Max      -> { Chunk.v; len = t.chunk_size }
    | `Best_fit -> { Chunk.v; len = Chunk.size_of_v v }

  let data t v =
    let v = Chunk.Data v in
    match t.chunking with
    | `Max      -> { Chunk.v; len = t.chunk_size }
    | `Best_fit -> { Chunk.v; len = Chunk.size_of_v v }

  module Tree = struct

    (* return all the tree leaves *)
    let find_leaves t root =
      let rec aux acc { Chunk.v; _ } = match v with
        | Chunk.Data  d -> Lwt.return (d :: acc)
        | Chunk.Index i ->
          Lwt_list.fold_left_s (fun acc key ->
              CA.find t.db key >>= function
              | None   -> Lwt.return acc
              | Some v -> aux acc v
            ) acc i
      in
      aux [] root >|= List.rev

    (* partition a list into a list of elements of at most size [n] *)
    let list_partition n l =
      let rec aux done_ i acc = function
        | []   -> List.rev (List.rev acc :: done_)
        | h::t ->
          if i >= n then
            aux (List.rev acc :: done_) 1 [h] t
          else
            aux done_ (i+1) (h :: acc) t
      in
      aux [] 0 [] l

    let add t ~key l =
      let rec aux = function
        | []  -> invalid_arg "Irmin_chunk.Tree.add"
        | [k] -> Lwt.return k
        | l   ->
          let n =
            if List.length l >= t.max_children
            then t.max_children
            else List.length l
          in
          match list_partition n l with
          | [i] -> AO.add t.db key (index t i) >|= fun () -> key
          | l   -> Lwt_list.map_p (fun i -> CA.add t.db (index t i) ) l >>= aux
      in
      aux l

  end

  let v config =
    let module C = Irmin.Private.Conf in
    let chunk_size = C.get config Conf.chunk_size in
    let max_data = chunk_size - Chunk.size_of_data_header in
    let max_children = (chunk_size - Chunk.size_of_index_header) / K.digest_size in
    let chunking = C.get config Conf.chunking in
    if max_children <= 1 then (
      let min = Chunk.size_of_index_header + K.digest_size * 2 in
      err_too_small ~min chunk_size;
    );
    Log.debug
      (fun l ->
         l "config: chunk-size=%d digest-size=%d max-data=%d max-children=%d"
           chunk_size K.digest_size max_data max_children);
    CA.v config >|= fun db ->
    { chunking; db; chunk_size; max_children; max_data }

  let batch t f = CA.batch t.db (fun db -> f { t with db })

  let find_leaves t key =
    AO.find t.db key >>= function
    | None    -> Lwt.return []
    | Some x  -> Tree.find_leaves t x

  let check_hash k v =
    let k'= K.digest v in
    if Irmin.Type.equal K.t k k' then Lwt.return ()
    else
      let pp_key = Irmin.Type.pp K.t in
      Fmt.kstrf Lwt.fail_invalid_arg
        "corrupted value: got %a, expecting %a" pp_key k' pp_key k

  let find t key =
    find_leaves t key >>= fun bufs ->
    let buf = String.concat "" bufs in
    check_hash key buf >|= fun () ->
    match Irmin.Type.of_bin_string V.t buf with
    |Ok va   -> Some va
    |Error _ -> None

  let list_range ~init ~stop ~step =
    let rec aux acc n =
      if n >= stop then List.rev acc
      else aux (n :: acc) (n + step)
    in
    aux [] init

  let pp_key = Irmin.Type.pp K.t

  let add t v =
    let buf = Irmin.Type.to_bin_string V.t v in
    let key = K.digest buf in
    let len = String.length buf in
    if len <= t.max_data then (
      AO.add t.db key (data t buf) >|= fun () ->
      Log.debug (fun l -> l "add -> %a (no split)" pp_key key);
      key
    ) else (
      let offs = list_range ~init:0 ~stop:len ~step:t.max_data in
      let aux off =
        let len = min t.max_data (String.length buf - off) in
        let payload = String.sub buf off len in
        CA.add t.db (data t payload)
      in
      Lwt_list.map_s aux offs >>= Tree.add ~key t >|= fun k ->
      Log.debug (fun l -> l "add -> %a (split)" pp_key k);
      key
    )

  let mem t key = CA.mem t.db key

end
