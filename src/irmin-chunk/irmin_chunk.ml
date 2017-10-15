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
open Irmin

module Conf = struct

  let min_size =
    Irmin.Private.Conf.key ~doc:"Minimal chunk size" "min-size"
      Irmin.Private.Conf.int 4000

  let chunk_size =
    Irmin.Private.Conf.key ~doc:"Size of chunk" "size"
      Irmin.Private.Conf.int 4666

end

let chunk_size = Conf.chunk_size

let err_too_small ~min size =
  Printf.ksprintf invalid_arg
    "Chunks of %d bytes are too small. Size should at least be %d bytes."
    size min

let config ?(config=Irmin.Private.Conf.empty) ?size ?min_size () =
  let module C = Irmin.Private.Conf in
  let min_size = match min_size with
    | None   -> C.default Conf.min_size
    | Some v -> v
  in
  let size = match size with
    | None   -> C.default Conf.chunk_size
    | Some v -> if v < min_size then err_too_small ~min:min_size v else v
  in
  C.add (C.add config Conf.min_size min_size) Conf.chunk_size size

module Chunk (K: Irmin.Hash.S) = struct

  type t = Data | Indirect

  let l_type = 1
  let get_type x = Cstruct.get_uint8 x 0
  let set_type x v = Cstruct.set_uint8 x 0 v

  let l_len = 2
  let get_length x = Cstruct.LE.get_uint16 x l_type
  let set_length x v = Cstruct.LE.set_uint16 x l_type v

  let head_length = l_type + l_len
  let offset_payload = head_length

  let get_sub_key x pos =
    let offset = offset_payload + (pos * K.digest_size) in
    Cstruct.sub x offset K.digest_size

  let set_data src srcoff dst dstoff len =
    Cstruct.blit src srcoff dst (offset_payload + dstoff) len

  let extract_data src srcoff dst dstoff len =
    Cstruct.blit src (offset_payload + srcoff) dst dstoff len

  let type_of_int = function
    | Indirect -> 0
    | Data     -> 1

  let int_of_type = function
    | 0 -> Indirect
    | 1 -> Data
    | _ -> failwith "Unknow type"

  let type_of_chunk x = int_of_type (get_type x)

  let create typ chunk_size len  =
    let c = Cstruct.create chunk_size in
    Cstruct.memset c 0x00;
    set_type c (type_of_int typ);
    set_length c len;
    c

end

module AO (S:AO_MAKER) (K:Irmin.Hash.S) (V: Irmin.Contents.Conv)
: sig
    include Irmin.AO
    val v : Irmin.config -> t Lwt.t
  end
    with type key = S(K)(V).key and type value = S(K)(V).value
= struct

  module AO = S(K)(V)
  type key = AO.key
  type value = AO.value

  module Chunk = Chunk(K)

  type t = {
    db          : AO.t;             (* An handler to the underlying database. *)
    chunk_size  : int;                                 (* the size of chunks. *)
    max_children: int;     (* the maximum number of children a node can have. *)
    max_length  : int; (* the maximum lentgh (in bytes) of data stored in one
                          chunk. *)
  }

  let v_of_raw raw =
    match Irmin.Type.decode_cstruct V.t raw with
    |Ok va -> va
    |Error _ -> invalid_arg "v_of_raw"

  let raw_of_v v =
    Irmin.Type.encode_cstruct V.t v

  module Tree = struct

    let get_child t node pos =
      let key = K.of_raw (Chunk.get_sub_key node pos) in
      AO.find t.db key >>= function
      |Some v -> Lwt.return_some @@ raw_of_v v
      |None -> Lwt.return_none

    let walk_to_list t root =
      let rec aux_walk t node =
        match Chunk.type_of_chunk node with
        | Chunk.Data ->
          let dlen = Chunk.get_length node in
          let leaf = Cstruct.create dlen in
          Chunk.extract_data node 0 leaf 0 dlen;
          Lwt.return_some [leaf]
        | Chunk.Indirect ->
          let nbr_child = Chunk.get_length node in
          let rec loop i accu =
            if i < nbr_child then
              get_child t node i >>= function
                |None -> Lwt.return_none
                |Some x ->
              aux_walk t x       >>= function
                |None -> Lwt.return_none
                |Some y ->
              loop (i+1) (List.append accu y)
            else
              Lwt.return_some accu
          in
          loop 0 []
      in
      aux_walk t root

    let rec bottom_up t l r =
      match l with
      | [] -> Lwt.return r
      | l ->
        let n =
          if List.length l >= t.max_children then t.max_children
          else List.length l
        in
        let indir = Chunk.(create Indirect) t.chunk_size n in
        let rec loop i l =
          if i >= n then l
          else
            let x = List.hd l in
            let rest = List.tl l in
            let offset_hash = i * K.digest_size in
            Chunk.set_data (K.to_raw x) 0 indir offset_hash K.digest_size;
            loop (i+1) rest
        in
        let calc = loop 0 l in
        AO.add t.db @@ v_of_raw indir >>= fun x ->
        bottom_up t calc (x::r)

    let rec create t = function
      | [] -> failwith "Irmin_chunk.Tree.bottom_up"
      | [e] -> Lwt.return e
      | l   -> bottom_up t l [] >>= create t

  end

  let v config =
    let module C = Irmin.Private.Conf in
    let chunk_size = C.get config Conf.chunk_size in
    let max_length = chunk_size - Chunk.head_length - 1 in
    let max_children = max_length / K.digest_size in
    if max_children <= 1 then (
      let min = Chunk.head_length + 1 + K.digest_size * 2 in
      err_too_small ~min chunk_size;
    );
    AO.v config >|=
    fun db -> { db; chunk_size; max_children; max_length }

  let find t key =
    AO.find t.db key >>= function
    | None   -> Lwt.return_none
    | Some x ->
      let x = raw_of_v x in
      match Chunk.type_of_chunk x with
      | Chunk.Data ->
        let dlen = Chunk.get_length x in
        let result = Cstruct.create dlen in
        Chunk.extract_data x 0 result 0 dlen;
        Lwt.return_some @@ v_of_raw result
      | Chunk.Indirect ->
        Tree.walk_to_list t x >>= function
          |None -> Lwt.return_none
          |Some y ->
        let result = v_of_raw @@ Cstruct.concat y in
        Lwt.return_some result

  type params = {
    used: int;                (* the total number of blocks which are needed. *)
    last: int;               (* the number of useful bytes in the last block. *)
  }

  let split t ~data ~nodes i v =
    let is_last = (i = nodes.used - 1) in
    let chunks = if is_last then nodes.last else t.max_children in
    let buf = Chunk.(create Indirect) t.chunk_size chunks in
    let rec loop j =
      let len, max_loop =
        if not is_last then t.max_length, t.max_children
        else
          if j = nodes.last - 1 then data.last, nodes.last
          else t.max_length, nodes.last
      in
      if j >= max_loop then Lwt.return_unit
      else (
        let chunk = Chunk.(create Data) t.chunk_size len in
        let offset_value = (j + i * t.max_children) * t.max_length in
        Chunk.set_data v offset_value chunk 0 len;
        let add_chunk () =
          AO.add t.db @@ v_of_raw chunk >>= fun x ->
          let offset_hash = j * K.digest_size in
          Chunk.set_data (K.to_raw x) 0 buf offset_hash K.digest_size;
          Lwt.return_unit
        in
        Lwt.join [add_chunk (); loop (j + 1)]
      ) in
    loop 0 >>= fun () ->
    AO.add t.db @@ v_of_raw buf >>= fun x ->
    Lwt.return x

  let add t v =
    let v = raw_of_v v in
    let value_length = Cstruct.len v in
    let rest_value_length = value_length mod t.max_length in
    if value_length <= t.max_length then (
      let chunk = Chunk.(create Data) t.chunk_size value_length  in
      Chunk.set_data v 0 chunk 0 value_length;
      AO.add t.db @@ v_of_raw chunk
    ) else (
      let data = (* the number of data blocks *)
        let x = value_length / t.max_length in
        if rest_value_length = 0 then
          { used = x; last = t.max_length }
        else
          { used = x + 1; last = rest_value_length }
      in
      let nodes = (* the number of tree nodes *)
        let x = data.used / t.max_children in
        let y = data.used mod t.max_children in
        if y = 0 then
          { used = x; last = t.max_children }
        else
          { used = x + 1; last = y }
      in
      let loop () =
        let rec aux acc = function
          | i when i = nodes.used -> Lwt.return (List.rev acc)
          | i when i < nodes.used ->
            split t ~data ~nodes i v >>= fun x ->
            aux (x::acc) (i+1)
          | _ -> assert false
        in
        aux [] 0
      in
      loop () >>= Tree.create t
    )

  let mem t key = AO.mem t.db key

end

module AO_stable (L: LINK_MAKER) (S: AO_MAKER) (K: Hash.S) (V: Irmin.Contents.Conv) = struct

  module AO = AO(S)(K)(V)
  module Link = L(K)

  type key = K.t
  type value = V.t
  type t = { ao: AO.t; link: Link.t }

  let v config =
    AO.v config >>= fun ao ->
    Link.v config >|= fun link ->
      { ao; link; }

  let find t k =
    Link.find t.link k >>= function
    | None   -> Lwt.return_none
    | Some k -> AO.find t.ao k

  let mem t k =
    Link.find t.link k >>= function
    | None   -> Lwt.return_false
    | Some k -> AO.mem t.ao k

  let add t v =
    AO.add t.ao v >>= fun k' ->
    let k = K.digest V.t v in
    Link.add t.link k k' >>= fun () ->
    Lwt.return k

end
